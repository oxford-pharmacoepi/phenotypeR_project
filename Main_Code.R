rm(list=ls())
##### Package installation #####
 renv::snapshot()
{
## Install Needed Packages
# install.packages("CDMConnector")
# install.packages("DBI")
# install.packages("dbplyr")
# install.packages("dplyr")
# install.packages("RPostgres")
# install.packages("usethis")
# install.packages("CodelistGenerator")
# install.packages("devtools")
# devtools::install_github("OHDSI/CirceR")
# devtools::install_github("ohdsi/Capr")
# install.packages("PatientProfiles")
# install.packages("DrugUtilisation")
# install.packages("IncidencePrevalence")
# install.packages("tictoc")
# pending install: SqlRender
# install.packages("remotes")
# remotes::install_github("oxford-pharmacoepi/CohortConstructor")
# remotes::install_github("darwin-eu-dev/IncidencePrevalence@omopgenerics")  
}

# renv::activate()
# renv::hydrate()

##### Open libraries #####
library(CDMConnector)
library(DBI)
library(dbplyr)
library(dplyr)
library(CodelistGenerator)
library(PatientProfiles)
library(here)
library(DrugUtilisation)
library(IncidencePrevalence)
library(tictoc)
library(CohortConstructor)

##### Log start ######

tic.clearlog()
tic.clear()
tic(msg = "phenotypeR total time run: ")

##### Options and set-up:  directories and settings ######
options(error = quote(dump.frames("testdump", TRUE, TRUE)))

tic(msg = "Settings and loading of Phoebe")


cohort_json_dir <- here("Cohorts")
cohorts_name <- "hpv_"
prefix <- "apu"
 cdm_schema <- "public"
# cdm_schema <- "public_100k"
results_schema <- "results"

# Input 
input <- list(
  runGenerateCohort = F,              #### Generate cohort or use preloaded cohorts
  runCalculateOverlap = F,            #### Calculate Overlap
  runCountCodes = F,                  #### run orphan codes and count codes
  runIndexEvents = F,                 #### run index events
  runProfiling = F,                   #### run age and time in database characterisation
  runMatchedSampleLSC = F,            #### run matched LSC
  runIncidence = T,                   #### run Incidence
  runPrevalence = T,                  #### run Prevalence
  sampleIncidencePrevalence = 100000, #### Sample for Incidence Prevalence (NULL if all cdm)
  cdmName = "CPRDgold"
)

# Database details
#server_dbi <- Sys.getenv("DB_SERVER_DBI_Pharmetrics") 
server_dbi <- Sys.getenv("DB_SERVER_DBI_CPRDgold") 
user <- Sys.getenv("DB_USER") 
port <- Sys.getenv("DB_PORT") 
host <- Sys.getenv("DB_HOST")




# To export output 
result_names <- c("cohort_definitions", "cohort_count", "code_counts", "cohort_overlap", 
                  "age_distribution", "time_distribution", "prevalence", "incidence", 
                  "index_events", "lsc_sample", "lsc_matched", "lsc_difference", "log")
output <- data <- vector("list", length(result_names)) |> setNames(result_names)



if (input$runCountCodes){
  concept_recommended <- read.csv(here("Phoebe/concept_recommended.csv"))
}


toc(log = TRUE)



##### Connect to database using CDM COnnector ########
tic(msg = "Connect to database")


db <- dbConnect(RPostgres::Postgres(), 
                dbname = server_dbi, 
                port = port, 
                host = host, 
                user = user, 
                password = Sys.getenv("DB_PASSWORD") ) 

if (input$runGenerateCohort) {
cdm <- cdm_from_con(con = db,
                         cdm_schema = c(schema = cdm_schema),
                         write_schema = c(schema= results_schema, prefix = prefix),
                    achilles_schema = results_schema
                    )
} else   {
  cdm <- cdm_from_con(con = db,
                      cdm_schema = c(schema = cdm_schema),
                      write_schema = c(schema= results_schema, prefix = prefix),
                      achilles_schema = results_schema,
                      cohort_tables = cohorts_name  # to load cohorts already there
  )
 
}

toc(log = TRUE)




####### Step 1: Get cohorts and generate them #######
# now from json, but we can do with CapR other sources 

tic(msg = "Generate Cohort Set")
cohort_set <- read_cohort_set(cohort_json_dir)
if (input$runGenerateCohort) {
  cdm <-   generateCohortSet(cdm, 
                             cohort_set,
                             name = cohorts_name,
                             computeAttrition = TRUE,
                             overwrite = TRUE)
}

toc(log = TRUE)

####### Step 1.2:  Cohort Counts      #########

tic(msg = "Cohort counts, attrition")

# cohort_count <- cohort_count(cdm[[cohorts_name]])
# cohort_attrition <- attrition(cdm[[cohorts_name]])
# cohort_set_cdm <- cohort_set(cdm[[cohorts_name]])
output$cohort_count <- cohort_count(cdm[[cohorts_name]]) %>% 
  left_join(settings(cdm[[cohorts_name]])) %>% 
  mutate(cdm_name = input$cdmName)

toc(log = TRUE)



####### Step 2: Cohort Overlap (Subjects) ###############
# Percentages and counts: Counts only for now, percentages easy
# May want to add names to cohorts

tic(msg = "Calculate Overlap")
if (input$runCalculateOverlap) {
# Summarize the number of IDs in each group
summary_by_group <- cdm[[cohorts_name]] %>%
                    group_by(cohort_definition_id) %>%
                    summarize(ids_in_group = n()) %>% 
                    collect()

# Join to get the IDs that intersect between groups
summary_intersections <- cdm[[cohorts_name]] %>%
  inner_join(cdm[[cohorts_name]], by = "subject_id") %>%
  filter(cohort_definition_id.x != cohort_definition_id.y) %>%
  select(subject_id, cohort_definition_id_x = cohort_definition_id.x, cohort_definition_id_y = cohort_definition_id.y) %>%
  distinct()  %>%
  group_by(cohort_definition_id_x, cohort_definition_id_y) %>%
  summarize(intersect_count = n()) %>% 
  collect()

output$cohort_overlap <- summary_intersections %>% 
  mutate(cdm_name = input$cdmName)
}
toc(log = TRUE)

####### Step 3: Counts : Concepts in Data Source, Orphan concepts, Cohort definition, Index Event Breakdown #########
# Details, Cohort Count ,  Cohort definition, Concept Sets, JSON, SQL
# TO DO: Ideally separate all steps - more loops but less mess
# 2,3 - Using achillesCOdeUse + Phoebe recommendations
# 2,3 - We could add options to use getcodeuse from CodelsitGenerator and the orphanCodes
# TO DO: Need to improve metadata in names of concept sets and names of cohorts
# TO DO: Need to add  Source field and Standard fields
# TO DO: Codelist generator - waiting for predictable table to erase the trycatch
# TO DO: Make the code with achillesCOdeUse faster (probably out of a loop)

#### Test orphans with codelistgen
# orphans <- CodelistGenerator::findOrphanCodes(code_list, cdm)



tic(msg = "Orphan codes + markdown readable text for only first cohort")

cohort_set_res = cohort_set
cohort_set_res$markdown <- ""

code_counts <- tibble()
index_events <- tibble()

for (n in  row_number(cohort_set_res) ) {
  
  cohort <- cohort_set_res$cohort_name[n]  
  json <- paste0(cohort_set_res$json[n]  )
  cohortExpresion <- CirceR::cohortExpressionFromJson(json)
  markdown <- CirceR::cohortPrintFriendly(cohortExpresion)
  cohort_set_res$markdown[n] <-  markdown
  
  ### Ideally reads the same JSON character line
  json2 <- jsonlite::read_json(paste0(cohort_json_dir, "/", cohort, ".json"))
  codes <- codesFromCohort(paste0(cohort_json_dir, "/", cohort, ".json"), cdm, withConceptDetails = F)
  #code_counts_2 <- tibble()
  
  for (code_list in codes) {
    
    codes_id <- code_list
    if (input$runCountCodes) {
      recommended_codes <- concept_recommended %>% 
        filter(concept_id_1 %in% codes_id ) %>% 
        filter(!concept_id_2 %in% codes_id) %>% 
        distinct(concept_id_2, .keep_all = TRUE)
    }
    
    try({
      if (input$runCountCodes) {
        recommended_codes_counts <- achillesCodeUse(list("recomendation" = recommended_codes$concept_id_2),
                                                    cdm,
                                                    countBy = c("record", "person"),
                                                    minCellCount = 5) %>%  
          mutate(standard_concept_id= as.integer(group_level )) %>%
          left_join( recommended_codes, 
                     join_by(standard_concept_id == concept_id_2 ) ) %>%
          mutate(type="reccomended_codes", cohort=cohort )
        
        
        original_codes_counts <- achillesCodeUse(list("original_codes" = codes_id),
                                                 cdm,
                                                 countBy = c("record", "person"),
                                                 minCellCount = 5) %>%  
          mutate(standard_concept_id= as.integer(group_level )) %>% 
          left_join( recommended_codes, 
                     join_by(standard_concept_id == concept_id_2 ) ) %>%
          mutate(type="original_codes", cohort=cohort, relationship_id="original_codes", 
                 concept_id_1=standard_concept_id  )
        
        code_counts <- rbind(code_counts, recommended_codes_counts, original_codes_counts )
      }
    })
  }  
  
  ####### Cohort index
  
  tic(msg = "Index Event Breakdown")
  try({
    if (input$runIndexEvents) {
      Index_events <- summariseCohortCodeUse( x= codes,
                                              cdm, 
                                              cohortTable=cohorts_name,
                                              timing = "entry",
                                              countBy =  c("record", "person"),
                                              byConcept = TRUE,
                                              cohortId = n)
      index_events <- rbind(index_events, Index_events)
    }
  })
  toc(log = TRUE)
  
} 

# save results
output$code_counts  <- code_counts %>% mutate(cdm_name = input$cdmName)
output$index_events <- index_events %>% mutate(cdm_name = input$cdmName)
output$cohort_definitions <- cohort_set_res %>% mutate(cdm_name = input$cdmName)

rm(concept_recommended)
toc(log = TRUE)
   
####### Step 4: Time Distributions #########
# observation time (days) after index , observation time (days) prior to index, time (days) between cohort start and end
# Need to add better characterisation of demographics (a sort of table 1)

tic(msg = "Patient_profiles summary")
#cdm$results_dx <- cdm[[cohorts_name]]
if (input$runProfiling) {
  Patient_profiles <- cdm[[cohorts_name]] %>%
    addDemographics(cdm) %>% 
    collect()   %>%
    mutate( age_group= cut(age, c(seq(0, 110, 5 ), Inf), include.lowest=TRUE))
  
  
  
  Age_distribution <- Patient_profiles %>% group_by(cohort_definition_id, age_group, sex) %>% tally()
  
  
  Time_distribution <- Patient_profiles %>%
    group_by(cohort_definition_id, sex) %>% 
    summarise_at(vars(age, prior_observation, future_observation), list(Min = min, Mean = mean, Median = median,  Max = max, Sd = sd)) %>%
    collect()
  
  rm(Patient_profiles)
  
  output$age_distribution <- Age_distribution %>% mutate(cdm_name = input$cdmName)
  output$time_distribution <- Time_distribution %>% mutate(cdm_name = input$cdmName)
}



toc(log = TRUE)


####### Step 5: 10-13 - Cohort Characterisation : Large scale + temporal + differneces  & matching   ################
#             NEW:  It could include matching cohorts                                     
# Missing differences between them that can be done in shiny step - Also demographics that can be done in previous steps
#  We can get also Visit Context here 
# Low priority: tipe of visits Before, during, simultaneous, after
# Could potentially be extracted from large scale ?

######## Matching pending
# tic(msg = "Matching")
# cdm$sample <- cdm[[cohorts_name]]  %>% 
#                   slice_sample( n=1000, by =cohort_definition_id ) 
# cdm <- cdm %>% CohortConstructor::generateMatchedCohortSet(targetCohortName = "phenotyper_test2" ,
#                                                            targetCohortId = 1:2,
#                                                            name = "matched_cohort",
#                                                            matchSex = TRUE,
#                                                            matchYearOfBirth = TRUE,
#                                                            ratio = 1)
# 
#  toc(log = TRUE)
#  tic(msg = "Large Scale Char ")
#  

 
 
 tic(msg = "Generate 1K Sample and Matched sample")
if (input$runMatchedSampleLSC) {

 cdm$sample <- cdm[[cohorts_name]]  %>% 
   slice_sample( n=1000, by =cohort_definition_id ) %>% compute()
 
 cdm$sample2 <- cdm$sample %>% 
   left_join(cdm$person %>% select(person_id, year_of_birth, gender_concept_id ),
             by=join_by(subject_id==person_id))  %>% compute()
 
 
 
 cdm$person_obs <- cdm$person %>% slice_sample( n=1000, by =year_of_birth )  %>% left_join(cdm$observation_period ) %>% compute()
 
 cdm$matched_cohort <- cdm$sample2 %>% left_join(cdm$person_obs , by=join_by(year_of_birth==year_of_birth, 
                                                                         gender_concept_id==gender_concept_id),
                                             relationship = "many-to-many", 
                                             keep=T)  %>% 
   filter(cohort_start_date>=observation_period_start_date, cohort_start_date<=observation_period_end_date) %>%
   distinct(subject_id, cohort_definition_id, .keep_all = TRUE) %>% 
   select(person_id,cohort_start_date,cohort_end_date, cohort_definition_id ) %>%
   rename( subject_id =person_id    ) %>% compute()
}
 
 # 
 # con <- attr(cdm, "dbcon")
 # DBI::dbWriteTable(con, matched_cohort, overwrite = TRUE, name=Id(schema = "results",table=paste0(prefix,"matched_cohort_test")))
 # cohort_ref <- dplyr::tbl(con, Id(schema = "results",table=paste0(prefix,"matched_cohort_test")))
 # 
 # DBI::dbWriteTable(con, settings(cdm$sample) , overwrite = TRUE, name=Id(schema = "results",table=paste0(prefix,"matched_cohort_test_set")))
 # cohort_set_ref <- dplyr::tbl(con, Id(schema = "results",table=paste0(prefix,"matched_cohort_test_set")))
 # 
 # 
 # cdm[["matched_cohort"]] <- cohort_ref %>% compute()  
 # 
 # cdm$matched_cohort <- omopgenerics::newCohortTable(cdm[["matched_cohort"]],cohortSetRef = cohort_set_ref)
 # 
 # 
toc(log = TRUE)

tic("LArgeScaleChar matched")
if (input$runMatchedSampleLSC) {
  large_scale_char_matched <- summariseLargeScaleCharacteristics(
    cohort=cdm$matched_cohort,
    window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), 
                  c(0, 0), 
                  c(1, 30), c(31, 365),  c(366, Inf)),
    eventInWindow = c("condition_occurrence", "visit_occurrence",
                      "measurement", "procedure_occurrence",  "observation"), 
    episodeInWindow = c("drug_era"),
    #includeSource = TRUE,
    minCellCount = 5,
    minimumFrequency = 0.0005
  )
  output$lsc_matched <- large_scale_char_matched %>% mutate(cdm_name = input$cdmName)
}
toc(log = TRUE)

tic("LArgeScaleChar sample")
if (input$runMatchedSampleLSC) {
  large_scale_char_sample <- summariseLargeScaleCharacteristics(
    cohort=cdm$sample,
    window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), 
                  c(0, 0), 
                  c(1, 30), c(31, 365),  c(366, Inf)),
    eventInWindow = c("condition_occurrence", "visit_occurrence",
                      "measurement", "procedure_occurrence",  "observation"), 
    episodeInWindow = c("drug_era"),
    #includeSource = TRUE,
    minCellCount = 5,
    minimumFrequency = 0.0005
  )
  output$lsc_sample <- large_scale_char_sample %>% mutate(cdm_name = input$cdmName)
}
toc(log = TRUE)

tic("LArgeScaleChar difference")
if (input$runMatchedSampleLSC) {
  difference <- large_scale_char_sample  %>% 
    left_join( large_scale_char_matched, 
               by = join_by(result_type, cdm_name, 
                            group_name, group_level,
                            strata_name, strata_level,  
                            table_name, type, 
                            analysis, concept,
                            variable, variable_level,
                            estimate_type )) %>% 
    mutate(numx =as.double(`estimate.x`),
           numy =as.double(`estimate.y`)) %>%
    mutate(difference =(numx-numy)/numy )
  
  # rm(matched_cohort)
  
  output$lsc_difference <- difference %>% mutate(cdm_name = input$cdmName)
}
toc(log = TRUE)


####### Step 6: - Incidence Rates ################
# Stratified by Age 10y, Gender, Calendar Year
# For now stratified by kid-Adult-Older Adult 
# it is the step it takes longest
 
tic(msg = "Incidence Prevalence Sampling + Denominator")
if (input$runIncidence|input$runPrevalence) {
  if (is.null(input$sampleIncidencePrevalence)) {
    cdmSampled <- cdm 
  } else{
    cdmSampled <- cdmSample(cdm, n = input$sampleIncidencePrevalence)
  }
  
  cdmSampled <- generateDenominatorCohortSet(
    cdm = cdmSampled, 
    name = "denominator", 
    ageGroup = list(c(0,17), c(18,64),
                    c(65,199)),
    sex = c("Male", "Female", "Both"),
    daysPriorObservation = 180,
    overwrite = TRUE
  )
}

toc(log = TRUE)

tic(msg = "Incidence by year, age, sex")

if (input$runIncidence ) {
  
  output$incidence <- estimateIncidence(
    cdm = cdmSampled,
    denominatorTable = "denominator",
    outcomeTable = cohorts_name,
    interval = "years",
    repeatedEvents = FALSE,
    outcomeWashout = Inf,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0 )
  
}

toc(log = TRUE)


tic(msg = "Prevalence by year, age, sex")

if (input$runPrevalence ) {
  output$prevalence <- estimatePeriodPrevalence(
    cdm = cdmSampled,
    denominatorTable = "denominator",
    outcomeTable = cohorts_name,
    interval = "years",
    completeDatabaseIntervals = TRUE,
    fullContribution = FALSE,
    minCellCount = 5,
    temporary = TRUE,
    returnParticipants = FALSE
  )
}


toc(log = TRUE)


rm(cdmSampled)

##### Log close ############

toc(log = TRUE)
tic.log(format = TRUE)
tic_log <- tic.log(format = TRUE)

output$log <- tibble(cdm_name = input$cdmName, log = paste0(tic_log %>%  unlist(), collapse = "\n"))




##### Cleaning the environment ############


# rm(cdm, cdm_Gold,  cdm_Gold_100k,
#    db, code_counts_2, codes,
#    json2, cohortExpresion, original_codes_counts,
#    recommended_codes, recommended_codes_counts)

# rm(list = ls.str(mode = 'numeric'))
# rm(list = ls.str(mode = 'character'))

analyses_performed <- as.integer(c(input$runGenerateCohort, 
                                   input$runCalculateOverlap,
                                   input$runCountCodes,
                                   input$runIndexEvents,
                                   input$runProfiling, 
                                   input$runMatchedSampleLSC, 
                                   input$runIncidence, 
                                   input$runPrevalence, 
                                   !is.null(input$sampleIncidencePrevalence)
))

analyses_performed <-  paste(analyses_performed , collapse = "_")
##### Save results ############
save(input, output, 
     file = here(paste0("Results/", input$cdmName, "_", cohorts_name,"_", analyses_performed, "_" ,format(Sys.time(), "_%Y_%m_%d") , ".RData")))



