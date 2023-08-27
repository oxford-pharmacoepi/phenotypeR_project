rm(list=ls())


# renv::snapshot()

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
# remotes::install_github("darwin-eu/CodelistGenerator@summarise_cohort_code_use", force=TRUE )
# install.packages("DrugUtilisation")
# install.packages("IncidencePrevalence")
# install.packages("tictoc")
# pending install: SqlRender

### Open libraries
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



tic.clearlog()
tic.clear()
tic(msg = "Phenotyper Total time run: ")


##### Connect to database using CDM COnnector ########


tic(msg = "Connect to database")

server_dbi <- Sys.getenv("DB_SERVER_DBI_Pharmetrics") 
user <- Sys.getenv("DB_USER") 
port <- Sys.getenv("DB_PORT") 
host <- Sys.getenv("DB_HOST")

db <- dbConnect(RPostgres::Postgres(), 
                dbname = server_dbi, 
                port = port, 
                host = host, 
                user = user, 
                password = Sys.getenv("DB_PASSWORD") ) 


cdm_Gold <- cdm_from_con(con = db,
                         cdm_schema = "public",
                         write_schema = "results")

 cdm_Gold_100k <- cdm_from_con(con = db,
                               cdm_schema = "public_100k",
                               write_schema = "results")

#getVocabVersion(cdm=cdm_Gold)
  
toc(log = TRUE)



##### Options and set-up:  directories and settings ######


tic(msg = "Settings and loading of Phoebe")

cohort_json_dir <- "~/CohortDx2023/PhenotypeR_project/Cohorts/DMPM/"
cdm <- cdm_Gold_100k
cohorts_name <- "pmdm_diagnostics_cohorts"
concept_recommended <- read.csv("~/CohortDx2023/PhenotypeR_project/Phoebe/concept_recommended.csv")

toc(log = TRUE)


####### Step 1: Get cohorts and generate them #######
# now from json, but we can do with CapR other sources 
#



tic(msg = "Generate Cohort Set")


#list.files(cohort_json_dir)
cohort_set <- read_cohort_set(cohort_json_dir)

cdm <-   generateCohortSet(cdm, 
                           cohort_set,
                           name = cohorts_name,
                           computeAttrition = TRUE,
                           overwrite = TRUE)

toc(log = TRUE)

################ 9 - Cohort Overlap (Subjects) ###############
# Percentages and counts: Counts only for now, percentages easy
# May want to add names to cohorts

tic(msg = "Calculate Overlap")

# Summarize the number of IDs in each group
summary_by_group <- cdm$pmdm_diagnostics_cohorts %>%
                    group_by(cohort_definition_id) %>%
                    summarize(ids_in_group = n()) %>% 
                    collect()

# Join to get the IDs that intersect between groups
summary_intersections <- cdm$pmdm_diagnostics_cohorts %>%
  inner_join(cdm$pmdm_diagnostics_cohorts, by = "subject_id") %>%
  filter(cohort_definition_id.x != cohort_definition_id.y) %>%
  select(subject_id, cohort_definition_id_x = cohort_definition_id.x, cohort_definition_id_y = cohort_definition_id.y) %>%
  distinct()  %>%
  group_by(cohort_definition_id_x, cohort_definition_id_y) %>%
  summarize(intersect_count = n()) %>% 
  collect()

toc(log = TRUE)

######### 2- Concepts in Data Source  #########
######### 3- Orphan concepts          #########
######### 1 - Cohort definition       #########
# Details, Cohort Count ,  Cohort definition, Concept Sets, JSON, SQL
# TO DO: Ideally separate all steps - more loops but less mess
# 2,3 - now use the precounted table + Phoebe recommendations
# 2,3 - We could add options to use getcodeuse from CodelsitGenerator and the Codelist generator search itself
# TO DO: Need to improve metadata in names of concept sets and names of cohorts
# TO DO: Need to add  Source field and Standard fields

tic(msg = "Orphan codes + markdown readable text for only first cohort")

cohort_set_res = cohort_set
cohort_set_res$markdown <- ""
counts_table <- dbSendQuery(db, "SELECT * FROM results.cohort_diagnostics_concept_counts_permanent_table")
counts_table <- dbFetch(counts_table)


code_counts <- tibble()

for (n in  row_number(cohort_set_res) ) {

  
  cohort <- cohort_set_res$cohort_name[n]  
  json <- paste0(cohort_set_res$json[n]  )
  cohortExpresion <- CirceR::cohortExpressionFromJson(json)
  markdown <- CirceR::cohortPrintFriendly(cohortExpresion)
  cohort_set_res$markdown[n] <-  markdown
  
  ### Ideally reads the same JSON character line
  json2 <- jsonlite::read_json(here(paste0("Cohorts/DMPM/", cohort, ".json")))
  codes <- codesFromCohort(here(paste0("Cohorts/DMPM/", cohort, ".json")), cdm, withConceptDetails = F)
  
  code_counts_2 <- tibble()
  
  for (code_list in codes) {

    codes_id <- code_list
    
    recommended_codes <- concept_recommended %>% 
      filter(concept_id_1 %in% codes_id ) %>% 
      filter(!concept_id_2 %in% codes_id) %>% 
      distinct(concept_id_2)
    
    recommended_codes_counts <- recommended_codes %>%
      left_join( counts_table, join_by(concept_id_2 == concept_id) )  %>%
      rename( concept_id=concept_id_2) %>%
      filter(!is.na(concept_count))%>%
      mutate(type="recommendation", cohort=cohort)
    
    
    original_codes_counts <- counts_table %>% filter(concept_id %in% codes_id) %>%
    mutate(type="original_codes", cohort=cohort )
    
    code_counts <- rbind(code_counts, recommended_codes_counts, original_codes_counts )
  }  
  
   } 
   
   # recommended_codes_counts <- summariseCodeUse(recommended_codes$concept_id_2,
   #                                    cdm,
   #                                    countBy =  "person",
   #                                    byConcept = TRUE,
   #                                    byYear = FALSE,
   #                                    bySex = FALSE,
   #                                    ageGroup = NULL,
   #                                    minCellCount = 0 )


  rm(counts_table)
  rm(concept_recommended)
   toc(log = TRUE)
   
   
######### 4 - Cohort Counts #########
# Subjects and records can be gotten from the cohort_count 
   
tic(msg = "Cohort counts, attrition, subset cdm")
   
# cdm$pmdm_diagnostics_cohorts %>% glimpse()
cohort_count <- cohort_count(cdm$pmdm_diagnostics_cohorts)
cohort_attrition <- cohort_attrition(cdm$pmdm_diagnostics_cohorts)
cohort_set_cdm <- cohort_set(cdm$pmdm_diagnostics_cohorts)

cdm_pmdm <- cdm %>% 
  cdm_subset_cohort(cohort_table = "pmdm_diagnostics_cohorts")

toc(log = TRUE)


######### 6 - Time Distributions #########
# observation time (days) after index , observation time (days) prior to index, time (days) between cohort start and end
# Need to add better characterisation of demographics (a sort of table 1)

tic(msg = "Patient_profiles summary")
cdm_pmdm$results_dx <- cdm_pmdm$pmdm_diagnostics_cohorts 

Patient_profiles <- cdm_pmdm$results_dx %>%
  addDemographics(cdm_pmdm) %>% 
  group_by(cohort_definition_id, sex) %>% 
  summarise_at(vars(age, prior_observation, future_observation), list(Min = min, Mean = mean, Median = median,  Max = max, Sd = sd)) %>%
  collect()


toc(log = TRUE)


######## # 10-13 - Cohort Characterisation : Large scale + temporal + differneces    ################
# Missing differences between them that can be done in shiny step - Also demographics that can be done in previous steps
########### 8 - Visit Context  ###########
# Low priority: tipe of visits Before, during, simultaneous, after
# Could potentially be extracted from large scale ?

tic(msg = "Large Scale Char ")

 large_scale_char <- summariseLargeScaleCharacteristics(
                     cohort=cdm_pmdm$pmdm_diagnostics_cohorts,
                     window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), 
                                   c(0, 0), 
                                   c(1, 30), c(31, 365),  c(366, Inf)),
                     # window =list(c(0, 0)),
                     tablesToCharacterize = c("condition_occurrence", "drug_era", "visit_occurrence"), 
                     # Further options:
                     # "condition_occurrence", "drug_exposure", 
                     # "procedure_occurrence", "device_exposure", "measurement", 
                     # "observation", "drug_era", "condition_era", "specimen"),
                     overlap = TRUE,
                     minCellCount = 5
                   )

 
 toc(log = TRUE)
 
 

 
 
 ########### 5 - Incidence Rates ################
 # Stratified by Age 10y, Gender, Calendar Year
 # For now stratified by kid-Adult-Older Adult 
 # it is the step it takes longest
 
 tic(msg = "Incidence by year, age, sex")
 
 
cdm <- generateDenominatorCohortSet(
  cdm = cdm, 
  name = "denominator", 
  cohortDateRange = NULL,
  ageGroup = list(c(0,17), c(18,64),
                  c(65,199)),
  sex = c("Male", "Female", "Both"),
  daysPriorHistory = 180
)



inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "pmdm_diagnostics_cohorts",
  interval = "years",
  repeatedEvents = FALSE,
  outcomeWashout = Inf,
  completeDatabaseIntervals = FALSE,
  minCellCount = 0 )



toc(log = TRUE)





########  7 - Index Event Breakdown ##############
# Only missing thing !
#  Add source field and Standard fields subjects and records
tic(msg = "Index Event Breakdown: not yet implemented")


toc(log = TRUE)

############# Logs ############

toc(log = TRUE)
tic.log(format = TRUE)
tic_log <- tic.log(format = TRUE)



############# Cleaning the environment ############


rm(cdm, cdm_Gold,cdm_pmdm, concept_recommended, 
   db, code_counts_2, codes, counts_table, 
   json2, cohortExpresion, original_codes_counts,
   recommended_codes, recommended_codes_counts)

 rm(list = ls.str(mode = 'numeric'))
 rm(list = ls.str(mode = 'character'))






