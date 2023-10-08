
rm(list=ls())

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
tic(msg = "phenotypeR matched total time run: ")




##### Connect to database using CDM COnnector ########


tic(msg = "Connect to database")

# server_dbi <- Sys.getenv("DB_SERVER_DBI_Pharmetrics") 
server_dbi <- Sys.getenv("DB_SERVER_DBI_CPRDgold") 
user <- Sys.getenv("DB_USER") 
port <- Sys.getenv("DB_PORT") 
host <- Sys.getenv("DB_HOST")

db <- dbConnect(RPostgres::Postgres(), 
                dbname = server_dbi, 
                port = port, 
                host = host, 
                user = user, 
                password = Sys.getenv("DB_PASSWORD") ) 


cdm <- cdm_from_con(con = db,
                         cdm_schema = "public",
                         write_schema = "results",
                    cohort_tables = "hpv_vaccines_cohorts")



#getVocabVersion(cdm=cdm_Gold)

toc(log = TRUE)

################################################################################

tic(msg = "Settings and loading of Phoebe")
# ~/CohortDx2023/phenotypeR_project/Results
cohort_json_dir <- here("Cohorts/")
cohorts_name <- "hpv_diagnostics_cohorts"

toc(log = TRUE)


tic(msg = "Generate Cohort Set")


#list.files(cohort_json_dir)
cohort_set <- read_cohort_set(cohort_json_dir)

cdm <-   generateCohortSet(cdm, 
                           cohort_set,
                           name = cohorts_name,
                           computeAttrition = TRUE,
                           overwrite = TRUE)

toc(log = TRUE)


tic(msg = "Generate Sample and Matched sample")


cdm$sample <- cdm[[cohorts_name]]  %>% 
              slice_sample( n=1000, by =cohort_definition_id ) 

cdm$sample2 <- cdm$sample %>% 
              left_join(cdm$person %>% select(person_id, year_of_birth, gender_concept_id ),
              by=join_by(subject_id==person_id))  %>% collect()



cdm$person_obs <- cdm$person %>% slice_sample( n=1000, by =year_of_birth )  %>% left_join(cdm$observation_period )  %>% collect()

matched_cohort <- cdm$sample2 %>% left_join(cdm$person_obs , by=join_by(year_of_birth==year_of_birth, 
                                               gender_concept_id==gender_concept_id),
                                               relationship = "many-to-many", 
                                               keep=T)  %>% 
                                               filter(cohort_start_date>=observation_period_start_date, cohort_start_date<=observation_period_end_date) %>%
                                               distinct(subject_id, cohort_definition_id, .keep_all = TRUE) %>% 
                                               select(person_id,cohort_start_date,cohort_end_date, cohort_definition_id ) %>%
                                                rename( subject_id =person_id    ) %>% collect() 
           
con <- attr(cdm, "dbcon")
DBI::dbWriteTable(con, matched_cohort, overwrite = TRUE, name=Id(schema = "results",table="matched_cohort_test"))
cohort_ref <- dplyr::tbl(con, Id(schema = "results",table="matched_cohort_test"))

DBI::dbWriteTable(con, cohort_set(cdm$sample) , overwrite = TRUE, name=Id(schema = "results",table="matched_cohort_test_set"))
cohort_set_ref <- dplyr::tbl(con, Id(schema = "results",table="matched_cohort_test_set"))


cdm[["matched_cohort"]] <- cohort_ref
cdm$matched_cohort <- new_generated_cohort_set(cdm[["matched_cohort"]],cohort_set_ref = cohort_set_ref, overwrite = TRUE)
  

toc(log = TRUE)
                                   
tic("LArgeScaleChar matched")
large_scale_char_matched <- summariseLargeScaleCharacteristics(
  cohort=cdm$matched_cohort,
  window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), 
                c(0, 0), 
                c(1, 30), c(31, 365),  c(366, Inf)),
  # window =list(c(0, 0)),
  tablesToCharacterize = c("condition_occurrence", "drug_era", "visit_occurrence",
                           "measurement", "procedure_occurrence",  "observation"), 
  overlap = TRUE,
  minCellCount = 5
)

toc()

tic("LArgeScaleChar sample")
large_scale_char_sample <- summariseLargeScaleCharacteristics(
  cohort=cdm$sample,
  window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), 
                c(0, 0), 
                c(1, 30), c(31, 365),  c(366, Inf)),
  # window =list(c(0, 0)),
  tablesToCharacterize = c("condition_occurrence", "drug_era", "visit_occurrence",
                           "measurement", "procedure_occurrence",  "observation"), 
  overlap = TRUE,
  minCellCount = 5
)
toc()


difference <- large_scale_char_sample  %>% 
                                          left_join( large_scale_char_matched, 
                                                    by = join_by(cohort_name, strata_name, 
                                                                 strata_level, table_name, 
                                                                 window_name, concept_id, 
                                                                 concept_name,  cdm_name,
                                                                 result_type)) %>% 
                                            mutate(difference =(`%.x`-`%.y`)/`%.y` ) %>% 
                                            filter(count.x!="<5")

#write.csv(difference, paste0("~/CohortDx2023/phenotypeR_project/Results/dosage_",cohorts_name, "difference_lsc_",format(Sys.time(), "_%Y_%m_%d") , ".csv"))
#write.csv(prev, paste0("~/CohortDx2023/phenotypeR_project/Results/dosage_",cohorts_name, "prevalence_",format(Sys.time(), "_%Y_%m_%d") , ".csv"))
#write.csv(inc, paste0("~/CohortDx2023/phenotypeR_project/Results/dosage_",cohorts_name, "incidence_",format(Sys.time(), "_%Y_%m_%d") , ".csv"))
