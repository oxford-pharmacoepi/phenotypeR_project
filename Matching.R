
#rm(list=ls())

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
library(CohortConstructor)

tic.clearlog()
tic.clear()
tic(msg = "phenotypeR matched total time run: ")



# 
# ##### Connect to database using CDM COnnector ########
# 
# 
 tic(msg = "Connect to database")
# 
# # server_dbi <- Sys.getenv("DB_SERVER_DBI_Pharmetrics") 
# server_dbi <- Sys.getenv("DB_SERVER_DBI_CPRDgold") 
# user <- Sys.getenv("DB_USER") 
# port <- Sys.getenv("DB_PORT") 
# host <- Sys.getenv("DB_HOST")
# 
# db <- dbConnect(RPostgres::Postgres(),
#                 dbname = server_dbi,
#                 port = port,
#                 host = host,
#                 user = user,
#                 password = Sys.getenv("DB_PASSWORD") )
# 
# 
# cdm <- cdm_from_con(con = db,
#                          cdm_schema = "public",
#                          write_schema = "results",
#                     cohort_tables = cohorts_name)



#getVocabVersion(cdm=cdm_Gold)

toc(log = TRUE)

################################################################################

tic(msg = "Settings and loading of Phoebe")
# ~/CohortDx2023/phenotypeR_project/Results
#cohort_json_dir <- here("Cohorts/")
#cohorts_name <- "hpv_diagnostics_cohorts"

toc(log = TRUE)


tic(msg = "Generate Cohort Set")


#list.files(cohort_json_dir)
# cohort_set <- read_cohort_set(cohort_json_dir)

# cdm <-   generateCohortSet(cdm, 
#                            cohort_set,
#                            name = cohorts_name,
#                            computeAttrition = TRUE,
#                            overwrite = FALSE)

toc(log = TRUE)


tic(msg = "Generate Sample and Matched sample")


toc(log = TRUE)


tic(msg = "Generate Sample and Matched sample")

cdm$sample <- cdm[[cohorts_name]]  %>% 
  slice_sample(n  = 1000,
               by = cohort_definition_id) 

cdm <- CohortConstructor::generateMatchedCohortSet(cdm   = cdm,
                                                   name  = "matched_cohort", 
                                                   targetCohortName = sample,
                                                   targetCohortId   = cohort_definition_id,
                                                   matchSex         = TRUE,
                                                   matchYearOfBirth = TRUE,
                                                   ratio = 1)

toc(log = TRUE)

tic("LargeScaleChar matched")

difference <- cdm[["matched_cohort"]] %>%
  PatientProfiles::summariseLargeScaleCharacteristics(
    window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), c(0, 0), c(1, 30), 
                  c(31, 365),  c(366, Inf)),
    eventInWindow   =  c("condition_occurrence", "visit_occurrence", "measurement", 
                          "procedure_occurrence",  "observation"),
    episodeInWindow =  c("drug_era"),
    minCellCount    = 5,
  )

toc(log = TRUE)



#write.csv(difference, paste0("~/CohortDx2023/phenotypeR_project/Results/dosage_",cohorts_name, "difference_lsc_",format(Sys.time(), "_%Y_%m_%d") , ".csv"))
#write.csv(prev, paste0("~/CohortDx2023/phenotypeR_project/Results/dosage_",cohorts_name, "prevalence_",format(Sys.time(), "_%Y_%m_%d") , ".csv"))
#write.csv(inc, paste0("~/CohortDx2023/phenotypeR_project/Results/dosage_",cohorts_name, "incidence_",format(Sys.time(), "_%Y_%m_%d") , ".csv"))
