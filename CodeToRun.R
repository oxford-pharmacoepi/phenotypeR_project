renv::restore()

# packages #####
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
library(readr)
library(stringr)
library(testthat)
library(SqlRender)
Sys.setenv(DB_PORT = "5432",
           DB_HOST = "163.1.65.51",
           DB_USER = "mdu",
           DB_PASSWORD = "Liv12094")

server_dbi <-"cdm_gold_202307"
# database metadata and connection details -----
# The name/ acronym for the database
db_name <- "CPRD GOLD"

# Database connection details -----
db <- dbConnect(RPostgres::Postgres(),
                dbname = "cdm_gold_202307", 
                port = Sys.getenv("DB_PORT"),
                host = Sys.getenv("DB_HOST"),
                user = Sys.getenv("DB_USER"),
                password =  Sys.getenv("DB_PASSWORD"))
cdm_schema <- "public_100k"
write_schema <- "results"
achilles_schema <- "results"

# Table prefix -----
# any tables created in the database during the analysis will start with this prefix
# we provide the default here but you can change it
# note, any existing tables in your write schema starting with this prefix may
# be dropped during running this analysis
study_prefix <- "mike_"

#create cohort
# cdm <- cdm_from_con(con = db,
#                     cdm_schema = c(schema = cdm_schema),
#                     write_schema = c(schema= write_schema, prefix = study_prefix),
#                     achilles_schema = achilles_schema, 
#                     cdm_name = db_name
# )
# 
# data <- read.csv("phenotype.csv") |> 
#   dplyr::filter(Include == "Y") |> 
#   dplyr::select(concept_id, Cohort_name)
# 
# code_list <- split(data$concept_id, data$Cohort_name)
# 
# remotes::install_github("oxford-pharmacoepi/CohortConstructor")
# 
# 
# codeList <- omopgenerics::newCodelist(code_list)
# 
# cdm <- generate_concept_cohort_set(
#   cdm,
#   concept_set = codeList,
#   name = "cohort",
#   limit = "first",
#   required_observation = c(0, 0),
#   end = "observation_period_end_date",
#   subset_cohort = NULL,
#   subset_cohort_id = NULL,
#   overwrite = TRUE
# )

# Run the study ------
source(here("RunStudy.R"))

