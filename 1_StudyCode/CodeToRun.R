
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

# database metadata and connection details -----
# The name/ acronym for the database
db_name <- "CPRD GOLD 100k"

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
study_prefix <- "phenotyper_"

# Run the study ------
source(here("RunStudy.R"))

