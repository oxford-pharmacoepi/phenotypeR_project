# renv::restore()

# packages ----
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
library(CirceR)
library(visOmopResults)
library(tidyr)

# database metadata and connection details -----
# The name/ acronym for the database
db_name <- "..."

# Database connection details -----
db <- dbConnect(RPostgres::Postgres(),
                dbname = Sys.getenv("..."), 
                port = Sys.getenv("..."),
                host = Sys.getenv("..."),
                user = Sys.getenv("..."),
                password =  Sys.getenv("..."))
cdm_schema <- "..."
write_schema <- "..."
achilles_schema <- "..."

# Table prefix and name-----
# any tables created in the database during the analysis will start with this prefix
# we provide the default here but you can change it
# note, any existing tables in your write schema starting with this prefix may
# be dropped during running this analysis
study_prefix <- "..."
# Name of the table to be created or read (if runGenerateCohort is FALSE)
cohort_table <- "..."

# Settings ----
# runGenerateCohort: Generate cohort or use preloaded cohorts
# cohortNames: names of the cohorts to be loaded (if runGenerateCohort is FALSE)
# runCalculateOverlap: Calculate Overlap
# runCountCodes: run orphan codes and count codes
# runIndexEvents: run index events
# runProfiling: run age and time in database characterisation
# runMatchedSampleLSC: run matched LSC
# runIncidence: run Incidence
# runPrevalence: run Prevalence
# sampleIncidencePrevalence: Sample for Incidence Prevalence (NULL if all cdm)

runGenerateCohort <- TRUE 
runCalculateOverlap <- TRUE       
runCountCodes <- TRUE                
runIndexEvents <- TRUE                 
runProfiling <- TRUE                   
runMatchedSampleLSC <- TRUE         
runIncidence <- TRUE                
runPrevalence <- TRUE               
sampleIncidencePrevalence <- 1000000 
exportResultsRData <- TRUE 

# zip file containing the csv with recommended codes
zipFileName <- "concept_recommended_20221006.zip"

# Run the study ------
source(here("RunStudy.R"))
