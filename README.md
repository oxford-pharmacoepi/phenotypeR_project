# phenotypeR_project
Project to start developing fast phenotyping tools

> Prerequisite  files:
  - Please download the 'concept_recommended.csv' available at: https://forums.ohdsi.org/t/phoebe-2-0/17410 and stor it into the project folder named 'Phoebe'.
  - Include your json files into the project folder named 'Cohorts_json'
  - Install/load library(remotes)
  - Install the darwin-eu/CodelistGenerator - under developement thus, check which branch has the following function: codesFromCohort()
    -- # remotes::install_github("darwin-eu/CodelistGenerator@summarise_cohort_code_use", force=TRUE )
    -- # remotes::install_github("darwin-eu/CodelistGenerator@codes_from_cohort", force=TRUE )
  - Update DrugUtilization package to v0.3.2 (needs to update renv file)


> Details of Main Code:
  - Line 84: define the cohort diagnostics name (i.e., replace 'name_diagnostics_cohorts' ) 
    cohorts_name <- "name_diagnostics_cohorts"
  

> Reporting errors/changes during developement:
  - For ######### 1 - Cohort definition       ######### 
    -- during a re-run, this error appeared:
      Warning message: In result_create(conn@ptr, statement, immediate) : Closing open result set, cancelling previous query
    --- To avoid this error, I had included ", n = -1" to dbFetch() AND clear the query
      dbFetch(counts_table, n = -1)
      dbClearResult(counts_table_query)
    --- FONT: https://stackoverflow.com/questions/61229489/how-do-i-fix-the-warning-message-closing-open-result-set-cancelling-previous-q 
  - For ########### 5 - Incidence Rates ################ 
    -- it requires to have testthat installed. 
  - For ########### 8 - Visit Context  ###########
    -- Error in checkInputs(cohort = cohort, cdm = cdm, window = window, tablesToCharacterize = tablesToCharacterize,  : 
    argument "cdm" is missing, with no default
    --- Fixed by specifying cdm = cdm in the summariseLargeScaleCharacteristics()
    -- Error regarding missing values. Solution: 
    --- Update DrugUtilization package to v0.3.2 (needs to update renv file)

