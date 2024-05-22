# Log start ------
tic.clearlog()
tic.clear()
tic(msg = "phenotypeR total time run: ")

# Set-up: directories and settings ------
tic(msg = "Project settings and loading of Phoebe")

# To export output 
result_names <- c("cohort_definitions", "cohort_details", "code_counts", "cohort_overlap", "cohort_timing",
                  "characteristics", "prevalence", "incidence", "snapshot", "orphan_codes", 
                  "index_events", "lsc_sample", "lsc_matched", "lsc_difference", "log")

output <- vector("list", length(result_names)) |> setNames(result_names)

# Concept recommended
if (runCountCodes){
  if (file.exists(here("Phoebe", zipFileName))) {
    unzip(zipfile = here("Phoebe", zipFileName), exdir = here("Phoebe"))
    concept_recommended <- read_csv(here("Phoebe", "concept_recommended.csv"),
                                    show_col_types = FALSE)
  } else { 
    runCountCodes <- FALSE
    cli::cli_alert(paste0("`runCountCodes` set to FALSE because there isn't the zip file `", zipFileName, "`"))
  }
}

# Checks
if (runGenerateCohort) {
  jsons <- readCohortSet(here("Cohorts"))
  if (nrow(jsons) == 0) {
    cli::cli_abort(c("x" = "There aren't JSON files in the folder 'Cohorts'"))
  }
}
toc(log = TRUE)

# Database connexion ----
tic(msg = "Connect to database")
if (runGenerateCohort) {
  cli::cli_bullets(c("*" = "Connecting to database"))
  cdm <- cdm_from_con(con = db,
                      cdm_schema = c(schema = cdm_schema),
                      write_schema = c(schema= write_schema, prefix = study_prefix),
                      achilles_schema = achilles_schema, 
                      cdm_name = db_name
  )
} else {
  cli::cli_bullets(c("*" = "Connecting to database and loading cohorts"))
  cdm <- cdm_from_con(
    con = db,
    cdm_schema = c(schema = cdm_schema),
    write_schema = c(schema= write_schema, prefix = study_prefix),
    achilles_schema = achilles_schema,
    cohort_tables = cohort_table, 
    cdm_name = db_name  
  )
}
toc(log = TRUE)

# Get cdm snapshot -----
tic(msg = "Getting cdm snapshot")
output$snapshot <- summary(cdm) 
toc(log = TRUE)

# Generate cohorts ----
if (runGenerateCohort) {
  tic(msg = "Instantiating cohorts")
  cdm <-   generateCohortSet(cdm, 
                             jsons,
                             name = cohort_table,
                             computeAttrition = TRUE,
                             overwrite = TRUE)
  toc(log = TRUE)
}

# Cohort details ----
cli::cli_bullets(c("*" = "Getting cohort summary"))
tic(msg = "Cohort details")
output$cohort_details <- summary(cdm[[cohort_table]]) 
toc(log = TRUE)

# Cohort definitions ----
if (runGenerateCohort) {
  tic(msg = "Markdown readable text")
  cli::cli_bullets(c("*" = "Getting cohort definitions"))
  output$cohort_definitions <- jsons |>
    rowwise() |>
    mutate(
      markdown = cohortPrintFriendly(cohortExpressionFromJson(.data$json)),
      cdm_name = cdmName(cdm)
    ) 
  toc(log = TRUE)
}

# Cohort overlap ----
if (runCalculateOverlap) {
  tic(msg = "Calculate Overlap")
  cli::cli_bullets(c("*" = "{.strong Summarising cohort overlap}"))
  output$cohort_overlap <- CohortCharacteristics::summariseCohortOverlap(cdm[[cohort_table]])
  toc(log = TRUE)
}

# Cohort timing ----
if (runCohortTiming) {
  tic(msg = "Calculate cohort timings")
  cli::cli_bullets(c("*" = "{.strong Summarising cohort timing}"))
  output$cohort_timing <- CohortCharacteristics::summariseCohortTiming(cdm[[cohort_table]], density = TRUE) 
  toc(log = TRUE)
}

# Code counts ----
if(runCountCodes) {
  tic(msg = "Code counts and orphan code counts")
  cli::cli_bullets(c("*" = "{.strong Summarising code use}"))
  codes <- codesFromCohort(here("Cohorts"), cdm, withConceptDetails = F)
  # original codes
  output$code_counts <- summariseAchillesCodeUse(codes, cdm = cdm) 
  # orphan codes
  cli::cli_bullets(c("*" = "{.strong Getting orphan codes}"))
  orphanCodelist <- lapply(codes, function(codelist, phoebe = concept_recommended) {
    phoebe |>
      filter(concept_id_1 %in% codelist) |>  
      filter(!concept_id_2 %in% codelist) |>  
      distinct(concept_id_2, .keep_all = TRUE) |> 
      pull("concept_id_2")
  })
  cli::cli_bullets(c("*" = "{.strong Summarise orphan code use}"))
  output$orphan_codes <- summariseAchillesCodeUse(orphanCodelist, cdm = cdm) |>
    # make it "orphan_codes" fromat:
    left_join(
      concept_recommended |>
        mutate(across(starts_with("concept"), ~ as.character(.x))) |>
        select("variable_level" = "concept_id_2", "relationship_id") |>
        distinct(),
      by = "variable_level",
      relationship = "many-to-many"
    ) |>
    mutate(
      additional_name = paste0(.data$additional_name, " &&& relationship_id"),
      additional_level = paste0(.data$additional_level, " &&& ", .data$relationship_id)
    ) |> 
    select(-"relationship_id")
  attr(output$orphan_codes, "settings") <- attr(output$orphan_codes, "settings") |> mutate(result_type = "orphan_codes")
  toc(log = TRUE)
}

# Index Event ----
if (runIndexEvents) {
  tic(msg = "Index Event Breakdown")
  cli::cli_bullets(c("*" = "{.strong Getting index event breakdown}"))
  cohortSet <- settings(cdm[[cohort_table]])
  if (!runCountCodes) {
    codes <- codesFromCohort(here("Cohorts"), cdm, withConceptDetails = F)
  }
  index_events <- list()
  for (id in cohortSet$cohort_definition_id) {
    index_events[[id]] <- summariseCohortCodeUse(
      x = codes[id],
      cdm = cdm, 
      cohortTable = cohort_table,
      timing = "entry",
      countBy =  c("record", "person"),
      byConcept = TRUE,
      cohortId = id
    )
  }
  # change to bind when omopgenerics #336
  output$index_events <- index_events |> bind_rows()
  file.remove(here("Phoebe", "concept_recommended.csv"))
  toc(log = TRUE)
}


# Demographics distirbution ----
if (runProfiling) {
  tic(msg = "Patient_profiles summary")
  cli::cli_bullets(c("*" = "{.strong Summarising cohort characteristics}"))
  output$characteristics <- cdm[[cohort_table]] |>
    addSex() |>
    mutate(time_in_cohort = as.numeric(cohort_end_date - cohort_start_date)) |>
    CohortCharacteristics::summariseCharacteristics(
      strata = c("sex"),
      ageGroup = lapply(as.list(1:length(seq(0, 110, 5))), function(k, x1 = seq(0, 110, 5), x2 = seq(4, 120, 5)) {c(x1[k], x2[k])}),
      tableIntersectCount = list(
        "Number visits prior year" = list(
          tableName = "visit_occurrence", 
          window = c(-365, -1)
        )
      ),
      otherVariables = "time_in_cohort",
      otherVariablesEstimates = c("min", "q25", "median", "q75", "max")
    )
  toc(log = TRUE)
}

# Large scale ----
# NEW:  It could include matching cohorts                                     
# Missing differences between them that can be done in shiny step - Also demographics that can be done in previous steps
#  We can get also Visit Context here 
# Low priority: tipe of visits Before, during, simultaneous, after
# Could potentially be extracted from large scale ?
# TODO: use macthCohorts when ready
if (runMatchedSampleLSC) {
  tic(msg = "Generate 1K Sample and Matched sample")
  cli::cli_bullets(c("*" = "{.strong Generating the match sample}"))
  cdm$sample <- cdm[[cohort_table]]  |> 
    slice_sample(n = 1000, by = cohort_definition_id) |> 
    compute()
  cdm$sample2 <- cdm$sample |> 
    left_join(
      cdm$person |> select(person_id, year_of_birth, gender_concept_id),
      by = join_by(subject_id == person_id)
    ) |>
    compute()
  
  cdm$person_obs <- cdm$person |> 
    slice_sample(n = 1000, by = year_of_birth) |> 
    left_join(cdm$observation_period, by = "person_id") |> 
    compute()
  
  cdm$matched_cohort <- cdm$sample2 |> 
    left_join(
      cdm$person_obs , 
      by=join_by(year_of_birth == year_of_birth, gender_concept_id == gender_concept_id),
      relationship = "many-to-many", 
      keep = TRUE
    ) |> 
    filter(
      cohort_start_date >= observation_period_start_date,  
      cohort_start_date <= observation_period_end_date
    ) |>
    distinct(subject_id, cohort_definition_id, .keep_all = TRUE) |> 
    select(
      c("person_id", "cohort_start_date", 
        "cohort_end_date", "cohort_definition_id")
    ) |>
    rename(subject_id = person_id) |> 
    compute()
  toc(log = TRUE)
  
  tic("Large scale characteristics matched")
  cli::cli_bullets(c("*" = "{.strong Summarising large scale characteristics}"))
  lsc_matched <-  CohortCharacteristics::summariseLargeScaleCharacteristics(
    cohort = cdm$matched_cohort,
    window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), c(0, 0), 
                  c(1, 30), c(31, 365),  c(366, Inf)),
    eventInWindow = c("condition_occurrence", "visit_occurrence",
                      "measurement", "procedure_occurrence",  "observation"), 
    episodeInWindow = c("drug_era"),
    minimumFrequency = 0.0005
  )
  toc(log = TRUE)
  
  tic("Large scale characteristics sample")
  lsc_sample <- CohortCharacteristics::summariseLargeScaleCharacteristics(
    cohort = cdm$sample,
    window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), c(0, 0), 
                  c(1, 30), c(31, 365),  c(366, Inf)),
    eventInWindow = c("condition_occurrence", "visit_occurrence",
                      "measurement", "procedure_occurrence",  "observation"), 
    episodeInWindow = c("drug_era"),
    minimumFrequency = 0.0005
  )
  toc(log = TRUE)
  
  tic("Large scale characteristics differences")
  cli::cli_bullets(c("*" = "{.strong Summarising large scale differences}"))
  # TODO: use summariseSMD when ready
  # TODO check que es mantinguin els settings
  output$lsc_difference <- lsc_matched |>
    select(-"group_name") |>
    rename("cohort_name_reference" = "group_level") |> 
    mutate(estimate_name = paste0(estimate_name, "_reference"),
           estimate_value = as.numeric(estimate_value)) |>
    select(-"estimate_type") |>
    pivot_wider(names_from = estimate_name, values_from = estimate_value) |>
    left_join(
      lsc_sample |>
        select(-"group_name") |>
        rename("cohort_name_comparator" = "group_level") |> 
        mutate(estimate_name = paste0(estimate_name, "_comparator"),
               estimate_value = as.numeric(estimate_value)) |>
        select(-"estimate_type") |>
        pivot_wider(names_from = estimate_name, values_from = estimate_value),
      by = join_by(result_id, cdm_name, strata_name, strata_level, variable_name, 
                   variable_level, additional_name, additional_level, ),
      relationship = "many-to-many"
    ) |>
    mutate(
      smd = (percentage_comparator/100 - percentage_reference/100) / sqrt((percentage_comparator/100*(1-percentage_comparator/100) + percentage_reference/100*(1-percentage_reference/100))/2),
      difference = (percentage_comparator-percentage_reference)/percentage_reference,
      across(.cols = contains("count"), .fn = ~if_else(.x < 5 & .x > 0, NA, .x)), # min cell count
      smd = if_else(is.na(count_reference) | is.na(count_comparator), NA_character_, as.character(smd)), # min cell count
      difference = if_else(is.na(count_reference) | is.na(count_comparator), NA_character_, as.character(difference)), # min cell count
    ) |>
    uniteGroup(cols = c("cohort_name_reference", "cohort_name_comparator")) |>
    pivot_longer(cols = c("smd", "difference"), names_to = "estimate_name", values_to = "estimate_value") |>
    mutate(estimate_type = "numeric", result_type = "large_scale_differences") |>
    select(-starts_with("percentage"), -starts_with("count")) |>
    omopgenerics::newSummarisedResult(settings = settings(lsc_matched) |> mutate(result_type = "summarised_large_scale_differences"))
  toc(log = TRUE)
  # to export
  output$lsc_matched <- lsc_matched |>
    mutate(group_name = if_else(group_name == "cohort_name", "cohort_name_reference", group_name)) 
  output$lsc_sample <- lsc_sample |>
    mutate(group_name = if_else(group_name == "cohort_name", "cohort_name_comparator", group_name)) 
}


# Incidence and prevalence ----
# Stratified by Age 10y, Gender, Calendar Year
if (runIncidence | runPrevalence) {
  tic(msg = "Incidence Prevalence Sampling + Denominator")
  cli::cli_bullets(c("*" = "{.strong Getting denominator for incidence prevalence }"))
  if (is.null(sampleIncidencePrevalence)) {
    cdmSampled <- cdm 
  } else{
    cdmSampled <- cdmSample(cdm, n = sampleIncidencePrevalence)
  }
  cdmSampled <- generateDenominatorCohortSet(
    cdm = cdmSampled, 
    name = "denominator", 
    ageGroup =  lapply(as.list(1:length(seq(0, 110, 10))), function(k, x1 = seq(0, 110, 10), x2 = seq(9, 120, 10)) {c(x1[k], x2[k])}),
    sex = c("Male", "Female", "Both"),
    daysPriorObservation = 180
  )
  toc(log = TRUE)
}

if (runIncidence ) {
  tic(msg = "Incidence by year, age, sex")
  cli::cli_bullets(c("*" = "{.strong Estimating incidence}"))
  output$incidence <- estimateIncidence(
    cdm = cdmSampled,
    denominatorTable = "denominator",
    outcomeTable = cohort_table,
    interval = "years",
    repeatedEvents = FALSE,
    outcomeWashout = Inf,
    completeDatabaseIntervals = FALSE,
    minCellCount = 5
  ) 
  toc(log = TRUE)
}

if (runPrevalence ) {
  tic(msg = "Prevalence by year, age, sex")
  cli::cli_bullets(c("*" = "{.strong Estimating prevalence}"))
  output$prevalence <- estimatePeriodPrevalence(
    cdm = cdmSampled,
    denominatorTable = "denominator",
    outcomeTable = cohort_table,
    interval = "years",
    completeDatabaseIntervals = TRUE,
    fullContribution = FALSE,
    minCellCount = 5
  )
  toc(log = TRUE)
}

if (dropCohortTable) {
  tic(msg = "Drop tables")
  cli::cli_bullets(c("*" = "{.strong Dropping tables}"))
  cdm <- dropTable(cdm, starts_with(cohort_table))
  toc(log = TRUE)
}

# Log close ----
tic_log <- tic.log(format = TRUE)
output$log <- tibble(cdm_name = cdmName(cdm), 
                     log = paste0(tic_log |>  unlist(), collapse = "\n"))

# Export results ----
id <- paste0(study_prefix, "_", cohort_table, "_", cdmName(cdm), "_", gsub("-", "", Sys.Date()))
output_folder <- basename(here(id))
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# csv 
summarised_results <- c("cohort_details", "cohort_overlap", "characteristics", "orphan_codes", "index_events",
                        "code_counts", "lsc_sample", "lsc_matched", "lsc_difference", "snapshot", "cohort_timing")
# summarised results
overallSR <- omopgenerics::bind(output[summarised_results])
overallSR <- overallSR |> 
  omopgenerics::newSummarisedResult(
    settings = settings(overallSR) |> mutate(study_prefix = study_prefix, cohort_table = cohort_table)
  ) 
overallSR <- overallSR |>
  omopgenerics::exportSummarisedResult(minCellCount = 5, fileName = paste0("sr_", id, ".csv"), path = output_folder)
# others
for (result in names(output[!names(output) %in% summarised_results])) {
  if (!is.null(output[[result]])) {
    write_csv(output[[result]], file = here(output_folder, paste0(result, ".csv")))
  }
}
# zip 
zip(
  zipfile = paste0(id, ".zip"),
  files = list.files(output_folder, full.names = TRUE, pattern = ".csv"),
  flags = '-r9Xj'
)

cli::cli_alert_success( "PhenotypeR finished!")
