# Log start ------
tic.clearlog()
tic.clear()
tic(msg = "phenotypeR total time run: ")

# Set-up: directories and settings ------
tic(msg = "Settings and loading of Phoebe")

# To export output 
result_names <- c("cohort_definitions", "cohort_details", "code_counts", "cohort_overlap", 
                  "age_distribution", "time_distribution", "prevalence", "incidence", 
                  "index_events", "lsc_sample", "lsc_matched", "lsc_difference", "log",
                  "snapshot", "orphan_codes")
output <- data <- vector("list", length(result_names)) |> setNames(result_names)

# Concept recommended
if (runCountCodes & !file.exists(here("Phoebe", "concept_recommended.csv")) ){
  if (file.exists(here("Phoebe", zipFileName))) {
    unzip(zipfile = here("Phoebe", zipFileName), exdir = here("Phoebe"))
    concept_recommended <- read_csv(here("Phoebe", "concept_recommended.csv"),
                                    show_col_types = FALSE)
  } else { 
    runCountCodes <- FALSE
    warning(paste0("`runCountCodes` set to FALSE because there isn't the zip file `", zipFileName, "`"))
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
  cdm <- cdm_from_con(con = db,
                      cdm_schema = c(schema = cdm_schema),
                      write_schema = c(schema= write_schema, prefix = study_prefix),
                      achilles_schema = achilles_schema, 
                      cdm_name = db_name
  )
} else   {
  cdm <- cdm_from_con(
    con = db,
    cdm_schema = c(schema = cdm_schema),
    write_schema = c(schema= write_schema, prefix = study_prefix),
    achilles_schema = achilles_schema,
    cohort_tables = cohort_table_name, 
    cdm_name = db_name  
  )
  
}
toc(log = TRUE)

# Get cdm snapshot -----
tic(msg = "Getting cdm snapshot")
output$snapshot <- summary(cdm)
toc(log = TRUE)

# Step 1: Generate cohorts

if (runGenerateCohort) {
  tic(msg = "Generate Cohort Set")
  cdm <-   generateCohortSet(cdm, 
                             jsons,
                             name = cohort_table,
                             computeAttrition = TRUE,
                             overwrite = TRUE)
  toc(log = TRUE)
}



# Cohort details ----
tic(msg = "Cohort details")
output$cohort_details <- summary(cdm[[cohort_table]])

toc(log = TRUE)

# Cohort Overlap ----
if (runCalculateOverlap) {
  tic(msg = "Calculate Overlap")
  output$cohort_overlap <- summariseCohortOverlap(cdm[[cohort_table]])
  toc(log = TRUE)
}

# Cohort definitions ----
tic(msg = "Markdown readable text")
output$cohort_definitions <- jsons |>
  rowwise() |>
  mutate(
    markdown = cohortPrintFriendly(cohortExpressionFromJson(.data$json)),
    cdm_name = cdmName(cdm)
  )
toc(log = TRUE)

# Code counts ----
if(runCountCodes) {
  tic(msg = "Code counts and orphan code counts")
  codes <- codesFromCohort(here("Cohorts"), cdm, withConceptDetails = F)
  # original codes
  output$code_counts <- achillesCodeUse(codes, cdm = cdm) 
  # orphan codes
  orphanCodelist <- lapply(codes, function(codelist, phoebe = concept_recommended) {
    phoebe |>
      filter(concept_id_1 %in% codelist) |>  
      filter(!concept_id_2 %in% codelist) |>  
      distinct(concept_id_2, .keep_all = TRUE) |> 
      pull("concept_id_2")
  })
  output$orphan_codes <- achillesCodeUse(orphanCodelist, cdm = cdm) |>
    left_join(
      concept_recommended |>
        mutate(across(starts_with("concept"), ~ as.character(.x))) |>
        select("original_code" = "concept_id_1", "group_level" = "concept_id_2", "relationship_id"),
      by = "group_level"
    ) |>
    # TODO: "original_code", "relationship_id" in additional when CodelistGenerator issue #125
    # splitAdditional() |>
    # uniteAdditional(cols = c(additionalColumns(originalCodes), "original_code", "relationship_id")) |>
    mutate(result_type = "recommended_codes_achilles_code_use")
  toc(log = TRUE)
}

# Index Event ----
if (runIndexEvents) {
  tic(msg = "Index Event Breakdown")
  cohortSet <- settings(cdm[[cohort_table]])
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
  output$index_events <- index_events |> bind_rows()
  file.remove(here("Phoebe", "concept_recommended.csv"))
  toc(log = TRUE)
}


# Step 4: Time Distributions ----
# observation time (days) after index , observation time (days) prior to index, time (days) between cohort start and end
# TODO: need to add better characterisation of demographics (a sort of table 1)
if (runProfiling) {
  tic(msg = "Patient_profiles summary")
  cohortDem <- cdm[[cohort_table]] |>
    addDemographics(
      ageGroup = lapply(as.list(1:length(seq(0, 110, 5))), function(k, x1 = seq(0, 110, 5), x2 = seq(4, 120, 5)) {c(x1[k], x2[k])})
    ) |>
    addCohortName() |>
    mutate(cdm_name := !!cdmName(cdm)) |>
    compute()
  output$age_distribution <- cohortDem |>
    group_by(cdm_name, cohort_name, age_group, sex) |>
    tally() |>
    collect()
  output$time_distribution <- cohortDem |>
    group_by(cdm_name, cohort_name, sex) |> 
    summarise_at(vars(age, prior_observation, future_observation), list(Min = min, Mean = mean, Median = median,  Max = max, Sd = sd)) |>
    collect()
  toc(log = TRUE)
}

# Cohort Characterisation : Large scale + temporal + differneces  & matching ----
# NEW:  It could include matching cohorts                                     
# Missing differences between them that can be done in shiny step - Also demographics that can be done in previous steps
#  We can get also Visit Context here 
# Low priority: tipe of visits Before, during, simultaneous, after
# Could potentially be extracted from large scale ?
# TODO: use macthCohorts when ready
if (runMatchedSampleLSC) {
  tic(msg = "Generate 1K Sample and Matched sample")
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
  lsc_matched <- summariseLargeScaleCharacteristics(
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
  lsc_sample <- summariseLargeScaleCharacteristics(
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
  output$lsc_difference <- lsc_matched |>
    splitGroup() |>
    rename("cohort_name_reference" = "cohort_name") |> 
    pivotSettings() |>
    mutate(estimate_name = paste0(estimate_name, "_reference"),
           estimate_value = as.numeric(estimate_value)) |>
    select(-"estimate_type") |>
    pivot_wider(names_from = estimate_name, values_from = estimate_value) |>
    left_join(
      lsc_sample |>
        splitGroup() |>
        rename("cohort_name_comparator" = "cohort_name") |> 
        pivotSettings() |>
        mutate(estimate_name = paste0(estimate_name, "_comparator"),
               estimate_value = as.numeric(estimate_value)) |>
        select(-"estimate_type") |>
        pivot_wider(names_from = estimate_name, values_from = estimate_value),
      by = join_by(result_id, cdm_name, result_type, package_name, package_version, strata_name,
                   strata_level, variable_name, variable_level, additional_name, additional_level,
                   table_name, type,  analysis),
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
    mutate(estimate_type = "numeric") |>
    select(-starts_with("percentage"), -starts_with("count")) |>
    appendSettings(colsSettings = c("table_name", "type", "analysis")) 
  toc(log = TRUE)
  # to export
  output$lsc_matched <- lsc_matched |>
    omopgenerics::suppress() |>
    mutate(group_name = if_else(group_name == "cohort_name", "cohort_name_reference", group_name)) 
  output$lsc_sample <- lsc_sample |>
    omopgenerics::suppress() |>
    mutate(group_name = if_else(group_name == "cohort_name", "cohort_name_comparator", group_name)) 
}


# Incidence Rates ----
# Stratified by Age 10y, Gender, Calendar Year
# For now stratified by kid-Adult-Older Adult 
if (runIncidence | runPrevalence) {
  tic(msg = "Incidence Prevalence Sampling + Denominator")
  if (is.null(sampleIncidencePrevalence)) {
    cdmSampled <- cdm 
  } else{
    cdmSampled <- cdmSample(cdm, n = sampleIncidencePrevalence)
  }
  cdmSampled <- generateDenominatorCohortSet(
    cdm = cdmSampled, 
    name = "denominator", 
    ageGroup = list(c(0,17), c(18,64), c(65,199)),
    sex = c("Male", "Female", "Both"),
    daysPriorObservation = 180
  )
  toc(log = TRUE)
}

if (runIncidence ) {
  tic(msg = "Incidence by year, age, sex")
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


# Log close ----
toc(log = TRUE)
tic.log(format = TRUE)
tic_log <- tic.log(format = TRUE)
output$log <- tibble(cdm_name = cdmName(cdm), 
                     log = paste0(tic_log |>  unlist(), collapse = "\n"))

# Export results ---
output_folder <- basename(here("ResultsPhenotypeR"))
# csv 
summarised_results <- c("cohort_details", "code_counts", "cohort_overlap",
                        "lsc_sample", "lsc_matched", "lsc_difference", "snapshot")
# summarised results
output[summarised_results] |> bind_rows() |>
  write_csv(file = here(output_folder, "summarised_phenotypeR_results.csv"))
# others
for (result in names(output[!names(output) %in% summarised_results])) {
  write_csv(output[[result]], file = here(output_folder, paste0(result, ".csv")))
}
# zip 
zip(
  zipfile = paste0(output_folder, "_", cdmName(cdm), "_", format(Sys.time(), "%d%m%Y"), ".zip"),
  files = list.files(output_folder, full.names = TRUE, pattern = ".csv"),
  flags = '-r9Xj'
)

