# Get data ----
# All potential outputs
result_names <- c("default" = "cohort_definitions", 
                  "default" = "cohort_count", 
                  "runCountCodes" = "code_counts", 
                  "runCalculateOverlap" = "cohort_overlap", 
                  "runProfiling" = "age_distribution", 
                  "runProfiling" = "time_distribution", 
                  "runPrevalence" = "prevalence", 
                  "runIncidence" = "incidence", 
                  "runIndexEvents" = "index_events", 
                  "runMatchedSampleLSC" = "lsc_sample", 
                  "runMatchedSampleLSC" = "lsc_matched", 
                  "runMatchedSampleLSC" = "lsc_difference", 
                  "default" = "log",
                  "default" = "snapshot")
job_names <- unique(names(result_names))

# Result files
result_files <- list.files(path = here(dataFolder), pattern = ".RData")

# Read files and join equal outputs
data <- vector("list", length(result_names)) |> setNames(result_names)
settings <- NULL
for (file in result_files) {
  x <- load(here(dataFolder, file))
  settings <- settings %>%  union_all(bind_rows(input))
  for (jobName in job_names) {
    if (jobName == "default" || input[[jobName]]) {
      for (outputName in result_names[names(result_names) %in% jobName]) {
        eval(
          parse(
            text = 
              paste0("data$", outputName, " <- data$", outputName, 
                     " %>% union_all(output$", outputName, ")")
            )
        )
      }
    }
  }
  rm(list = x)
}
rm(x)

# Tranform data for shiny ----
# Orphan code counts
if (any(settings$runCountCodes)) {
  data$orphan_counts <- data$code_counts %>% 
    filter(strata_name == "recomendation") %>% 
    ungroup()  %>% 
    distinct() %>% 
    mutate(standard_concept_name = substr(additional_level, 1,
                                          unlist(gregexpr(';', additional_level))-2)) %>% 
    pivot_wider(names_from = variable_name, values_from = estimate_value) %>% 
    select("cdm_name", "cohort", "relationship_id",
           "standard_concept_id", "standard_concept_name", "Record count", "Person count")
  # Code counts
  data$code_counts <- data$code_counts %>% 
    filter(strata_name == "original_codes") %>% 
    ungroup()  %>% 
    distinct() %>% 
    mutate(standard_concept_name = substr(additional_level, 1,
                                          unlist(gregexpr(';', additional_level))-2)) %>% 
    pivot_wider(names_from = variable_name, values_from = estimate_value) %>% 
    select("cdm_name", "cohort", 
           "standard_concept_id", "standard_concept_name", "Record count", "Person count")
}
# Index events
if (any(settings$runIndexEvents)) {
  data$index_events <- data$index_events %>% 
    pivot_wider(names_from = variable_name, values_from = estimate) %>% 
    select(cdm_name, cohort_name, codelist_name, group_name, group_level, standard_concept_id, standard_concept_name, 
           source_concept_name, source_concept_id, domain_id,  
           cdm_name, `Record count`, `Person count`)
}
# Cohort overlap
if (any(settings$runCalculateOverlap)) {
  data$cohort_overlap <- data$cohort_overlap %>%
    ungroup() %>%
    inner_join(data$cohort_count %>%
                 select(cdm_name,
                        cohort_definition_id_x = cohort_definition_id,
                        cohort_name_x = cohort_name,
                        subject_counts_x = number_subjects),
               by = c("cdm_name", "cohort_definition_id_x")) %>%
    inner_join(data$cohort_count %>%
                 select(cdm_name,
                        cohort_definition_id_y = cohort_definition_id,
                        cohort_name_y = cohort_name,
                        subject_counts_y = number_subjects),
               by = c("cdm_name", "cohort_definition_id_y")) %>%
    mutate(
      intersect_counts = as.integer(intersect_count)) %>%
    select(-intersect_count)
}
# Profiling
if (any(settings$runProfiling)) {
  # Age distribution
  data$age_distribution <- data$age_distribution %>%
    ungroup() %>%
    inner_join(data$cohort_count %>% select(cdm_name, cohort_definition_id, cohort_name)) %>%
    select(-cohort_definition_id)
  # Time distribution
  to_pivot <- colnames(data$time_distribution)[!colnames(data$time_distribution) %in%
                                                 c("sex", "cohort_name", "cdm_name", "cohort_definition_id")]
  data$time_distribution <- tibble(covariate = c("age", "prior_observation", "future_observation")) %>%
    left_join(
      data$time_distribution %>%
        ungroup() %>%
        inner_join(data$cohort_count %>% select(cdm_name, cohort_definition_id, cohort_name)) %>%
        select(-cohort_definition_id) %>%
        mutate(across(tidyr::everything(), ~ as.character(.x))) %>%
        pivot_longer(cols = to_pivot, values_to = "estimate_value") %>%
        mutate(
          estimate_type = case_when(
            grepl("Min", name) ~ "min",
            grepl("Max", name) ~ "max",
            grepl("Median", name) ~ "median",
            grepl("Mean", name) ~ "mean",
            grepl("Sd", name) ~ "sd",
          ),
          covariate = gsub("_Min|_Max|_Median|_Sd|_Mean", "", name),
          estimate_value = niceNum(estimate_value, 3)
        ) %>%
        select(-name)
    ) %>%
    select(cdm_name, cohort_name, sex, covariate, estimate_type, estimate_value)
}
# LSC
if (any(settings$runMatchedSampleLSC)) {
  data$lsc_table <- data$lsc_matched %>% 
    mutate(
      estimate_type = paste0("matched_", estimate_type),
      estimate = as.numeric(estimate)
    ) %>% 
    pivot_wider(names_from = estimate_type, values_from = estimate) %>% 
    left_join(
      data$lsc_sample %>% 
        mutate(
          estimate_type = paste0("sample_", estimate_type),
          estimate = as.numeric(estimate)
        ) %>% 
        pivot_wider(names_from = estimate_type, values_from = estimate)) %>% 
    mutate(
      difference_count = (sample_count - matched_count)/matched_count,
      difference_percentage = (sample_percentage - matched_percentage)/matched_percentage
    ) %>% 
    select(
      cdm_name, cohort_name = group_level, table_name, concept, concept_name = variable, 
      window = variable_level, matched_count, matched_percentage, sample_count, sample_percentage, 
      difference_count, difference_percentage
    )
}

# Shiny theme ----
DUtheme <- create_theme(
  adminlte_color(
    light_blue = "#0c0e0c" 
  ),
  adminlte_sidebar(
    # width = "400px",
    dark_bg = "#78B7C5",
    dark_hover_bg = "#3B9AB2",
    dark_color = "white"
  ), 
  adminlte_global(
    content_bg = "#eaebea" 
  ),
  adminlte_vars(
    border_color = "#112446",
    active_link_hover_bg = "#FFF",
    active_link_hover_color = "#112446",
    active_link_hover_border_color = "#112446",
    link_hover_border_color = "#112446"
  )
)
