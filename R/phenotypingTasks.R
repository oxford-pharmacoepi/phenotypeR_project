#' `cohortOverlap()` Percentages and counts: Counts only for now, percentages easy. May want to add names to cohorts
#'
#' @param cdm A cdm object
#' @param cohorts_name Cohort name
#'  
#' @import CDMConnector
#' @return A tibble with a summary of intersections.
#' @export
cohortOverlap <- function(cdm,
                          cohorts_name) {
  summary_by_group <- cdm[[cohorts_name]] %>%
    group_by(cohort_definition_id) %>%
    summarize(ids_in_group = n()) %>% 
    collect()
  
  # Join to get the IDs that intersect between groups
  summary_intersections <- cdm[[cohorts_name]] %>%
    inner_join(cdm[[cohorts_name]], by = "subject_id") %>%
    filter(cohort_definition_id.x != cohort_definition_id.y) %>%
    select(subject_id, cohort_definition_id_x = cohort_definition_id.x, cohort_definition_id_y = cohort_definition_id.y) %>%
    distinct()  %>%
    group_by(cohort_definition_id_x, cohort_definition_id_y) %>%
    summarize(intersect_count = n()) %>% 
    collect()
  
  return(summary_intersections)
}

#' `orphanCodes()`Details, Cohort Count ,  Cohort definition, Concept Sets, JSON, SQL
#' TO DO: Ideally separate all steps - more loops but less mess
#' 2,3 - now use the precounted table + Phoebe recommendations
#' 2,3 - We could add options to use getcodeuse from CodelsitGenerator and the Codelist generator search itself
#' TO DO: Need to improve metadata in names of concept sets and names of cohorts
#' TO DO: Need to add  Source field and Standard fields
#'
#' @return
#' @export
#'
#' @examples
orphanCodes <- function(cdm,
                        cohort_set,
                        concept_counts) {
  
tic(msg = "Orphan codes + markdown readable text for only first cohort")
cohort_set_res <- cohort_set
cohort_set_res$markdown <- ""
counts_table <- concept_counts
# counts_table <- dbFetch(counts_table)

code_counts <- tibble()

for (n in  row_number(cohort_set_res) ) {
  
  cohort <- cohort_set_res$cohort_name[n]  
  json <- paste0(cohort_set_res$json[n]  )
  cohortExpresion <- CirceR::cohortExpressionFromJson(json)
  markdown <- CirceR::cohortPrintFriendly(cohortExpresion)
  cohort_set_res$markdown[n] <-  markdown
  
  ### Ideally reads the same JSON character line
  json2 <- jsonlite::read_json(file.path(cohort_json_dir, paste0(cohort, ".json")))
  codes <- CodelistGenerator::codesFromCohort(file.path(cohort_json_dir, paste0(cohort, ".json")), 
                                              cdm, 
                                              withConceptDetails = FALSE)
  
  code_counts_2 <- tibble()
  
  for (code_list in codes) {
    # code_list <- codes[[1]]
    codes_id <- code_list
    
    recommended_codes <- concept_recommended %>% 
      filter(concept_id_1 %in% codes_id ) %>% 
      filter(!concept_id_2 %in% codes_id) %>% 
      distinct(concept_id_2, .keep_all = TRUE)
    
    recommended_codes_counts <- recommended_codes %>%
      left_join( counts_table, join_by(concept_id_2 == concept_id) )  %>%
      rename( concept_id=concept_id_2) %>%
      filter(!is.na(concept_count))%>%
      mutate(type="recommendation", cohort=cohort)
    
    
    original_codes_counts <- counts_table %>% filter(concept_id %in% codes_id) %>%
      mutate(type="original_codes", cohort=cohort, relationship_id="original", concept_id_1=concept_id  )
    
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

}


#' `timeDistributions()` observation time (days) after index , observation time (days) prior to index, time (days) between cohort start and end. Need to add better characterisation of demographics (a sort of table 1)
#'
#' @param cdm 
#' @param cohorts_name 
#'
#' @return
#' @export
timeDistributions <- function(cdm,
                              cohorts_name) {
  cdm$results_dx <- cdm[[cohorts_name]]
  
  Patient_profiles <- cdm$results_dx %>%
    PatientProfiles::addDemographics(cdm) %>% 
    collect()   %>%
    mutate(age_group= cut(age, c(seq(0, 110, 5 ), Inf), include.lowest=TRUE))
  
  Age_distribution <- Patient_profiles %>% group_by(cohort_definition_id, age_group, sex) %>% tally()
  
  Time_distribution <- Patient_profiles %>%
    group_by(cohort_definition_id, sex) %>% 
    summarise_at(vars(age, prior_observation, future_observation), list(Min = min, Mean = mean, Median = median,  Max = max, Sd = sd)) %>%
    collect()
  
  rm(Patient_profiles)
  return(Time_distribution)
}



#' Cohort Characterisation : Large scale + temporal + differneces. Missing differences between them that can be done in shiny step - Also demographics that can be done in previous steps. 8 - Visit Context  ###########
#' Low priority: tipe of visits Before, during, simultaneous, after
#' Could potentially be extracted from large scale ?
#'
#' @param cdm 
#' @param cohorts_name 
#'
#' @return
#' @export
#'
#' @examples
largeScaleCharacterization <- function(cdm, cohorts_name) {
  
  largeScaleCharacterization <- PatientProfiles::summariseLargeScaleCharacteristics(cohort=cdm[[cohorts_name]],
                                                                                    window = list(c(-Inf, -366), c(-365, -31), c(-30, -1), 
                                                                                                  c(0, 0), 
                                                                                                  c(1, 30), c(31, 365),  c(366, Inf)),
  # window =list(c(0, 0)),
  tablesToCharacterize = c("condition_occurrence", "drug_era", "visit_occurrence",
                           "measurement", "procedure_occurrence",  "observation"), 
  # Further options:
  #  "drug_exposure", 
  #  "device_exposure",  
  #   "condition_era", 
  # "specimen"),
  
  overlap = TRUE,
  minCellCount = 5
)
  return(largeScaleCharacterization)
}
