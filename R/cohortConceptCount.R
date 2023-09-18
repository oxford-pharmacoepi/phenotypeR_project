#' `cohortConceptCount()` generates concept_counts
#'
#' @param cdm a CDM object
#' @import CDMConnector DBI dbplyr dplyr
#'
#' @return A CDM table
#' @export
cohortConceptCount <- function(cdm) {
  concept_counts_db <- cdm$condition_occurrence %>%
    group_by(condition_concept_id) %>%
    summarize(
      concept_id = condition_concept_id,
      concept_count = n(),
      concept_subjects = n_distinct(person_id)) %>%
    dplyr::union_all(
      cdm$condition_occurrence %>%
        group_by(condition_source_concept_id) %>%
        summarize(concept_id = condition_source_concept_id,
                  concept_count = n(),
                  concept_subjects = n_distinct(person_id))) %>%
    dplyr::union_all(
      cdm$drug_exposure %>%
        group_by(drug_concept_id) %>%
        summarize(
          concept_id = drug_concept_id,
          concept_count = n(),
          concept_subjects = n_distinct(person_id)
        )
    ) %>%
    dplyr::union_all(
      cdm$drug_exposure %>%
        group_by(drug_source_concept_id) %>%
        summarize(
          concept_id = drug_source_concept_id,
          concept_count = n(),
          concept_subjects = n_distinct(person_id)
        )
    ) %>%
    dplyr::union_all(
      cdm$procedure_occurrence %>%
        group_by(procedure_concept_id) %>%
        summarize(
          concept_id = procedure_concept_id,
          concept_count = n(),
          concept_subjects = n_distinct(person_id)
        )
    ) %>%
    dplyr::union_all(
      cdm$procedure_occurrence %>%
        group_by(procedure_source_concept_id) %>%
        summarize(
          concept_id = procedure_source_concept_id,
          concept_count = n(),
          concept_subjects = n_distinct(person_id)
        )
    ) %>%
    dplyr::union_all(
      cdm$measurement %>%
        group_by(measurement_concept_id) %>%
        summarize(
          concept_id = measurement_concept_id,
          concept_count = n(),
          concept_subjects = n_distinct(person_id)
        )
    ) %>%
    dplyr::union_all(
      cdm$measurement %>%
        group_by(measurement_source_concept_id) %>%
        summarize(
          concept_id = measurement_source_concept_id,
          concept_count = n(),
          concept_subjects = n_distinct(person_id)
        )
    ) %>%
    dplyr::union_all(
      cdm$observation %>%
        group_by(observation_concept_id) %>%
        summarize(
          concept_id = observation_concept_id,
          concept_count = n(),
          concept_subjects = n_distinct(person_id)
        )
    ) %>%
    dplyr::union_all(
      cdm$observation %>%
        group_by(observation_source_concept_id) %>%
        summarize(
          concept_id = observation_source_concept_id,
          concept_count = n(),
          concept_subjects = n_distinct(person_id)
        )
    )
return(concept_counts_db)
}


