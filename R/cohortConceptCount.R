connectionDetails <- Eunomia::getEunomiaConnectionDetails()

cdmDatabaseSchema <- "main"
cohortDatabaseSchema <- "main"


CohortDiagnostics::createConceptCountsTable(connectionDetails = connectionDetails,
                                            connection = NULL,
                                            cdmDatabaseSchema = cdmDatabaseSchema,
                                            tempEmulationSchema = NULL,
                                            conceptCountsDatabaseSchema = cohortDatabaseSchema,
                                            conceptCountsTable = "concept_counts",
                                            conceptCountsTableIsTemp = FALSE,
                                            removeCurrentTable = TRUE)

connection <- DatabaseConnector::connect(connectionDetails)
concept_counts <- querySql(connection, "SELECT * FROM main.concept_counts")

class(concept_counts)


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

concept_counts_db <- as.data.frame(concept_counts_db) %>%
  select(concept_id, concept_count, concept_subjects)

class(concept_counts_db)

class(concept_counts)


concept_counts_db
concept_counts

length(concept_counts_db$concept_id)
length(concept_counts$CONCEPT_ID)

unique(concept_counts_db$concept_id)
unique(concept_counts$CONCEPT_ID)

concept_counts_db$concept_id %in% concept_counts$CONCEPT_ID

concept_counts_db$concept_count  %in% concept_counts$CONCEPT_COUNT

concept_counts_db$concept_subjects %in% concept_counts$CONCEPT_SUBJECTS 

sum(concept_counts_db$concept_id) == sum(concept_counts$CONCEPT_ID)

concept_counts_db$concept_count  == concept_counts$CONCEPT_COUNT

concept_counts_db$concept_subjects == concept_counts$CONCEPT_SUBJECTS 

intersect(concept_counts_db$concept_id, concept_counts$CONCEPT_ID)
unique(concept_counts_db$concept_id)
setdiff(concept_counts_db$concept_id, concept_counts$CONCEPT_ID)

