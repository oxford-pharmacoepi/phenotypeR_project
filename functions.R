addStudyToSettings <- function(x, studyPrefix, cohortTable) {
  newCols <- x |>
    distinct(result_id) |>
    dplyr::mutate(
      "study_prefix" = studyPrefix,
      "cohort_table" = cohortTable
    ) 
  x |>
    dplyr::left_join(newCols, by = "result_id") |>
    omopgenerics::newSummarisedResult()
}