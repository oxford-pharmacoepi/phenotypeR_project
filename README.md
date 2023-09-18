
<!-- README.md is generated from README.Rmd. Please edit that file -->

# phenotypeProject

<!-- badges: start -->
<!-- badges: end -->

Project to start developing fast phenotyping tools

## Installation

You can install the development version of phenotypeProject from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("oxford-pharmacoepi/phenotypeProject")
```

#### Example

Create a connection with CDMConnector

``` r
db <- DBI::dbConnect(duckdb::duckdb(), CDMConnector::eunomia_dir())
cdm <- CDMConnector::cdm_from_con(con = db,
                                  cdm_schema = "main",
                                  write_schema = "main")
```

#### Define variables

``` r
cohort_json_dir <- here::here(system.file("cohorts", package = "CohortDiagnostics"))
cohorts_name <- "gi_bleed"
concept_recommended <- read.csv(here::here("Phoebe/concept_recommended.csv"))
```

##### Get cohorts and generate them now from json, but we can do with CapR other sources

``` r
cohort_set <- CDMConnector::read_cohort_set(cohort_json_dir)

cdm <- CDMConnector::generateCohortSet(cdm, 
                                       cohort_set,
                                       name = cohorts_name,
                                       computeAttrition = TRUE,
                                       overwrite = TRUE)
```

#### Concept counts table

``` r
concept_counts <- phenotypeProject::cohortConceptCount(cdm)
#> Warning: replacing previous import 'CDMConnector::in_schema' by
#> 'dbplyr::in_schema' when loading 'phenotypeProject'
#> Warning: replacing previous import 'dbplyr::ident' by 'dplyr::ident' when
#> loading 'phenotypeProject'
#> Warning: replacing previous import 'dbplyr::sql' by 'dplyr::sql' when loading
#> 'phenotypeProject'
concept_counts
#> # Source:   SQL [?? x 13]
#> # Database: DuckDB 0.7.1 [cbarboza@Windows 10 x64:R 4.2.1/C:\Users\cbarboza\AppData\Local\Temp\RtmpSKhYvA\file61801a347faa.duckdb]
#>    condition_concept_id concept_id concept_count concept_subjects
#>                   <int>      <int>         <dbl>            <dbl>
#>  1              4112343    4112343         10217             2606
#>  2               192671     192671           479              479
#>  3                28060      28060          2656             1677
#>  4               378001     378001          1013              852
#>  5               257012     257012           825              812
#>  6              4134304    4134304           493              442
#>  7             40481087   40481087         17268             2686
#>  8              4113008    4113008           500              462
#>  9               372328     372328          3605             2025
#> 10               260139     260139          8184             2543
#> # ℹ more rows
#> # ℹ 9 more variables: condition_source_concept_id <int>, drug_concept_id <int>,
#> #   drug_source_concept_id <int>, procedure_concept_id <int>,
#> #   procedure_source_concept_id <int>, measurement_concept_id <int>,
#> #   measurement_source_concept_id <int>, observation_concept_id <int>,
#> #   observation_source_concept_id <int>
```

#### Cohort Overlap (Subjects Percentages and counts: Counts only for now, percentages easy May want to add names to cohorts)

``` r
phenotypeProject::cohortOverlap(cdm = cdm, cohorts_name = cohorts_name)
#> # A tibble: 6 × 3
#> # Groups:   cohort_definition_id_x [3]
#>   cohort_definition_id_x cohort_definition_id_y intersect_count
#>                    <int>                  <int>           <dbl>
#> 1                      1                      2             479
#> 2                      1                      3             479
#> 3                      2                      1             479
#> 4                      2                      3             479
#> 5                      3                      1             479
#> 6                      3                      2             479
```

#### Time distributions observation time (days) after index, observation time (days) prior to index, time (days) between cohort start and end Need to add better characterisation of demographics (a sort of table 1)

``` r
phenotypeProject::timeDistributions(cdm = cdm, cohorts_name = cohorts_name)
#> # A tibble: 6 × 17
#> # Groups:   cohort_definition_id [3]
#>   cohort_definition_id sex    age_Min prior_observation_Min
#>                  <int> <chr>    <dbl>                 <dbl>
#> 1                    1 Female      31                 11572
#> 2                    1 Male        32                 11938
#> 3                    2 Female      31                 11572
#> 4                    2 Male        32                 11938
#> 5                    3 Female      31                 11572
#> 6                    3 Male        32                 11938
#> # ℹ 13 more variables: future_observation_Min <dbl>, age_Mean <dbl>,
#> #   prior_observation_Mean <dbl>, future_observation_Mean <dbl>,
#> #   age_Median <dbl>, prior_observation_Median <dbl>,
#> #   future_observation_Median <dbl>, age_Max <dbl>,
#> #   prior_observation_Max <dbl>, future_observation_Max <dbl>, age_Sd <dbl>,
#> #   prior_observation_Sd <dbl>, future_observation_Sd <dbl>
```

#### Cohort Characterisation : Large scale + temporal + differneces. Missing differences between them that can be done in shiny step - Also demographics that can be done in previous steps. 8 - Visit Context

Low priority: tipe of visits Before, during, simultaneous, after Could
potentially be extracted from large scale ?

``` r
phenotypeProject::largeScaleCharacterization(cdm = cdm, cohorts_name = cohorts_name)
```

#### Incidence

``` r
cdm <- IncidencePrevalence::generateDenominatorCohortSet(
  cdm = cdm, 
  name = "denominator", 
  cohortDateRange = NULL,
  ageGroup = list(c(0,17), c(18,64),
                  c(65,199)),
  sex = c("Male", "Female", "Both"),
  daysPriorHistory = 180
)

inc <- IncidencePrevalence::estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = cohorts_name,
  interval = "years",
  repeatedEvents = FALSE,
  outcomeWashout = Inf,
  completeDatabaseIntervals = FALSE,
  minCellCount = 0 )
```
