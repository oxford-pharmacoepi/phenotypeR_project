cohorts_name <- "phenotyper_inc_pharm"
cdm <- cdm_from_con(con = db,
                         cdm_schema = "public",
                         write_schema = "results",
                         cohort_tables = cohorts_name)

cdm[[cohorts_name]] %>% glimpse()


cohort_set(cdm[[cohorts_name]] )
