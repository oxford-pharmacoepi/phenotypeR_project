ui <- dashboardPage(
  dashboardHeader(title = "PhenotypeR"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Background",
        tabName = "background"
      ),
      # menuItem(
      #   text = "Databases",
      #   tabName = "cdm_snapshot"
      # ),
      menuItem(
        text = "Cohorts",
        tabName = "cohorts"
      ),
      menuItem(
        text = "Cohort counts",
        tabName = "counts"
      ),
      if (any(settings$runCountCodes)) {
        menuItem(
          text = "Code counts",
          tabName = "code_counts"
        )
      },
      if (any(settings$runCountCodes)) {
        menuItem(
          text = "Orphan codes",
          tabName = "orphan"
        )
      },
      if (any(settings$runIndexEvents)) {
        menuItem(
          text = "Index events",
          tabName = "index"
        )
      },
      if (any(settings$runCalculateOverlap)) {
        menuItem(
          text = "Cohort overlap",
          tabName = "overlap"
        )
      },
      if (any(settings$runProfiling)) {
        menuItem(
          text = "Age distribution",
          tabName = "age"
        )
        menuItem(
          text = "Time & age distribution",
          tabName = "time"
        )
      },
      if (any(settings$runPrevalence)) {
        menuItem(
          text = "Population prevalence",
          tabName = "prevalence"
        )
      },
      if (any(settings$runIncidence)) {
        menuItem(
          text = "Population incidence",
          tabName = "incidence"
        )
      },
      if (any(settings$runMatchedSampleLSC)) {
        menuItem(
          text = "Large sclae characterisation",
          tabName = "large_scale_characterisation"
        )
      },
      menuItem(
        text = "Execution log",
        tabName = "log"
      )
    )
  ),
  
  # ## body ----
  dashboardBody(
    use_theme(DUtheme),
    tabItems(
      # background  ------
      tabItem(
        tabName = "background",
        h3("PhenotypeR"),
        h5("https://github.com/oxford-pharmacoepi/phenotypeR_project"),
      ),
      # cdm snapshot ------
      # tabItem(
      #   tabName = "cdm_snapshot",
      #   h4("Information about the databases."),
      # selectors(cdmSnapshot, "cdm_snapshot", c("cdm_name")),
      # downloadButton("cdm_snapshot_tidy_download_word", "Download word"),
      # downloadButton("cdm_snapshot_tidy_download_csv", "Download csv"),
      # DTOutput("cdm_snapshot_tidy") %>% withSpinner()
      # ),
      # cohort definition ------
      tabItem(
        tabName = "cohorts",
        # h4("Cohort definitions."),
        selectors(data$cohort_definitions, "definitions", c("cdm_name", "cohort_name"), multiple = FALSE, default = list()),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Cohort definition",
            uiOutput("markdown")
          ),
          tabPanel(
            "JSON",
            h4(),
            rclipboardSetup(),
            uiOutput("clip"),
            verbatimTextOutput("verb"),
          )
          # tabPanel(
          #   "Concept sets",
          # )
        )
      ),
      # cohort_counts ----
      tabItem(
        tabName = "counts",
        selectors(data$cohort_count, "counts", c("cdm_name", "cohort_name"), multiple = TRUE, default = list()),
        DTOutput("tidy_counts")
      ),
      # code counts ----
      if (any(settings$runCountCodes)) {
        tabItem(
          tabName = "code_counts",
          selectors(data$code_counts, "code_counts", c("cdm_name", "cohort"), multiple = FALSE, default = list()),
          # selectors(data$orphan_counts, "code_counts", c("domain_id"), multiple = TRUE, default = list()),
          pickerInput(
            inputId = "select_code_count_columns",
            label = "Columns to display",
            choices = c("Standard concept id", "Standard concept name", "Cohort", 
                        "Cdm name", "Record count", "Person count"),
            selected = c("Standard concept id", "Standard concept name", "Cohort", 
                         "Cdm name", "Record count", "Person count"),
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
          ),
          DTOutput("tidy_code_counts")
        )
      },
      # orphan ----
      if (any(settings$runCountCodes)) {
        tabItem(
          tabName = "orphan",
          selectors(data$orphan_counts, "orphan", c("cdm_name", "cohort"), multiple = FALSE, default = list()),
          # selectors(data$orphan_counts, "orphan", c("domain_id"), multiple = TRUE, default = list()),
          pickerInput(
            inputId = "select_orphan_count_columns",
            label = "Columns to display",
            choices = c("Standard concept id", "Standard concept name", "Relationship id", "Cohort", 
                        "Cdm name", "Record count", "Person count"),
            selected = c("Standard concept id", "Standard concept name", "Relationship id", "Cohort", 
                         "Cdm name", "Record count", "Person count"),
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
          ),
          DTOutput("tidy_orphan_counts")
        )
      },
      # index ----
      if (any(settings$runIndexEvents)) {
        tabItem(
          tabName = "index",
          selectors(data$index_events, 
                    "index", 
                    c("cdm_name", "cohort_name"), 
                    multiple = FALSE, default = list()),
          selectors(data$index_events, 
                    "index", 
                    c("codelist_name",  "domain_id", "group_name"), 
                    multiple = TRUE, default = list()),
          h5(),
          div(
            style = "display: inline-block;vertical-align:top; width: 150px;",
            pickerInput(
              inputId = "select_index_columns",
              label = "Columns to display",
              choices = c("Standard concept id", "Standard concept name", "Source concept name", 
                          "Source concept id", "Domain id", "Codelist name", "Cohort name", 
                          "Cdm name", "Record count", "Person count"),
              selected = c("Standard concept id", "Standard concept name", "Source concept id", 
                           "Source concept name", "Domain id", "Codelist name", "Cohort name", 
                           "Cdm name", "Record count", "Person count"),
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              ),
              multiple = TRUE
            )
          ),
          DTOutput("index_date_tidy")
        )
      },
      # overlap ----
      if (any(settings$runCalculateOverlap)) {
        tabItem(
          tabName = "overlap",
          selectors(data$cohort_overlap, "overlap", columns = c("cdm_name", "cohort_name_x", "cohort_name_y")),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Table",
              DTOutput("overlap_tidy")
            ),
            tabPanel(
              "Plot",
              h4(),
              radioButtons(
                inputId = "plot_overlap_type",
                label = "Plot type",
                choices = c("Percentage", "Counts"),
                selected = "Percentage"
              ),
              plotlyOutput("overlap_plot") %>% withSpinner()
            )
          )
        )
      },
      # age ----
      if (any(settings$runProfiling)) {
        tabItem(
          tabName = "age",
          selectors(data$age_distribution, "age", c("cdm_name", "cohort_name", "sex", "age_group")),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Tidy table",
              DTOutput("age_tidy_table")
            ),
            tabPanel(
              "Formatted table",
              h4(),
              gt_output("age_format_table")
            )
          )
        )
      },
      # time ----
      if (any(settings$runProfiling)) {
        tabItem(
          tabName = "time",
          selectors(data$time_distribution, "time", c("cdm_name", "cohort_name", "sex", "covariate", "estimate_type")),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Tidy table",
              DTOutput("time_tidy_table")
            ),
            tabPanel(
              "Formatted table",
              h4(),
              gt_output("time_format_table")
            )
          )
        )
      },
      # prevalence ----
      if (any(settings$runPrevalence)) {
        tabItem(
          tabName = "prevalence",
          selectors(data$prevalence, "prevalence", 
                    c("cdm_name", "outcome_cohort_name", 
                      "denominator_age_group", "denominator_sex")),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Table",
              h5(),
              div(
                style = "display: inline-block;vertical-align:top; width: 150px;",
                pickerInput(
                  inputId = "select_prevalence_columns",
                  label = "Columns to display",
                  choices = stringr::str_to_sentence(
                    gsub("_", " ", 
                         c("cdm_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
                           "outcome_cohort_name", "prevalence_start_date", "prevalence_end_date",                    
                           "n_cases", "n_population", "prevalence",                             
                           "prevalence_95CI_lower", "prevalence_95CI_upper", "population_obscured",                    
                           "cases_obscured", "result_obscured", "analysis_type", "analysis_interval",                    
                           "analysis_complete_database_intervals", "analysis_time_point", "analysis_full_contribution"))),
                  selected = stringr::str_to_sentence(
                    gsub("_", " ", 
                         c("cdm_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
                           "outcome_cohort_name", "prevalence_start_date", "prevalence_end_date",                    
                           "n_cases", "n_population", "prevalence",                             
                           "prevalence_95CI_lower", "prevalence_95CI_upper", "population_obscured",                    
                           "cases_obscured", "result_obscured", "analysis_type", "analysis_interval",                    
                           "analysis_complete_database_intervals", "analysis_time_point", "analysis_full_contribution"))),
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE
                )
              ),
              DTOutput("prevalence_table")
            ),
            tabPanel(
              "Plot",
              h4(),
              plotSelectors(prefix = "plot_prevalence", 
                            choices = c("cdm_name", "denominator_age_group", "denominator_sex", "outcome_cohort_name"),
                            default = list(color = "cdm_name", facet_by = "outcome_cohort_name")),
              plotlyOutput("prevalence_plot")
            )
          )
        )
      },
      # incidence ----
      if (any(settings$runIncidence)) {
        tabItem(
          tabName = "incidence",
          selectors(data$incidence, "incidence", 
                    c("cdm_name", "outcome_cohort_name", 
                      "denominator_age_group", "denominator_sex")),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Table",
              h5(),
              div(
                style = "display: inline-block;vertical-align:top; width: 150px;",
                pickerInput(
                  inputId = "select_incident_columns",
                  label = "Columns to display",
                  choices = stringr::str_to_sentence(
                    gsub("_", " ", 
                         c("cdm_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
                           "outcome_cohort_name", "incidence_start_date", "incidence_end_date",                    
                           "n_events", "n_persons", "person_years", "incidence_100000_pys",                             
                           "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper", 
                           "cohort_obscured", "result_obscured","analysis_interval", "analysis_complete_database_intervals"))),
                  selected = stringr::str_to_sentence(
                    gsub("_", " ", 
                         c("cdm_name", "denominator_age_group", "denominator_sex", "denominator_days_prior_observation",
                           "outcome_cohort_name", "incidence_start_date", "incidence_end_date",                    
                           "n_events", "n_persons", "person_years", "incidence_100000_pys",                             
                           "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper", 
                           "cohort_obscured", "result_obscured","analysis_interval", "analysis_complete_database_intervals"))),
                  options = list(
                    `actions-box` = TRUE,
                    size = 10,
                    `selected-text-format` = "count > 3"
                  ),
                  multiple = TRUE
                )
              ),
              DTOutput("incidence_table")
            ),
            tabPanel(
              "Plot",
              h4(),
              plotSelectors(prefix = "plot_incidence",
                            choices = c("cdm_name", "denominator_age_group", "denominator_sex", "outcome_cohort_name"),
                            default = list(color = "cdm_name", facet_by = "outcome_cohort_name")),
              plotlyOutput("incidence_plot")
            )
          )
        )
      },
      # large_scale_characterisation ----
      if (any(settings$runMatchedSampleLSC)) {
        tabItem(
          tabName = "large_scale_characterisation",
          selectors(data$lsc_table, "lsc", c("cdm_name", "cohort_name", "table_name", "window")),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Table",
              h4(),
              pickerInput(
                inputId = "select_lsc_columns",
                label = "Columns to display",
                choices = c("Cdm name", "Cohort name", "Table name", "Concept", "Concept name",
                            "Window", "Matched count", "Matched percentage", "Sample count", 
                            "Sample percentage", "Difference count", "Difference percentage"),
                selected = c("Cdm name", "Cohort name", "Table name", "Concept", "Concept name",
                             "Window", "Matched count", "Matched percentage", "Sample count", 
                             "Sample percentage", "Difference count", "Difference percentage"),
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"
                ),
                multiple = TRUE
              ),
              DTOutput("lsc_table")
            ),
            tabPanel(
              "Plot",
              h4(),
              plotSelectors("plsc", 
                            c("cohort_name", "cdm_name", "table_name", "window", "concept_name"),
                            default = list("color" = "concept_name", "facet_by" = "cdm_name")),
              plotlyOutput('lsc_plot', height = "800px") %>% withSpinner()
            )
          )
        )
      },
      # log ----
      tabItem(
        tabName = "log",
        selectors(data$log, "log", c("cdm_name"), multiple = FALSE),
        uiOutput("log")
      )
      # end ----
    )
  )
)
