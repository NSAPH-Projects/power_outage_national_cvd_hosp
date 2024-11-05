# Run models on all cut points and durations of binary exposure for 
# emergency hospitalizations

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines)

source(here("code", "run_models", "helper_function.R"))

# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_nov_2.RDS'))
an_dat <- an_dat %>% mutate(all_hosp = n_cvd_no_hem_no_hyp + n_resp)

# binary exposures
exposure_columns <- c(colnames(an_dat)[10:21])

# Run models --------------------------------------------------------------

all_hospitalizations <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'all_hosp',
    po_data = an_dat
  ),
  exposure_columns
)

cvd_only <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'n_all_cvd',
    po_data = an_dat
  ),
  exposure_columns
)

resp_only <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'n_resp',
    po_data = an_dat
  ),
  exposure_columns
)

# Get model coefficients --------------------------------------------------

all_hospitalizations_summary <-
  read_model_results(all_hospitalizations, exposure_columns)

cvd_only_summary <- read_model_results(cvd_only, exposure_columns)

resp_only_summary <- read_model_results(resp_only, exposure_columns)


# This we could save  -----------------------------------------------------




# Plot --------------------------------------------------------------------

p_all_hosp <-
  create_summary_plot(
    all_hospitalizations_summary,
    title = 'Association of power outage exposure with emergency respiratory and
cardiovascular (not hypertension or hem stroke) hospitalizations among medicare benes in counties
> 500 benes, >50% pous coverage',
    subtitle = 'y axis right is outage duration - 4, 8, 12 hrs'
  )

ggsave(p_all_hosp,
       filename = here("results", 'plots_of_results', 'all_hospitalizations.png'))

p_resp <-
  create_summary_plot(
    resp_only_summary,
    title = 'Association of power outage exposure with emergency respiratory
hospitalizations among medicare benes in counties > 500 benes, >50% pous coverage',
    subtitle = 'y axis right is outage duration - 4, 8, 12 hrs'
  )

ggsave(p_resp,
       filename = here(
         "results",
         'plots_of_results',
         'respiratory_hospitalizations.png'
       ))

p_cvd <-
  create_summary_plot(
    cvd_only_summary,
    title = 'Association of power outage exposure with emergency cvd (no hem no hyp)
hospitalizations among medicare benes in counties > 500 benes, >50% pous coverage',
    subtitle = 'y axis right is outage duration - 4, 8, 12 hrs'
  )

ggsave(p_cvd,
       filename = here(
         "results",
         'plots_of_results',
         'cvd_no_hem_no_hyp_hospitalizations.png'
       ))
