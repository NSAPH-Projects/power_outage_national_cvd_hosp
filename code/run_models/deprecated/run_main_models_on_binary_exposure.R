# Run models on all cut points and durations of binary exposure for 
# emergency hospitalizations

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines)

source(here("code", "run_models", "run_models_helper_functions.R"))

# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_nov_5.RDS'))
an_dat <- an_dat %>% mutate(all_hosp = n_cvd_no_hem_no_hyp + n_resp)

# binary exposures
exposure_columns <- c(colnames(an_dat)[10:21])

# Run models --------------------------------------------------------------

all_hospitalizations <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'all_hosp',
    po_data = an_dat,
    offset_col = 'n_benes'
  ),
  exposure_columns
)

cvd_only <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'n_cvd_no_hem_no_hyp',
    po_data = an_dat,
    offset_col = 'n_benes'
  ),
  exposure_columns
)

resp_only <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'n_resp',
    po_data = an_dat,
    offset_col = 'n_benes'
  ),
  exposure_columns
)

inc_hem_hyp <- setNames(
  lapply(
    X = exposure_columns, 
    FUN = run_binary_exposure_model,
    outcome_col = 'n_all_cvd',
    po_data = an_dat,
    offset_col = 'n_benes'
  ),
  exposure_columns 
)

# Get model coefficients --------------------------------------------------

all_hospitalizations_summary <-
  read_model_results(all_hospitalizations, exposure_columns)

cvd_only_summary <- read_model_results(cvd_only, exposure_columns)

resp_only_summary <- read_model_results(resp_only, exposure_columns)

hem_hyp_summary <- read_model_results(inc_hem_hyp, exposure_columns)

# save 

write_rds(
  all_hospitalizations_summary,
  here("results", "model_coefs_errors", "all_hosp_coef_errors.RDS")
)

write_rds(
  cvd_only_summary,
  here("results", "model_coefs_errors", "cvd_only_coef_errors.RDS")
)

write_rds(
  resp_only_summary,
  here("results", "model_coefs_errors", "resp_only_coef_errors.RDS")
)

write_rds(
  hem_hyp_summary,
  here("results", "model_coefs_errors", "hem_hyp_coef_errors.RDS")
)

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
       width = 14,
       height = 7,
       filename = here("results", 'plots_of_results', 'all_hospitalizations.png'))

p_resp <-
  create_summary_plot(
    resp_only_summary,
    title = 'Association of power outage exposure with emergency respiratory
hospitalizations among medicare benes in counties > 500 benes, >50% pous coverage',
    subtitle = 'y axis right is outage duration - 4, 8, 12 hrs'
  )

ggsave(p_resp,
       width = 14,
       height = 7,
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
       width = 14,
       height = 7,
       filename = here(
         "results",
         'plots_of_results',
         'cvd_no_hem_no_hyp_hospitalizations.png'
       ))

p_hem_hyp <- 
  create_summary_plot(
    hem_hyp_summary,
    title = 'Association of power outage exposure with emergency cvd, including hem and hyp,
hospitalizations among medicare benes in counties > 500 benes, >50% pous coverage',
    subtitle = 'y axis right is outage duration - 4, 8, 12 hrs'
  )

ggsave(p_hem_hyp,
       width = 14,
       height = 7,
       filename = here(
         "results",
         'plots_of_results',
         'cvd_YES_hem_hyp_hospitalizations.png'
       ))
