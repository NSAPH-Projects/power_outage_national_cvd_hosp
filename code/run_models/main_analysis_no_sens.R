# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines)

source(here("code", "run_models", "run_models_helper_functions.R"))

# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_nov_5.RDS'))
an_dat <- an_dat %>%
  mutate(
    all_hosp = n_cvd_no_hem_no_hyp + n_resp,
    all_hosp_75_older = n_cvd_no_hem_no_hyp_1_age + n_resp_1_age,
    all_hosp_younger_75 = n_cvd_no_hem_no_hyp_0_age + n_resp_0_age,
    all_hosp_female = n_cvd_no_hem_no_hyp_2_sex + n_resp_2_sex,
    all_hosp_male = n_cvd_no_hem_no_hyp_1_sex + n_resp_1_sex
  )

# binary exposures
exposure_columns <- c(colnames(an_dat)[10:21])

# Run models --------------------------------------------------------------

# main models
main_models <- list(
  all_hospitalizations_among_all_benes =
    run_binary_exposure_model(
      'all_hosp', 
      'n_benes', 
      an_dat, 
      exposure_col = 'exposed_8_hrs_0.01'),
  cvd_only_among_all_benes =
    run_binary_exposure_model(
      'n_cvd_no_hem_no_hyp', 
      'n_benes', 
      an_dat, 
      exposure_col = 'exposed_8_hrs_0.01'),
  resp_only_among_all_benes =
    run_binary_exposure_model(
      'n_resp', 
      'n_benes', 
      an_dat, 
      exposure_col = 'exposed_8_hrs_0.01'),
  inc_hem_hyp_among_all_benes =
    run_binary_exposure_model(
      'n_all_cvd', 
      'n_benes', 
      an_dat, 
      exposure_col = 'exposed_8_hrs_0.01')
)

# Get model results -------------------------------------------------------

model_results_list <- read_main_model_results(models = main_models)
model_results <- rbindlist(model_results_list)
model_results$model_name <- names(model_results_list)

# Plot --------------------------------------------------------------------

p <- create_summary_plot_main_model(
  summary_table = model_results
)

ggsave(
  p,
  filename = here(
    'results',
    'plots_of_results',
    'main_analysis_no_sensitivity.png'
  ),
  width = 28,
  height = 20
)
