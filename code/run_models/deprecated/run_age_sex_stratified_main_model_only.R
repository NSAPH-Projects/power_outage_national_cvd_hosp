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

age_sex_models <- list(
  all_hospitalizations_among_benes_75_older = run_binary_exposure_model(
    outcome_col = 'all_hosp_75_older',
    offset_col = 'n_benes_older_75',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  all_hospitalizations_among_benes_younger_75 = run_binary_exposure_model(
    outcome_col = 'all_hosp_younger_75',
    offset_col ='n_benes_under_75',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  all_hospitalizations_among_male_benes = run_binary_exposure_model(
    outcome_col ='all_hosp_male',
    offset_col ='n_benes_sex_1',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  all_hospitalizations_among_female_benes = run_binary_exposure_model(
    outcome_col ='all_hosp_female',
    offset_col ='n_benes_sex_2',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  cvd_only_among_benes_75_older = run_binary_exposure_model(
    outcome_col ='n_cvd_no_hem_no_hyp_1_age',
    offset_col ='n_benes_older_75',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  cvd_only_among_benes_younger_75 = run_binary_exposure_model(
    outcome_col ='n_cvd_no_hem_no_hyp_0_age',
    offset_col ='n_benes_under_75',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  cvd_only_among_male_benes = run_binary_exposure_model(
    outcome_col ='n_cvd_no_hem_no_hyp_1_sex',
    offset_col ='n_benes_sex_1',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  cvd_only_among_female_benes = run_binary_exposure_model(
    outcome_col ='n_cvd_no_hem_no_hyp_2_sex',
    offset_col ='n_benes_sex_2',
    an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  resp_only_among_benes_75_older = run_binary_exposure_model(
    outcome_col ='n_resp_1_age',
    offset_col ='n_benes_older_75',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  resp_only_among_benes_younger_75 = run_binary_exposure_model(
    outcome_col ='n_resp_0_age',
    offset_col ='n_benes_under_75',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  resp_only_among_male_benes = run_binary_exposure_model(
    outcome_col ='n_resp_1_sex',
    offset_col ='n_benes_sex_1',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  ),
  resp_only_among_female_benes = run_binary_exposure_model(
    outcome_col ='n_resp_2_sex',
    offset_col ='n_benes_sex_2',
    po_data = an_dat,
    exposure_col = 'exposed_8_hrs_0.01'
  )
)

# Get model results -------------------------------------------------------

model_results_list <- read_main_model_results(models = age_sex_models)
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
    'age_sex_main_model_only_results.png'
  ),
  width = 28,
  height = 20
)
