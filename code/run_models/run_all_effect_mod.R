# Run all effect modification models:
# - age, sex, quartile of DME use, and quartile of poverty 


# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, gnm, data.table, here, splines)
source(here("code", "run_models", "run_models_helper_functions.R"))


# Read --------------------------------------------------------------------

an_dat <- 
  read_rds(here('data', 'an_dat_urgent_hosp_nov_5.RDS'))

pov_by_county <-
  read_rds(here(
    "data_for_upload",
    "effect_mod_data",
    "pov_measures.RDS"
  ))

dme_by_county <-
  read_rds(here(
    "data_for_upload",
    "effect_mod_data",
    "dme_use_by_county.RDS"
  ))

an_dat <- an_dat %>%
  left_join(dme_by_county) %>%
  left_join(pov_by_county)

q4_dme <- an_dat %>% filter(dme_quartile == 4)
q1_dme <- an_dat %>% filter(dme_quartile == 1)

q4_pov <- an_dat %>% filter(pov_quartile == 4)
q1_pov <- an_dat %>% filter(pov_quartile == 1)


# Run models --------------------------------------------------------------

effect_mod_models <- list(
  cvd_dme_4 =
    run_binary_exposure_model(
      'n_cvd_no_hem_no_hyp',
      'n_benes', 
      q4_dme, 
      exposure_col = 'exposed_8_hrs_0.01'),
  resp_dme_4 =
    run_binary_exposure_model(
      'n_resp', 
      'n_benes', 
      q4_dme, 
      exposure_col = 'exposed_8_hrs_0.01'),
  cvd_dme_1 =
    run_binary_exposure_model(
      'n_cvd_no_hem_no_hyp', 
      'n_benes', 
      q1_dme, 
      exposure_col = 'exposed_8_hrs_0.01'),
  resp_dme_1 =
    run_binary_exposure_model(
      'n_resp', 
      'n_benes', 
      q1_dme, 
      exposure_col = 'exposed_8_hrs_0.01'),
  cvd_pov_4 =
    run_binary_exposure_model(
      'n_cvd_no_hem_no_hyp',
      'n_benes', 
      q4_pov, 
      exposure_col = 'exposed_8_hrs_0.01'),
  resp_pov_4 =
    run_binary_exposure_model(
      'n_resp', 
      'n_benes', 
      q4_pov, 
      exposure_col = 'exposed_8_hrs_0.01'),
  cvd_pov_1 =
    run_binary_exposure_model(
      'n_cvd_no_hem_no_hyp', 
      'n_benes', 
      q1_pov, 
      exposure_col = 'exposed_8_hrs_0.01'),
  resp_pov_1 =
    run_binary_exposure_model(
      'n_resp', 
      'n_benes', 
      q1_pov, 
      exposure_col = 'exposed_8_hrs_0.01'),
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

# Read results ------------------------------------------------------------

model_results_list <- read_main_model_results(models = effect_mod_models)
model_results <- rbindlist(model_results_list)
model_results$model_name <- names(model_results_list)

p <- create_summary_plot_main_model(
  summary_table = model_results
)

ggsave(
  p,
  filename = here(
    'results',
    'plots_of_results',
    'all_effect_mod.png'
  ),
  width = 28,
  height = 20
)


