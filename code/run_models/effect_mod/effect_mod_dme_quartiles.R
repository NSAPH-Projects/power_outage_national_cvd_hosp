# Run models for effect modification by DME status 

pacman::p_load(tidyverse, gnm, data.table, here, splines)
source(here("code", "run_models", "run_models_helper_functions.R"))

# Read --------------------------------------------------------------------

dme_by_county <-
  read_rds(here(
    "data_for_upload",
    "effect_mod_data",
    "dme_use_by_county.RDS"
  ))

an_dat <- 
  read_rds(here('data', 'an_dat_urgent_hosp_nov_5.RDS'))

# join and select relevant cols
an_dat <- an_dat |>
  left_join(dme_by_county) |>
  select(
    five_digit_fips,
    n_benes,
    exposed_8_hrs_0.01,
    exposed_8_hrs_0.01_lag_1:exposed_8_hrs_0.01_lag_6,
    max_temp,
    precip,
    wind_speed,
    n_cvd_no_hem_no_hyp,
    n_resp,
    stratum,
    dme_quartile
  )


# Run models --------------------------------------------------------------

q4_dme <- an_dat %>% filter(dme_quartile == 4)
q1_dme <- an_dat %>% filter(dme_quartile == 1)

main_models <- list(
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
      exposure_col = 'exposed_8_hrs_0.01')
)


# Read results ------------------------------------------------------------

model_results_list <- read_main_model_results(models = main_models)
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
    'effect_modification_by_dme_quartile.png'
  ),
  width = 28,
  height = 20
)
