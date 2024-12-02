# Run models for effect modification by poverty status 

pacman::p_load(tidyverse, gnm, data.table, here, splines)
source(here("code", "run_models", "run_models_helper_functions.R"))

# Read --------------------------------------------------------------------

pov_by_county <-
  read_rds(here(
    "data_for_upload",
    "effect_mod_data",
    "pov_measures.RDS"
  ))

an_dat <- 
  read_rds(here('data', 'an_dat_urgent_hosp_nov_5.RDS'))

# join and select relevant cols
an_dat <- an_dat |>
  left_join(pov_by_county) |>
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
    pov_quartile
  )


# Run models --------------------------------------------------------------

q4_pov <- an_dat %>% filter(pov_quartile == 4)
q1_pov <- an_dat %>% filter(pov_quartile == 1)

main_models <- list(
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
    'effect_modification_by_pov_quartile.png'
  ),
  width = 28,
  height = 20
)
