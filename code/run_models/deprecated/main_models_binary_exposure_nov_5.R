
pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines)

source(here("code", "run_models", "run_models_helper_functions.R"))

# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_nov_5.RDS'))
an_dat <- an_dat %>%
  mutate(
    all_hosp = n_cvd_no_hem_no_hyp + n_resp
  )

# binary exposures
exposure_columns <- c(colnames(an_dat)[10:21])

# Run models --------------------------------------------------------------

# main models
main_models <- list(
  all_hospitalizations_among_all_benes =
    run_models('all_hosp', 'n_benes', an_dat, exposure_columns),
  cvd_only_among_all_benes =
    run_models('n_cvd_no_hem_no_hyp', 'n_benes', an_dat, exposure_columns),
  resp_only_among_all_benes =
    run_models('n_resp', 'n_benes', an_dat, exposure_columns),
  inc_hem_hyp_among_all_benes =
    run_models('n_all_cvd', 'n_benes', an_dat, exposure_columns)
)

# Get results -------------------------------------------------------------

model_results_list <- 
  lapply(main_models, read_model_results, exposure_columns)

# save 
mapply(save_model_results, model_results_list, names(model_results_list))

# Plots -------------------------------------------------------------------

model_names_list <- names(model_results_list)

for (i in 1:length(model_names_list)){
  create_and_save_plot(
    summary_table = model_results_list[[i]],
    title = paste0('Association of power outage exposure with',
                   model_names_list[[i]],
                   ' in counties > 500 benes, >50% pous coverage'),
    subtitle = 'y axis right is outage duration - 4, 8, 12 hrs',
    filename = paste0(model_names_list[[i]], '.png')
  )
}


