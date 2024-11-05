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
  all_hospitalizations_among_benes_75_older = run_models(
    'all_hosp_75_older',
    'n_benes_older_75',
    an_dat,
    exposure_columns
  ),
  all_hospitalizations_among_benes_younger_75 = run_models(
    'all_hosp_younger_75',
    'n_benes_under_75',
    an_dat,
    exposure_columns
  ),
  all_hospitalizations_among_male_benes = run_models(
    'all_hosp_male',
    'n_benes_sex_1',
    an_dat,
    exposure_columns
    ),
  all_hospitalizations_among_female_benes = run_models(
    'all_hosp_female',
    'n_benes_sex_2',
    an_dat,
    exposure_columns
    ),
  cvd_only_among_benes_75_older = run_models(
    'n_cvd_no_hem_no_hyp_1_age',
    'n_benes_older_75',
    an_dat,
    exposure_columns
  ),
  cvd_only_among_benes_younger_75 = run_models(
    'n_cvd_no_hem_no_hyp_0_age',
    'n_benes_under_75',
    an_dat,
    exposure_columns
  ),
  cvd_only_among_male_benes = run_models(
    'n_cvd_no_hem_no_hyp_1_sex',
    'n_benes_sex_1',
    an_dat,
    exposure_columns
  ),
  cvd_only_among_female_benes = run_models(
    'n_cvd_no_hem_no_hyp_2_sex',
    'n_benes_sex_2',
    an_dat,
    exposure_columns
  ),
  resp_only_among_benes_75_older = run_models(
    'n_resp_1_age',
    'n_benes_older_75',
    an_dat,
    exposure_columns
    ),
  resp_only_among_benes_younger_75 = run_models(
    'n_resp_0_age',
    'n_benes_under_75',
    an_dat,
    exposure_columns
    ),
  resp_only_among_male_benes = run_models(
    'n_resp_1_sex',
    'n_benes_sex_1',
    an_dat,
    exposure_columns
    ),
  resp_only_among_female_benes = run_models(
    'n_resp_2_sex',
    'n_benes_sex_2',
    an_dat,
    exposure_columns
    )
)

# Get model results -------------------------------------------------------

model_results_list <-
  lapply(age_sex_models, read_model_results, exposure_columns)

# Save model results ------------------------------------------------------

mapply(save_model_results, model_results_list, names(model_results_list))

# Plots -------------------------------------------------------------------

model_names_list <- model_results_list$names

for (model_name in model_names_list){
  create_and_save_plot(
    summary_table = model_results_list$model_name,
    title = paste0('Association of power outage exposure with',
                   model_name,
                   ' in counties > 500 benes, >50% pous coverage'),
    subtitle = 'y axis right is outage duration - 4, 8, 12 hrs',
    filename = paste0(model_name, '.png')
  )
}
