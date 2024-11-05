# Run models on all cut points and durations of binary exposure for 
# emergency hospitalizations

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

# Age ---------------------------------------------------------------------

all_hospitalizations_older_75 <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'all_hosp_75_older',
    po_data = an_dat,
    offset = 'n_benes_older_75'
  ),
  exposure_columns
)

all_hospitalizations_younger_75 <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'all_hosp_younger_75',
    po_data = an_dat,
    offset = 'n_benes_under_75'
  ),
  exposure_columns
)

cvd_only_older_75 <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'n_cvd_no_hem_no_hyp_1_age',
    po_data = an_dat,
    offset = 'n_benes_older_75'
  ),
  exposure_columns
)

cvd_only_younger_75 <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'n_cvd_no_hem_no_hyp_0_age',
    po_data = an_dat,
    offset = 'n_benes_under_75'
  ),
  exposure_columns
)

resp_only_older_75 <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'n_resp_1_age',
    po_data = an_dat,
    offset = 'n_benes_older_75'
  ),
  exposure_columns
)

resp_only_younger_75 <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'n_resp_0_age',
    po_data = an_dat,
    offset = 'n_benes_under_75'
  ),
  exposure_columns
)


# Sex ---------------------------------------------------------------------

all_hospitalizations_female <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'all_hosp_female',
    po_data = an_dat,
    offset = 'n_benes_sex_2'
  ),
  exposure_columns
)

all_hospitalizations_male <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'all_hosp_male',
    po_data = an_dat,
    offset = 'n_benes_sex_1'
  ),
  exposure_columns
)

cvd_only_female <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'n_cvd_no_hem_no_hyp_2_sex',
    po_data = an_dat,
    offset = 'n_benes_sex_2'
  ),
  exposure_columns
)

cvd_only_male <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'n_cvd_no_hem_no_hyp_1_sex',
    po_data = an_dat,
    offset = 'n_benes_sex_1'
  ),
  exposure_columns
)

resp_only_female <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'n_resp_2_sex',
    po_data = an_dat,
    offset = 'n_benes_sex_2'
  ),
  exposure_columns
)

resp_only_male <- setNames(
  lapply(
    X = exposure_columns,
    FUN = run_binary_exposure_model,
    outcome_col = 'n_resp_1_sex',
    po_data = an_dat,
    offset = 'n_benes_sex_1'
  ),
  exposure_columns
)

# Get model coefs ---------------------------------------------------------

names <- c(
  "all_hospitalizations_older_75",
  "all_hospitalizations_younger_75",
  "all_hospitalizations_male",
  "all_hospitalizations_female",
  "cvd_only_older_75",
  "cvd_only_younger_75",
  "cvd_only_male",
  "cvd_only_female",
  "resp_only_older_75",
  "resp_only_younger_75",
  "resp_only_male",
  "resp_only_female"
)

age_sex_models <-
  list(
    all_hospitalizations_older_75,
    all_hospitalizations_younger_75,
    all_hospitalizations_male,
    all_hospitalizations_female,
    cvd_only_older_75,
    cvd_only_younger_75,
    cvd_only_male,
    cvd_only_female,
    resp_only_older_75,
    resp_only_younger_75,
    resp_only_male,
    resp_only_female
  )

model_results_list <- lapply(FUN = read_model_results(), X = age_sex_models)

mapply(
  FUN = function(model_result, name) {
    file_path <- here("results", "model_coefs_errors", paste0(name, "_coef_errors.RDS"))
    write_rds(model_result, file_path)
  },
  model_results_list,
  names
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
