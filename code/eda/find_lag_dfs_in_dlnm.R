# Investigate the correct dfs for power outage lags in dlnm


# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines,
               dlnm)

source(here("code", "run_models", "run_models_helper_functions.R"))


# Helpers -----------------------------------------------------------------

# calculate qaics 
get_qaic <- function(quasi_models, models){
  quasi_model <- quasi_models$po_model
  model <- models$po_model
  ll <- as.numeric(logLik(model))
  quasi_summary <- summary(quasi_model)
  dispersion <- quasi_summary$dispersion
  dfs <- df.residual(quasi_model)
  qaic <- -2 * ll/dispersion + 2 * dfs
  return(qaic)
}

# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_dec_17.RDS'))
an_dat <- an_dat %>% mutate(log_n_benes = log(n_benes))

# Models ------------------------------------------------------------------

dfs_of_interest = c(3, 4, 5, 6)

test_different_dfs_in_lags <- lapply(
  FUN = run_dlnm_po_model_copilot,
  X = dfs_of_interest,
  po_data = an_dat,
  outcome_col = 'n_cvd_no_hyp',
  exposure_col = 'exposed_8_hrs_0.01',
  offset_col = 'n_benes',
  precip_dfs = 2
)

test_different_dfs_in_lags_regular_pois <-
  lapply(
    FUN = run_dlnm_po_model_regular_poisson,
    X = dfs_of_interest,
    po_data = an_dat,
    outcome_col = 'n_cvd_no_hyp',
    exposure_col = 'exposed_8_hrs_0.01',
    offset_col = 'n_benes',
    precip_dfs = 2
  )

qaics <-
  mapply(FUN = get_qaic,
         quasi_models = test_different_dfs_in_lags,
         models = test_different_dfs_in_lags_regular_pois)

# according to this, the best fit is 4 dfs on the lag dimension

test_different_dfs_in_lags <-
  lapply(
    FUN = run_dlnm_po_model_linear_precip,
    X = dfs_of_interest,
    po_data = an_dat,
    outcome_col = 'n_resp',
    exposure_col = 'exposed_8_hrs_0.01',
    offset_col = 'log_n_benes'
  )

test_different_dfs_in_lags_regular_pois <-
  lapply(
    FUN = run_dlnm_po_model_linear_precip_regular_poisson,
    X = dfs_of_interest,
    po_data = an_dat,
    outcome_col = 'n_resp',
    exposure_col = 'exposed_8_hrs_0.01',
    offset_col = 'log_n_benes'
  )

qaics <-
  mapply(FUN = get_qaic,
         quasi_models = test_different_dfs_in_lags,
         models = test_different_dfs_in_lags_regular_pois)

max(qaics)
print(qaics)

# best fit is 3 dfs
