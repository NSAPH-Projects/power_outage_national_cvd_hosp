# Investigate the correct dfs for power outage lags in dlnm


# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines,
               dlnm)

source(here("code", "run_models", "run_models_helper_functions.R"))


# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_nov_5.RDS'))
an_dat <- an_dat %>% mutate(log_n_benes = log(n_benes))

# Models ------------------------------------------------------------------

dfs_of_interest = c(3, 4, 5, 6)

test_different_dfs_in_lags <- lapply(FUN = run_dlnm_po_model_copilot,
                                     X = dfs_of_interest,
                                     po_data = an_dat,
                                     outcome_col = 'n_cvd_no_hem_no_hyp',
                                     exposure_col = 'exposed_8_hrs_0.01',
                                     offset_col = 'log_n_benes',
                                     precip_dfs = 2)


summary(test_different_dfs_in_lags[[4]]$po_model)

anova(test_different_dfs_in_lags[[1]]$po_model, 
      test_different_dfs_in_lags[[2]]$po_model, 
      test = 'F')

anova(test_different_dfs_in_lags[[2]]$po_model, 
      test_different_dfs_in_lags[[3]]$po_model, 
      test = 'F')

anova(test_different_dfs_in_lags[[3]]$po_model, 
      test_different_dfs_in_lags[[4]]$po_model, 
      test = 'F')

anova(test_different_dfs_in_lags[[1]]$po_model, 
      test_different_dfs_in_lags[[4]]$po_model, 
      test = 'F')

# 6 dfs for the lag terms is the best fit 
# try for main respiratory models? 

test_different_dfs_in_lags <-
  lapply(
    FUN = run_dlnm_po_model_linear_precip,
    X = dfs_of_interest,
    po_data = an_dat,
    outcome_col = 'n_resp',
    exposure_col = 'exposed_8_hrs_0.01',
    offset_col = 'log_n_benes'
  )

summary(test_different_dfs_in_lags[[4]]$po_model)

anova(test_different_dfs_in_lags[[1]]$po_model, 
      test_different_dfs_in_lags[[2]]$po_model, 
      test = 'F')

anova(test_different_dfs_in_lags[[2]]$po_model, 
      test_different_dfs_in_lags[[3]]$po_model, 
      test = 'F')

anova(test_different_dfs_in_lags[[3]]$po_model, 
      test_different_dfs_in_lags[[4]]$po_model, 
      test = 'F')

anova(test_different_dfs_in_lags[[1]]$po_model, 
      test_different_dfs_in_lags[[4]]$po_model, 
      test = 'F')
# looks like 3 dfs is the best fit 

