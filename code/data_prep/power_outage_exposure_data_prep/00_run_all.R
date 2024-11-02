# This script runs the power outage data cleaning pipeline. 

# load necessary libraries
pacman::p_load(here, logger)

# init log
log_appender(appender_console)
log_threshold(INFO)

# function to source a script and clean up
source_and_clean <- function(script_path) {
  log_info(paste("Sourcing:", script_path))
  source(script_path, echo = TRUE)
  rm(list = ls())
  gc()
}

# start timing
start_time <- Sys.time()
log_info('Preparing to clean power outage data.')

# define the scripts to be sourced using `here`
scripts <- list(
  here(
    'code',
    'data_prep',
    'power_outage_exposure_data_prep',
    'a00_define_contiguous_US.R'
  ),
  here(
    'code',
    'data_prep',
    'power_outage_exposure_data_prep',
    'a01_find_eia_state_customers.R'
  ),
  here(
    'code',
    'data_prep',
    'power_outage_exposure_data_prep',
    'a02_get_county_census_cust_est.R'
  ),
  here(
    'code',
    'data_prep',
    'power_outage_exposure_data_prep',
    'a03_county_customer_census_estimates.R'
  ),
  here(
    'code',
    'data_prep',
    'power_outage_exposure_data_prep',
    'b01_read_and_clean.R'
  ),
  here(
    'code',
    'data_prep',
    'power_outage_exposure_data_prep',
    'b02_expand_to_hourly.R'
  ),
  here(
    'code',
    'data_prep',
    'power_outage_exposure_data_prep',
    'b03_attach_denoms.R'
  ),
  here(
    'code',
    'data_prep',
    'power_outage_exposure_data_prep',
    'b04_id_outages_continuous_measures.R'
  ),
  here(
    'code',
    'data_prep',
    'power_outage_exposure_data_prep',
    'b05_identify_binary_daily_exposure.R'
  ),
  here(
    'code',
    'data_prep',
    'power_outage_exposure_data_prep',
    'b06_identify_outages_percentile.R'
  ),
  here(
    'code',
    'data_prep',
    'power_outage_exposure_data_prep',
    'b07_create_exposure_data_for_upload.R'
  )
)

# source each script and clean up
for (script in scripts) {
  source_and_clean(script)
}

# stop timing
stop_time <- Sys.time()

log_info(paste0('Total run time was ', round(
  difftime(stop_time, start_time, units = "mins"), 2
), ' minutes'))
