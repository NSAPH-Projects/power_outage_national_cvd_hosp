# This script identifies power outage events based on a percentile rather than 
# a cut point of % of county customers without power. This is a 
# denominator-independent metric. It classifies a county-day as exposed to 
# outage if customers out is in the 99.9th percentile or higher for more than 
# 1, 2, or 4 hours. 

# Author: Heather
# Created: sometime in 2022
# Last updated: Oct 7th, 2024

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, here, lubridate, data.table, fst)
source(here("code", 'functions', 'helpers_percentile_based_outages.R'))

# Read --------------------------------------------------------------------

counties <-
  read_fst(here("local_data",
                "power_outage_exposure_data_cleaning_output", 
                "hourly_data_with_coverage_exclusions.fst")) |>
  as.data.table()

counties <-
  counties[, .(
    clean_state_name,
    clean_county_name,
    five_digit_fips,
    year,
    hour,
    customers_out_hourly = customers_out_hourly_locf,
    customers_served_total = downscaled_county_estimate
  )]


counties[, cut_point := quantile(customers_out_hourly, 0.999), 
         by = .(clean_state_name, clean_county_name, five_digit_fips, year)]

# Do ----------------------------------------------------------------------

exposures_99_9_1_hr <- 
  get_exposure_on_percentile(counties, outage_duration = hours(1))

exposures_99_9_2_hr <- 
  get_exposure_on_percentile(counties, outage_duration = hours(2))

exposures_99_9_4_hr <- 
  get_exposure_on_percentile(counties, outage_duration = hours(4))


# Join --------------------------------------------------------------------

all_exposures <- list(exposures_99_9_1_hr,
                      exposures_99_9_2_hr,
                      exposures_99_9_4_hr)

combined_df <- Reduce(function(x, y)
  merge(x, y, by = c(
    "clean_state_name", "clean_county_name", 'five_digit_fips', "day"
  )), all_exposures)


# Write -------------------------------------------------------------------

write_fst(
  combined_df,
  here(
    "data_for_upload",
    "power_outage_exposure_data_cleaning_output",
    "days_exposed_unexposed_percentile_based_exposure.fst"
  )
)
