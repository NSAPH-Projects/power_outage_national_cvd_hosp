# This script produces a time series of days for 1 year for each county, 
# tabulating how many hours without power there were on each day for a variety 
# of cut-points defining power outage. 

# Author: Heather
# Created: sometime in 2022
# Last updated: Oct 7th, 2024

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, here, lubridate, data.table, fst)
source(here('code', 'functions', 'expand_to_hourly_helpers.R'))

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

# Continuous measures -----------------------------------------------------

# identify continuous hours out per day for different cut points
counties <- identify_power_outage_on_all_counties(counties, cut_point = 0.005)
counties <- identify_power_outage_on_all_counties(counties, cut_point = 0.01)
counties <- identify_power_outage_on_all_counties(counties, cut_point = 0.03)
counties <- identify_power_outage_on_all_counties(counties, cut_point = 0.05)


# group by day and sum to get the number of hours out per day 
# can multiply this by customers served to get customer-hrs out

setDT(counties)
counties[, day := lubridate::floor_date(hour, unit = 'day')]
hrs_out <- counties[, .(
  n_hrs_out_0.005 = sum(po_on_0.005, na.rm = TRUE),
  n_hrs_out_0.01 = sum(po_on_0.01, na.rm = TRUE),
  n_hrs_out_0.03 = sum(po_on_0.03, na.rm = TRUE),
  n_hrs_out_0.05 = sum(po_on_0.05, na.rm = TRUE)
), by = .(five_digit_fips, day)]

write_fst(hrs_out, here(
  'data_for_upload',
  'power_outage_exposure_data_cleaning_output',
  'daily_hrs_out_oct_31.fst'
))

