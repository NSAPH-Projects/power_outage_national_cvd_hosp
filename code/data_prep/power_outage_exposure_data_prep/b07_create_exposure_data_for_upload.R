# This script combines all the exposure metrics to create an analytic dataset 
# to upload to FASSE.

# Author: Heather
# Created: sometime in 2022
# Last updated: Oct 7th, 2024

pacman::p_load(here, tidyverse, data.table, fst, arrow)


# Read --------------------------------------------------------------------

denoms <- read_fst(
  here(
    'local_data',
    'power_outage_exposure_data_cleaning_output',
    "county_customer_denoms_and_p_missing.fst"
  )
)

percentile_based_exposure <- read_fst(
  here(
    'data_for_upload',
    "power_outage_exposure_data_cleaning_output",
    "days_exposed_unexposed_percentile_based_exposure.fst"
  )
)


binary_exposure <- read_fst(
  here(
    'data_for_upload',
    "power_outage_exposure_data_cleaning_output",
    "all_days_exposed_unexposed.fst"
  )
)

daily_hrs_out <- read_fst(
  here(
    'data_for_upload',
    "power_outage_exposure_data_cleaning_output",
    "daily_hrs_out_oct_31.fst"
  )
)

all_exposures <- percentile_based_exposure %>%
  left_join(daily_hrs_out) %>%
  left_join(binary_exposure)

all_exposures <-
  all_exposures %>%
  mutate(year = lubridate::year(day)) 

# attach denoms 
all_exposures <- all_exposures %>% left_join(denoms)

# Write -------------------------------------------------------------------

write_parquet(
  all_exposures,
  here(
    'data_for_upload',
    "power_outage_exposure_data_cleaning_output",
    "analytic_exposure_data_all_years.parquet"
  )
)

all_exposures <- all_exposures %>% filter(year == 2018)

write_parquet(
  all_exposures,
  here(
    'data_for_upload',
    "power_outage_exposure_data_cleaning_output",
    "analytic_exposure_data_2018.parquet"
  )
)
