# Identify power outage events, defining power outage based on cut point 
# percentage of customers out and a duration. 

# Author: Heather
# Created: sometime in 2022
# Last updated: Oct 7th, 2024

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, here, lubridate, data.table, fst)

# Read --------------------------------------------------------------------

counties <-
  read_fst(here("data",
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
    customers_served_total = customers_served_estimate_to_use
  )]

counties_list <- 
  counties %>% group_by(five_digit_fips) %>% group_split()

# Binary measures ---------------------------------------------------------

# maybe turn this into a loop itself? 
# 8 hrs 
pos_0.005_8_hrs <- wrap_id_power_outages(
  counties_list = counties_list,
  cut_point = 0.005,
  outage_duration = hours(8)
)

pos_0.01_8_hrs <- wrap_id_power_outages(
  counties_list = counties_list,
  cut_point = 0.01,
  outage_duration = hours(8)
)

pos_0.03_8_hrs <- wrap_id_power_outages(
  counties_list = counties_list,
  cut_point = 0.03,
  outage_duration = hours(8)
)

pos_0.05_8_hrs <- wrap_id_power_outages(
  counties_list = counties_list,
  cut_point = 0.05,
  outage_duration = hours(8)
)

# 4 hrs
pos_0.005_4_hrs <- wrap_id_power_outages(
  counties_list = counties_list,
  cut_point = 0.005,
  outage_duration = hours(4)
)

pos_0.01_4_hrs <- wrap_id_power_outages(
  counties_list = counties_list,
  cut_point = 0.01,
  outage_duration = hours(4)
)

pos_0.03_4_hrs <- wrap_id_power_outages(
  counties_list = counties_list,
  cut_point = 0.03,
  outage_duration = hours(4)
)

pos_0.05_4_hrs <- wrap_id_power_outages(
  counties_list = counties_list,
  cut_point = 0.05,
  outage_duration = hours(4)
)

# 12 hrs
pos_0.005_12_hrs <- wrap_id_power_outages(
  counties_list = counties_list,
  cut_point = 0.005,
  outage_duration = hours(12)
)

pos_0.01_12_hrs <- wrap_id_power_outages(
  counties_list = counties_list,
  cut_point = 0.01,
  outage_duration = hours(12)
)

pos_0.03_12_hrs <- wrap_id_power_outages(
  counties_list = counties_list,
  cut_point = 0.03,
  outage_duration = hours(12)
)

pos_0.05_12_hrs <- wrap_id_power_outages(
  counties_list = counties_list,
  cut_point = 0.05,
  outage_duration = hours(12)
)



saveRDS(
  all_an_dat,
  here(
    "power_outage_medicare_data",
    "power_outage_medicare_data_cleaning_output",
    "days_exposed_unexposed_1p_8_hrs.RDS"
  )
)
saveRDS(
  num_outages_frame,
  here(
    "power_outage_medicare_data",
    "power_outage_medicare_data_cleaning_output",
    "num_outages_g_8_hrs.RDS"
  )
)

