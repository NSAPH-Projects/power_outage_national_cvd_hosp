# Create analytic data from exposure and outcome for urgent hospitalizations

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow)

# Read --------------------------------------------------------------------

panel_fips <- read_rds(here('data_for_upload', 'panel_for_2018.RDS')) %>%
  rename(day = date)

hosp <-
  read_rds(here(
    "data",
    "urg_and_emerg_num_hosp_by_day_by_county_inc_state_jan_28.RDS"
  )) %>%
  select(five_digit_fips = county,
         day = admission_date,
         n_all_cvd:n_resp_1_medicaid) 

outage_exposure <- 
  read_parquet(
    here(
      'data_for_upload',
      'power_outage_exposure_data_cleaning_output',
      'analytic_exposure_hot_cold_urban_rural_smoke.parquet'
    )
  ) %>%
select(five_digit_fips, 
       day, 
       urban,
       exposed_1_hrs_percentile:exposed_8_hrs_0.05_cold, # may need to change this line in the rerun - new cols 
       county_customers, 
       percent_served) %>%
  mutate(day = as.Date(day))

meteo <- read_parquet(here('data', 'meteo_vars.parquet')) %>%
  select(five_digit_fips, day = observation_date, max_temp:wind_speed)

denoms <- read_rds(here('data', 'benes_by_county_fips_jan_28.RDS')) %>%
  select(five_digit_fips = county, n_benes:n_benes_non_eligible)

# Do ----------------------------------------------------------------------

an_dat <- 
  panel_fips %>%
  left_join(outage_exposure) %>%
  left_join(meteo) %>%
  left_join(hosp) %>%
  left_join(denoms)

length(unique(an_dat$five_digit_fips))
# 3143 to start 

j <- an_dat %>% filter(!is.na(exposed_8_hrs_0.01) & percent_served >=0.5)
length(unique(j$five_digit_fips))
# 907 excluded due to missing exposure data 
write_rds(j, here("data_for_upload", 'included_counties.RDS')) # new line for getting included counties 

# get rid of counties w no one
an_dat <- an_dat %>% filter(n_benes > 0)
length(unique(an_dat$five_digit_fips))
# 5 counties 

# missing values have different meanings.
# when power outage exposure is missing, this means we had insufficient exposure 
# data for those counties and so we should exclude them 

an_dat <- an_dat %>% filter(!is.na(exposed_1_hrs_0.005))
length(unique(an_dat$five_digit_fips))
# excludes 132 for 3006 left

an_dat_low_missingness <- an_dat %>% filter(percent_served >= 0.8 &
                                              !is.na(percent_served))
# VIVIAN 
# Need to know how many counties have >80% of data 
length(unique(an_dat_low_missingness$five_digit_fips))
# 1877

# filter for low percent served
an_dat <- an_dat %>% filter(percent_served >= 0.5 & !is.na(percent_served))
length(unique(an_dat$five_digit_fips))
# excludes 772 counties 
# in total excluded 909 counties 




# when hospitalizations are missing, that means that there were no 
# hospitalizations on those days, so we should set those to 0

an_dat <-
  an_dat %>%
  mutate_at(
    vars(n_all_cvd:n_benes_non_eligible),
    ~ ifelse(is.na(.), 0, .)
  )
length(unique(an_dat$five_digit_fips))

# add strata 
an_dat[, day_of_week := lubridate::wday(day)]
an_dat[, two_month_period := cut(day, breaks = "2 months", labels = FALSE)]
an_dat[, stratum := .GRP, by = .(five_digit_fips, day_of_week, two_month_period)]

# filter out lagged nas
#an_dat <- an_dat[complete.cases(an_dat),]

# Write -------------------------------------------------------------------

write_rds(an_dat, here('data', 'an_dat_urgent_hosp_jan_28.RDS'))