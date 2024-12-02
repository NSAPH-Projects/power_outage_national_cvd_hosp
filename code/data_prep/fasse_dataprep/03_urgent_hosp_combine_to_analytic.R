# Create analytic data from exposure and outcome for urgent hospitalizations

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow)

# Read --------------------------------------------------------------------

panel_fips <- read_rds(here('data_for_upload', 'panel_for_2018.RDS')) %>%
  rename(day = date)

hosp <-
  read_rds(here(
    "data",
    "urg_and_emerg_num_hosp_by_day_by_county_inc_state_dec_1.RDS"
  )) %>%
  select(five_digit_fips = county,
         day = admission_date,
         n_all_cvd:n_resp_2_sex) 

outage_exposure <- 
  read_parquet(
    here(
      'data_for_upload',
      'power_outage_exposure_data_cleaning_output',
      'analytic_exposure_data_2018.parquet'
    )
  ) %>%
  select(five_digit_fips, 
         day, 
         exposed_1_hrs_percentile:exposed_1_hrs_0.05, 
         county_customers, 
         percent_served) %>%
  mutate(day = as.Date(day))

meteo <- read_parquet(here('data', 'meteo_vars.parquet')) %>%
  select(five_digit_fips, day = observation_date, max_temp:wind_speed)

denoms <- read_rds(here('data', 'benes_by_county_fips_dec_1.RDS')) %>%
  select(five_digit_fips = county, n_benes:n_benes_sex_2)

# Do ----------------------------------------------------------------------

an_dat <- 
  panel_fips %>%
  left_join(outage_exposure) %>%
  left_join(meteo) %>%
  left_join(hosp) %>%
  left_join(denoms)

length(unique(an_dat$five_digit_fips))
# 3143 to start 

# missing values have different meanings.
# when power outage exposure is missing, this means we had insufficient exposure 
# data for those counties and so we should exclude them 

an_dat <- an_dat %>% filter(!is.na(exposed_1_hrs_0.005))
length(unique(an_dat$five_digit_fips))
# excludes 135 for 3008 left

# filter for low percent served
an_dat <- an_dat %>% filter(percent_served >= 0.5 & !is.na(percent_served))
length(unique(an_dat$five_digit_fips))
# excludes 772 counties 

# when hospitalizations are missing, that means that there were no 
# hospitalizations on those days, so we should set those to 0

an_dat <-
  an_dat %>%
  mutate_at(
    vars(n_all_cvd:n_benes_sex_2),
    ~ ifelse(is.na(.), 0, .)
  )
length(unique(an_dat$five_digit_fips))

# there are some counties where the medicare beneficiary counts are missing. 
# these are in virginia, for small towns. idk what is going on here, 
# need to find out, but for now we'll exclude those as well as places with
# fewer than 500 benes

an_dat <- an_dat %>% filter(n_benes > 500) # this removes 133 counties 
# honestly the lines are not super stable above 500 either so not sure
# what joan wants me to do about that. 
length(unique(an_dat$five_digit_fips))
# excludes 75 counties 

# add strata 
an_dat[, day_of_week := lubridate::wday(day)]
an_dat[, two_month_period := cut(day, breaks = "2 months", labels = FALSE)]
an_dat[, stratum := .GRP, by = .(five_digit_fips, day_of_week, two_month_period)]

# filter out lagged nas
#an_dat <- an_dat[complete.cases(an_dat),]

# Write -------------------------------------------------------------------

write_rds(an_dat, here('data', 'an_dat_urgent_hosp_dec_1.RDS'))
