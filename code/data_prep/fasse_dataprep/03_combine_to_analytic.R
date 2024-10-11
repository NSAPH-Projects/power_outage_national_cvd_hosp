# Create analytic dataset from exposure and outcome data

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow)

# Read --------------------------------------------------------------------

panel_fips <- read_rds(here('data', 'panel_for_2018.RDS')) %>%
  rename(day = date)

hosp <- 
  read_rds(here("data", "emerg_num_hosp_by_day_by_county_inc_state.RDS")) %>%
  select(five_digit_fips = county, 
         day = admission_date,
         n_cvd:n_mi) 

outage_exposure <- 
  read_parquet(here('data', 'analytic_exposure_data_2018.parquet')) %>%
  select(five_digit_fips, day, exposed_1_hrs_percentile:exposed_1_hrs_0.05) %>%
  mutate(day = as.Date(day))

meteo <- read_parquet(here('data', 'meteo_vars.parquet')) %>%
  select(five_digit_fips, day = observation_date, max_temp:wind_speed)

denoms <- read_rds(here('data', 'benes_by_county_fips.RDS')) %>%
  select(five_digit_fips = county, n_benes)

# Do ----------------------------------------------------------------------

an_dat <- 
  panel_fips %>%
  left_join(outage_exposure) %>%
  left_join(meteo) %>%
  left_join(hosp) %>%
  left_join(denoms)

# missing values have different meanings.
# when power outage exposure is missing, this means we had insufficent exposure 
# data for those counties and so we should exclude them 

an_dat <- an_dat %>% filter(!is.na(exposed_1_hrs_0.005))

# when hospitalizations are missing, that means that there were no 
# hospitalizations on those days, so we should set those to 0

an_dat <- 
  an_dat %>%
  mutate_at(vars(n_cvd, n_resp, n_stroke, n_mi, n_benes),
            ~ ifelse(is.na(.), 0, .))

# there are some counties where the medicare beneficiary counts are missing. 
# these are in virginia, for small towns. idk what is going on here, 
# need to find out, but for now we'll exclude those as well as places with
# fewer than 500 benes

an_dat <- an_dat %>% filter(n_benes > 500) # this removes 133 counties 

write_rds(an_dat, here('data', 'an_dat.RDS'))
