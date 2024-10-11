# Create analytic data from exposure and outcome for urgent hospitalizations

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow)

# Read --------------------------------------------------------------------

panel_fips <- read_rds(here('data', 'panel_for_2018.RDS')) %>%
  rename(day = date)

hosp <- 
  read_rds(here("data", "num_hosp_by_day_by_county_inc_state.RDS")) %>%
  select(five_digit_fips = county, 
         day = admission_date,
         n_cvd:n_mi) 

outage_exposure <- 
  read_parquet(here('data', 'analytic_exposure_data_2018.parquet')) %>%
  select(five_digit_fips, 
         day, 
         exposed_1_hrs_percentile:exposed_1_hrs_0.05, 
         county_customers, 
         percent_served) %>%
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
# when power outage exposure is missing, this means we had insufficient exposure 
# data for those counties and so we should exclude them 

an_dat <- an_dat %>% filter(!is.na(exposed_1_hrs_0.005))

# filter for low percent served
an_dat <- an_dat %>% filter(percent_served > 0.5 & !is.na(percent_served))

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
# honestly the lines are not super stable above 500 either so not sure
# what joan wants me to do about that. 

# add lags - across all cut points and all durations
columns_to_lag <- colnames(an_dat)[3:21]

# lag all
for (col in columns_to_lag) {
  an_dat[, paste0(col, "_lag_1") := shift(get(col), n = 1, type = "lag"), 
         by = five_digit_fips]
  an_dat[, paste0(col, "_lag_2") := shift(get(col), n = 2, type = "lag"), 
         by = five_digit_fips]
  an_dat[, paste0(col, "_lag_3") := shift(get(col), n = 3, type = "lag"), 
         by = five_digit_fips]
  an_dat[, paste0(col, "_lag_4") := shift(get(col), n = 4, type = "lag"), 
         by = five_digit_fips]
  an_dat[, paste0(col, "_lag_5") := shift(get(col), n = 5, type = "lag"), 
         by = five_digit_fips]
  an_dat[, paste0(col, "_lag_6") := shift(get(col), n = 6, type = "lag"), 
         by = five_digit_fips]
}

# add strata 
an_dat[, day_of_week := lubridate::wday(day)]
an_dat[, two_month_period := cut(day, breaks = "2 months", labels = FALSE)]
an_dat[, stratum := .GRP, by = .(five_digit_fips, day_of_week, two_month_period)]

# Write -------------------------------------------------------------------

write_rds(an_dat, here('data', 'an_dat_all_hosp.RDS'))
