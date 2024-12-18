# Create analytic data from exposure and outcome for urgent hospitalizations

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow)

# Read --------------------------------------------------------------------

panel_fips <- read_rds(here('data_for_upload', 'panel_for_2018.RDS')) %>%
  rename(day = date)

hosp <-
  read_rds(here(
    "data",
    "urg_and_emerg_num_hosp_by_day_by_county_inc_state_dec_13.RDS"
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

i <- an_dat %>% filter(n_benes > 500)
# 2964
# 178 have < 500

j <- an_dat %>% filter(n_benes > 500 & !is.na(exposed_8_hrs_0.01) & percent_served >=0.5)
length(unique(j$five_digit_fips))
# 804 excluded due to missing exposure data 

# missing values have different meanings.
# when power outage exposure is missing, this means we had insufficient exposure 
# data for those counties and so we should exclude them 

an_dat <- an_dat %>% filter(!is.na(exposed_1_hrs_0.005))
length(unique(an_dat$five_digit_fips))
# excludes 134 for 3009 left

# filter for low percent served
an_dat <- an_dat %>% filter(percent_served >= 0.5 & !is.na(percent_served))
length(unique(an_dat$five_digit_fips))
# excludes 773 counties 
# in total excluded 907 counties 

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

write_rds(an_dat, here('data', 'an_dat_urgent_hosp_dec_17.RDS'))


# descriptive stats
min(an_dat$n_benes) # 501
max(an_dat$n_benes) # 252,004
mean(an_dat$n_benes) # 10,931

# mean outages
outages <- an_dat %>% group_by(five_digit_fips) %>% 
  summarize(n = sum(exposed_8_hrs_0.01))
mean(outages$n)
sd(outages$n)
# mean outages 7.39
# sd = 28.29


benes <- an_dat %>% group_by(five_digit_fips) %>%
  summarize(benes = max(n_benes))
sum(benes$benes)
# total benes covered: 23622770

benes_started <- 
  denoms %>%
  select(five_digit_fips, n_benes) %>%
  unique()

sum(benes_started$n_benes)
# 33242414
# 71.06 percent of benes covered 

pserved <- an_dat %>% 
  select(five_digit_fips, percent_served) %>%
  distinct() %>% 
  mutate(served = ifelse(percent_served > 1, 1, percent_served))

mean(pserved$served)
# mean coverage in remaining counties was 93% 
# 93% of county-hours present 

mean(an_dat$n_benes, na.rm = T) * 0.01
# on average outages affect 109.3 or more benes 

sum(an_dat$exposed_8_hrs_0.01)
# total number of outages is 15990

length(an_dat$exposed_8_hrs_0.01)
# total days is 788765

# means that there were 2.02% of days affected by outage 

mean(an_dat$n_cvd_no_hyp) # 3.23
mean(an_dat$n_resp) # 2.25
