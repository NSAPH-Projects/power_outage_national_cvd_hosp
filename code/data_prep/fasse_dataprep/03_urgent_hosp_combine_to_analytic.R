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


# descriptive stats
min(an_dat$n_benes) # 8
max(an_dat$n_benes) # 252,004
mean(an_dat$n_benes) # 10,584

# VIVIAN I need these numbers
# mean outages
outages <- an_dat %>% group_by(five_digit_fips) %>% 
  summarize(n = sum(exposed_8_hrs_0.01))
mean(outages$n)
sd(outages$n)
median(outages$n)
IQR(outages$n)
quantile(outages$n, c(0.05, 0.25, 0.75, 0.95))

# mean outages 7.67
# sd = 29.20


benes <- an_dat %>% group_by(five_digit_fips) %>%
  summarize(benes = max(n_benes))
sum(benes$benes)
# total benes covered: 23645101

benes_started <- 
  denoms %>%
  select(five_digit_fips, n_benes) %>%
  unique()

sum(benes_started$n_benes)
# 33242414
# 71.12 percent of benes covered 

pserved <- an_dat %>% 
  select(five_digit_fips, percent_served) %>%
  distinct() %>% 
  mutate(served = ifelse(percent_served > 1, 1, percent_served))

mean(pserved$served)
# mean coverage in remaining counties was 93% 
# 93% of county-hours present 

mean(an_dat$n_benes, na.rm = T) * 0.01
# on average outages affect 106 or more benes 

sum(an_dat$exposed_8_hrs_0.01)
# total number of outages is 17,148

length(an_dat$exposed_8_hrs_0.01)
# total days is 815410

# means that there were 2.10% of days affected by outage 

mean(an_dat$n_cvd_no_hyp) # 3.12
mean(an_dat$n_resp) # 2.18


# Get high coverage -------------------------------------------------------


an_dat_low_missingness <-
  an_dat_low_missingness %>%
  mutate_at(
    vars(n_all_cvd:n_benes_non_eligible),
    ~ ifelse(is.na(.), 0, .)
  )
length(unique(an_dat_low_missingness$five_digit_fips))

# add strata 
an_dat_low_missingness[, day_of_week := lubridate::wday(day)]
an_dat_low_missingness[, two_month_period := cut(day, breaks = "2 months", labels = FALSE)]
an_dat_low_missingness[, stratum := .GRP, by = .(five_digit_fips, day_of_week, two_month_period)]

# filter out lagged nas
#an_dat <- an_dat[complete.cases(an_dat),]

# Write -------------------------------------------------------------------

write_rds(
  an_dat_low_missingness,
  here('data', 'an_dat_urgent_hosp_LOW_MISSINGNESS_SENS.RDS')
)

