# This file reads in the medpar denominator and hospitalization files for
# 2018 and pulls the hospitalizations related to CVD

# going to tally them in this script to save memory
# need daily hospitalization counts by day for each county for 2018
# panel


# Libraries ---------------------------------------------------------------

library(here)
library(tidyverse)
library(arrow)
library(data.table)
library(sf)
library(tigris)


# Read files --------------------------------------------------------------

# read in medicare hospitalizations and beneficiaries for 2018
benes_2018 <-
  arrow::read_parquet(here("data", "mbsf_medpar_denom_2018.parquet")) |>
  as.data.table()

hosp_2018 <-
  arrow::read_parquet(here('data', "medpar_hospitalizations_2018.parquet")) |>
  as.data.table()

icd_codes <- readRDS(here("data", "all_icd_codes.RDS"))

# join metadata to hospitalizations that did occur in 2018
hosp_info <- merge(hosp_2018, benes_2018, by = "bene_id", all.x = TRUE)

# idk if this is helping idk how this server works
rm(hosp_2018, benes_2018)

# for sampling for writing
# hosp_info_sample <- hosp_info[sample(.N, 1000)]
# write_rds(hosp_info_sample, here('data', "working_medicare_sample.RDS"))

# Pull ICD codes ----------------------------------------------------------

# get the first five ICD codes
hosp_info <-
  hosp_info[, paste0("code_", 1:5) := transpose(lapply(diagnoses, function(x) {
    length(x) <- 5
    x
  }))]

# define codes from icd codes file
cvd <- icd_codes$cvd_no_hem_no_hyp
resp <- icd_codes$resp

# indicate hospitalizations with the codes
hosp_info <- hosp_info[, contains_cvd_code := as.integer(
  code_1 %in% cvd |
    code_2 %in% cvd |
    code_3 %in% cvd |
    code_4 %in% cvd |
    code_5 %in% cvd
)]

hosp_info <- hosp_info[, contains_resp_code := as.integer(
  code_1 %in% resp |
    code_2 %in% resp |
    code_3 %in% resp |
    code_4 %in% resp |
    code_5 %in% resp
)]

# summarize to day-county level
hosp_by_day_by_county <- hosp_info[, .(
  n_cvd = sum(contains_cvd_code),
  n_resp = sum(contains_resp_code)),
  by = .(admission_date, county, state)
]


# Write -------------------------------------------------------------------

write_rds(hosp_by_day_by_county, here("data", "num_hosp_by_day_by_county_inc_state.RDS"))


# Bonus plots -------------------------------------------------------------

# about how many hospitalizations would we expect to be due to CVD?
# how many due to respiratory issues?

# can we count hospitalizations by county?

# plot hospitalization counts by county
hosp_by_county_2018 <- hosp_info[, .(
  n_cvd = sum(contains_cvd_code),
  n_resp = sum(contains_resp_code)
), by = county]


# read in counties
us_counties <- counties(year = 2018)

# define the FIPS and letter codes for Alaska, Hawaii, and other territories
excluded_states <- c("02", "15", "60", "66", "69", "72", "78")
excluded_states_letters <- c("AK", "HI", "AS", "GU", "MP", "PR", "VI")

# clean slightly
conus_counties <- us_counties %>%
  filter(!STATEFP %in% excluded_states) %>%
  mutate(county = paste0(STATEFP, COUNTYFP)) %>%
  select(county)

# filter hosp to conus
hosp_by_county_2018 <- hosp_by_county_2018[!state %in% excluded_states_letters]

# join
plot_hosp_county <- conus_counties %>% left_join(hosp_by_county_2018)

# plot
p1 <- plot_hosp_county %>% ggplot() + geom_sf(aes(fill = n_cvd))
p2 <- plot_hosp_county %>% ggplot() + geom_sf(aes(fill = n_resp))

hosp_by_state_by_day <- hosp_info[, .(
  n_cvd = sum(contains_cvd_code),
  n_resp = sum(contains_resp_code),
  by = .(admission_date, state)
)]

library(tigris)
us_counties <- counties(year = 2018)

# Define the FIPS codes for Alaska, Hawaii, and other territories
excluded_states <- c("02", "15", "60", "66", "69", "72", "78")

excluded_states_letters <- c("AK", "HI", "AS", "GU", "MP", "PR", "VI")
hosp_info_sample <- hosp_info_sample %>% filter(!(state %in% excluded_states_letters))

conus_counties <- us_counties %>%
  filter(!STATEFP %in% excluded_states)

conus_counties <- conus_counties %>% mutate(county = paste0(STATEFP, COUNTYFP)) %>% select(county)

cc <- conus_counties %>% left_join(hosp_by_county_2018)
hosp_by_county_2018 <- hosp_by_county_2018 %>% left_join(conus_counties)

l <- cc %>% ggplot() + geom_sf(aes(fill = n_cvd))
ll <- cc %>% ggplot() + geom_sf(aes(fill = n_resp))



library(sf)
plot(st_geometry(conus_counties))


