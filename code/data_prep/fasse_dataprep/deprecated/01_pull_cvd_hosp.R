# This file reads in the medpar denominator and hospitalization files for
# 2018 and pulls the hospitalizations related to CVD

# going to tally them in this script to save memory
# need daily hospitalization counts by day for each county for 2018
# panel

# By: Heather, September 20th, 2024

# Libraries ---------------------------------------------------------------

pacman::p_load(here, tidyverse, arrow, data.table)

# Read files --------------------------------------------------------------

panel_fips <- read_rds(here('data_for_upload', 'panel_for_2018.RDS')) %>%
  rename(day = date)

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

# Pull ICD codes ----------------------------------------------------------

# filter to hospitalizations actually dated in 2018
hosp_info <- hosp_info %>% mutate(admission_date = as.Date(admission_date)) %>%
  filter(admission_date %in% panel_fips$day)

# get the first five ICD codes
hosp_info <-
  hosp_info[, paste0("code_", 1:5) := transpose(lapply(diagnoses, function(x) {
    length(x) <- 5
    x
  }))]

# define codes from icd codes file
all_cvd <- icd_codes$all_cvd # all cvd codes, including hypertension and hem stroke
cvd_no_hem_no_hyp <- icd_codes$cvd_no_hem_no_hyp # cvd without hem stroke or hypertension
resp <- icd_codes$resp # all respiratory codes
# stroke and MI codes to check our work
stroke <- c(
  icd_codes$istroke_icd_9,
  icd_codes$istroke_icd_10,
  icd_codes$hem_stroke_icd_10,
  icd_codes$hem_stroke_icd_9
)
mi <- c(icd_codes$mi_icd_10, icd_codes$mi_icd_9)


# indicate hospitalizations with the codes
hosp_info <- hosp_info[, contains_any_cvd_code := as.integer(
  code_1 %chin% all_cvd |
    code_2 %chin% all_cvd |
    code_3 %chin% all_cvd |
    code_4 %chin% all_cvd |
    code_5 %chin% all_cvd
)]

hosp_info <- hosp_info[, contains_cvd_no_hem_no_hyp := as.integer(
  code_1 %chin% cvd_no_hem_no_hyp |
    code_2 %chin% cvd_no_hem_no_hyp |
    code_3 %chin% cvd_no_hem_no_hyp |
    code_4 %chin% cvd_no_hem_no_hyp |
    code_5 %chin% cvd_no_hem_no_hyp
)]

hosp_info <- hosp_info[, contains_resp_code := as.integer(
  code_1 %chin% resp |
    code_2 %chin% resp |
    code_3 %chin% resp |
    code_4 %chin% resp |
    code_5 %chin% resp
)]

hosp_info <- hosp_info[, contains_stroke_code := as.integer(
  code_1 %chin% stroke |
    code_2 %chin% stroke |
    code_3 %chin% stroke |
    code_4 %chin% stroke |
    code_5 %chin% stroke
)]

hosp_info <- hosp_info[, contains_mi_code := as.integer(
  code_1 %chin% mi |
    code_2 %chin% mi |
    code_3 %chin% mi |
    code_4 %chin% mi |
    code_5 %chin% mi
)]

# identify emergency vs non emergency
hosp_info[, `:=`(
  is_emergency = ifelse(admsn_type_cd == 1, 1, 0),
  is_urg_or_emerg = ifelse(admsn_type_cd == 1 | admsn_type_cd == 2, 1, 0)
)]

# identify hosp by age and sex 
hosp_info[, `:=` (
  age = ifelse(age_dob >= 75, 1, 0)
)]

# summarize to day-county level
hosp_by_day_by_county <- hosp_info[, .(
  n_all_cvd = sum(contains_any_cvd_code),
  n_cvd_no_hem_no_hyp = sum(contains_cvd_no_hem_no_hyp),
  n_resp = sum(contains_resp_code),
  n_stroke = sum(contains_stroke_code),
  n_mi = sum(contains_mi_code)),
  by = .(admission_date, county, state)
]

# for emergency hosp only
emerg_hosp_by_day_by_county <- hosp_info[is_emergency == 1]

emerg_hosp_by_day_by_county <- emerg_hosp_by_day_by_county[, .(
  n_all_cvd = sum(contains_any_cvd_code),
  n_cvd_no_hem_no_hyp = sum(contains_cvd_no_hem_no_hyp),
  n_resp = sum(contains_resp_code),
  n_stroke = sum(contains_stroke_code),
  n_mi = sum(contains_mi_code)),
  by = .(admission_date, county, state)
]


# for urgent hosp only
urgent_hosp_by_day_by_county <- hosp_info[is_urg_or_emerg == 1]

urg_hosp_by_sex <- urgent_hosp_by_day_by_county[sex != 0, .(
  n_all_cvd = sum(contains_any_cvd_code),
  n_cvd_no_hem_no_hyp = sum(contains_cvd_no_hem_no_hyp),
  n_resp = sum(contains_resp_code),
  n_stroke = sum(contains_stroke_code),
  n_mi = sum(contains_mi_code)
),
by = .(admission_date, county, state, sex)]

urg_hosp_by_sex <-
  dcast(
    urg_hosp_by_sex,
    admission_date + county + state ~ sex,
    value.var = c(
      "n_all_cvd",
      "n_cvd_no_hem_no_hyp",
      "n_resp",
      "n_stroke",
      "n_mi"
    )
  )

setnames(urg_hosp_by_sex, old = names(urg_hosp_by_sex)[-(1:3)], 
         new = paste0(names(urg_hosp_by_sex)[-(1:3)], "_sex"))

urg_hosp_by_age <- urgent_hosp_by_day_by_county[, .(
  n_all_cvd = sum(contains_any_cvd_code),
  n_cvd_no_hem_no_hyp = sum(contains_cvd_no_hem_no_hyp),
  n_resp = sum(contains_resp_code),
  n_stroke = sum(contains_stroke_code),
  n_mi = sum(contains_mi_code)
),
by = .(admission_date, county, state, age)]

urg_hosp_by_age <-
  dcast(
    urg_hosp_by_age,
    admission_date + county + state ~ age,
    value.var = c(
      "n_all_cvd",
      "n_cvd_no_hem_no_hyp",
      "n_resp",
      "n_stroke",
      "n_mi"
    )
  )

setnames(urg_hosp_by_age, old = names(urg_hosp_by_age)[-(1:3)], 
         new = paste0(names(urg_hosp_by_age)[-(1:3)], "_age"))

urgent_hosp_by_day_by_county <- urgent_hosp_by_day_by_county[, .(
  n_all_cvd = sum(contains_any_cvd_code),
  n_cvd_no_hem_no_hyp = sum(contains_cvd_no_hem_no_hyp),
  n_resp = sum(contains_resp_code),
  n_stroke = sum(contains_stroke_code),
  n_mi = sum(contains_mi_code)),
  by = .(admission_date, county, state)
]

urgent_hosp_by_day_by_county <- 
  urgent_hosp_by_day_by_county %>%
  left_join(urg_hosp_by_age) %>% 
  left_join(urg_hosp_by_sex)

# Write -------------------------------------------------------------------

write_rds(hosp_by_day_by_county,
          here("data", "num_hosp_by_day_by_county_inc_state.RDS"))

write_rds(emerg_hosp_by_day_by_county,
          here("data", "emerg_num_hosp_by_day_by_county_inc_state.RDS"))

write_rds(urgent_hosp_by_day_by_county,
          here("data", "urg_num_hosp_by_day_by_county_inc_state.RDS"))

# write denom, for secondary analyses also
benes_2018[, age_group := ifelse(age_dob >= 75, "older_75", "under_75")]
benes_2018[, sex_group := ifelse(sex == '1', "sex_1", "sex_2")]

# summarize
benes_summary <- benes_2018[, .(
  n_benes = .N,
  n_benes_older_75 = sum(age_group == "older_75"),
  n_benes_under_75 = sum(age_group == "under_75"),
  n_benes_sex_1 = sum(sex_group == "sex_1"),
  n_benes_sex_2 = sum(sex_group == "sex_2")
), by = .(county, state)]

write_rds(benes_summary, here('data', 'benes_by_county_fips.RDS'))
