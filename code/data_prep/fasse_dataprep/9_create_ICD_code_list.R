# ICD code file for someone to check over to make sure that the codes we've
# chosen actually represent what we want the codes to represent

pacman::p_load(here, tidyverse, arrow, data.table)

# Helper ------------------------------------------------------------------

# function helps identify hospitalizations that contain any icd code from 
# a predetermined list of codes 
contains_any_code <- function(codes, condition) {
  as.integer(Reduce(`|`, lapply(codes, `%chin%`, condition)))
}

# read in panel fips
panel_fips <- read_rds(here('data_for_upload', 'panel_for_2018.RDS')) |>
  rename(day = date)

# read in hospitalization data
hosp_2018 <-
  arrow::read_parquet(here('data', "medpar_hospitalizations_2018.parquet")) |>
  as.data.table()

# filter to hospitalizations actually dated in 2018
hosp_info <- hosp_2018 |>
  mutate(admission_date = as.Date(admission_date)) |>
  filter(admission_date %in% panel_fips$day)

# get the first five ICD codes
hosp_info <-
  hosp_info[, paste0("code_", 1:5) := transpose(lapply(diagnoses, function(x) {
    length(x) <- 5
    x
  }))]

all_codes <- c(
  hosp_info$code_1,
  hosp_info$code_2,
  hosp_info$code_3,
  hosp_info$code_4,
  hosp_info$code_5
)

# this is all codes from the data 
all_codes_from_data <- sort(unique(all_codes))

# these are the hypertension codes
hypertension_codes <- c("I10", "I110", "I119", "I120", "I129", "I130",
  "I1310", "I1311", "I132", "I150", "I151", "I152", "I158", "I159", "I160",
  "I161", "I169")

# get all resp codes - all codes beginning with J
all_resp_codes <- all_codes_from_data[grep("^J", all_codes_from_data)]

# get all cvd codes - all codes beginning with I 
all_cvd_codes <- all_codes_from_data[grep("^I", all_codes_from_data)]

# get cvd codes with no hypertension
cvd_no_hyp <- setdiff(all_cvd_codes, hypertension_codes)

icd_codes <- list(
  all_cvd = all_cvd_codes,
  all_resp = all_resp_codes,
  cvd_no_hyp = cvd_no_hyp,
  hyp_codes = hypertension_codes
)

write_rds(icd_codes, here("data", "all_icd_codes.RDS"))



