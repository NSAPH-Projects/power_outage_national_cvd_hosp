# This file reads in the medpar denominator and hospitalization files for
# 2018 and pulls the hospitalizations related to CVD

library(here)
library(tidyverse)
library(arrow)
library(data.table)



# read in medicare hospitalizations and beneficiaries for 2018
benes_2018 <-
  arrow::read_parquet(here("data", "mbsf_medpar_denom_2018.parquet")) |>
  as.data.table()
hosp_2018 <-
  arrow::read_parquet(here('data', "medpar_hospitalizations_2018.parquet")) |>
  as.data.table()


xwalk <- zctaCrosswalk::get_zcta_crosswalk()

# EDA questions
hist(benes_2018$age_dob)

sort(unique(benes_2018$age_dob))

sum(benes_2018$sex == 1)

sum(benes_2018$sex == 2)

sum(!(benes_2018$sex == 1 | benes_2018$sex == 2))

sort(unique(benes_2018$race))

# when sex is missing its value is 0
# when race is missing its value is also 0

# look at racial breakdown
counts <- benes_2018[, .N, by = race]
counts <- benes_2018[, .N, by = race_rti]

# about 3% missing, 82% white? is that correct?
# 2% black
# 1% other
# 1% asian, 1% hispanic 0.3% Native American

# changes quite a bit between this and the research triangle one
# how does HMO status matter?
# and what is OREC and how does it matter?

# hospitalization file
# have to link to bene id

ids_hosp <- unique(hosp_2018$bene_id)
ids_bene <- unique(benes_2018$bene_id)

sum(ids_hosp %in% ids_bene)
# okay good match


hosp_info <- merge(hosp_2018, benes_2018, by = "bene_id", all.x = TRUE)

# how many hospitalizations by county and by month
hosp_info %>% group_by(county) %>% summarize(n = n())

hosp_info %>% group_by(admission_date) %>% summarize(n = n())


# add county info to hosp_info

# ok want to get the already pulled out codes i feel like that would be helpful
# and then summarize to a small file that we can save and work with


# how many hospitalizations by county
# how many hospitalizations by month
# any counties or days with 0 hospitazliations?

View(benes_2018[1:1000,])


# nrow(benes %>% group_by(year, bene_id) %>% filter(row_number() == 1))

# join hosp and benes
hosp_2017_2018 <- hosp %>%
  mutate(admission_year = year(admission_date)) %>%
  filter(admission_year %in% c("2017", "2018")) %>%
  select(-year)

hosp_2017_2018_w_benes <- left_join(hosp_2017_2018, benes, by = c("bene_id" = "bene_id", "admission_year" = "year")) %>%
  filter(!is.na(zip))

# create diagnosis variables - un-array diagnosis variable bc diagnosis var originally array

# Loop through each row and extract diag position codes

# Initialize lists to store extracted values for each position
# Create empty lists for each position
max_positions <- max(sapply(hosp_2017_2018_w_benes$diagnoses, length))
position_lists <- lapply(1:max_positions, function(x) list())

# Iterate over each row
for (i in 1:nrow(hosp_2017_2018_w_benes)) {
  row <- hosp_2017_2018_w_benes$diagnoses[[i]]

  # Iterate over each position
  for (j in 1:max_positions) {
    if (length(row) >= j) {
      position_lists[[j]][[i]] <- row[j]
    } else {
      position_lists[[j]][[i]] <- "999999999"
    }
  }
}

# Assign the lists to new columns in the dataframe
for (j in 1:max_positions) {
  col_name <- paste0("diag", j)
  hosp_2017_2018_w_benes[[col_name]] <- unlist(position_lists[[j]])
}

hosp_2017_2018_w_benes <- hosp_2017_2018_w_benes %>%
  select(bene_id, admission_date, discharge_date, admission_year, state, zip, zcta, age_dob, sex, starts_with("diag"), -diagnoses)

write_fst(hosp_2017_2018_w_benes, paste0(path_data, "data_process_falls/hosp_2017_2018_w_benes_all_hosp.fst"))


