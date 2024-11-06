# Reads in empower data from excel and cleans it to get the quartiles of 
# percentages of benes that use DME and oxygen

# Author: Heather
# Date: Nov 6th, 2024


# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, here, readxl)


# Helper omg  -------------------------------------------------------------

find_quartiles <- function(x) {
  # calculate the quartile boundaries
  quartiles <- quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  
  # classify each number into quartiles
  quartile_labels <- findInterval(x, vec = c(-Inf, quartiles, Inf))
  
  return(quartile_labels)
}

# Load --------------------------------------------------------------------

empower <- read_xlsx(
  here(
    'local_data',
    'effect_mod_data',
    '2018_HHSemPOWERMapHistoricalDataset.xlsx'
  ), 
  sheet = 2
) %>%
  rename(five_digit_fips = FIPS_Code)

fips <- read_rds(here('data_for_upload', 'cotus_county_list_of_fips.RDS'))

# Do ----------------------------------------------------------------------

# add five digit fips 
empower_fips <- fips %>% left_join(empower)

# should exclude missing counties from analysis. not sure what the NAs mean
# oglala lakota county is missing and some counties in alaska are as well 

# going to take mean of benes and DME users over all months and use this 
empower_fips <-
  empower_fips %>%
  select(five_digit_fips,
         Jan_2018_Medicare_Benes:Dec_2018_Power_Dependent_Devices_DME)  

# get average benes
benes <- empower_fips %>%
  select(five_digit_fips, contains("Medicare_Benes")) %>%
  group_by(five_digit_fips) %>% 
  pivot_longer(cols = Jan_2018_Medicare_Benes:Dec_2018_Medicare_Benes) %>%
  group_by(five_digit_fips) %>%
  summarize(benes_2018 = round(mean(value)))

# get average dme users
dme_users <- empower_fips %>%
  select(five_digit_fips, contains("Power_Dependent_Devices_DME")) %>%
  group_by(five_digit_fips) %>%
  pivot_longer(cols = 
Jan_2018_Power_Dependent_Devices_DME:Dec_2018_Power_Dependent_Devices_DME) %>%
  group_by(five_digit_fips) %>%
  summarize(dme_2018 = round(mean(value)))

# join and calculate quartiles for each county
dme_use_data <- benes %>% left_join(dme_users)

dme_use_data <- dme_use_data %>%
  mutate(p_dme = dme_2018/benes_2018) %>%
  filter(!is.na(p_dme)) %>%
  mutate(dme_quartile = find_quartiles(p_dme))

# write
write_rds(dme_use_data,
          here(
            'data_for_upload',
            'effect_mod_data',
            'dme_use_by_county.RDS'
          ))

