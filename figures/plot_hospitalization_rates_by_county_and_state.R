# Plot hospitalization rates by county etc.

# libraries
library(here)
library(tidyverse)
library(sf)

# read in hospitalizations
k <- readRDS(here("data", "num_hosp_by_day_by_county_inc_state.RDS"))

k1 <- k %>% group_by(state, county) %>% summarize(n_cvd = sum(n_cvd),
                                                  n_resp = sum(n_resp))

# read in counties
us_counties <- counties_sf

# define the FIPS and letter codes for Alaska, Hawaii, and other territories
excluded_states <- c("02", "15", "60", "66", "69", "72", "78")
excluded_states_letters <- c("AK", "HI", "AS", "GU", "MP", "PR", "VI")

# clean slightly
conus_counties <- us_counties %>%
  rename(county = county_fips) %>%
  filter(!(state_fips %in% excluded_states))

# filter hosp to conus
k1 <- k1 %>% filter(!(state %in% excluded_states_letters))

# join
plot_hosp_county <- conus_counties %>% left_join(k1)

# plot
p1 <- plot_hosp_county %>% ggplot() + geom_sf(aes(fill = n_cvd))
p2 <- plot_hosp_county %>% ggplot() + geom_sf(aes(fill = n_resp))
