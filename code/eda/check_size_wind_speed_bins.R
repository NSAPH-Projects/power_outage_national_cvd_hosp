# Check size of wind speed bins

pacman::p_load(here, tidyverse, arrow)

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp.RDS'))

o <- an_dat %>% 
  mutate(p_wind_speed = ntile(wind_speed, 10)) %>% 
  group_by(p_wind_speed) %>% 
  summarize(mean_outage = mean(exposed_8_hrs_0.005))

plot(o$p_wind_speed, o$mean_outage)

# what i'm getting from this plot is that the relationship is not linear and 
# bins 8, 9, and 10 all need their own bins. so going to do 4-category var
# with deciles 1-7, 8, 9, and 10

