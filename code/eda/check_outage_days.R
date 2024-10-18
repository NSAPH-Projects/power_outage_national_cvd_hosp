
pacman::p_load(arrow, data.table, tidyverse, here, fst)

outages <- read_parquet(
  here(
    'data',
    'power_outage_exposure_data_cleaning_output',
    'analytic_exposure_data_all_years.parquet'
  )
)

denoms <- read_fst(
  here(
    'data',
    'power_outage_exposure_data_cleaning_output',
    "county_customer_denoms_and_p_missing.fst"
  )
)

outages <- outages %>% left_join(denoms)

outages <- outages %>% filter(percent_served > 0.5 & county_customers > 1000)


day_of_week <- outages %>%
  mutate(day_of_week = lubridate::wday(day)) %>%
  group_by(five_digit_fips, year, day_of_week) %>% 
  summarize(num_outages = sum(exposed_8_hrs_0.005))

day_of_week %>%
  group_by(day_of_week) %>%
  summarize(n = mean(num_outages)) %>%
  knitr::kable()
      
p2 <- day_of_week %>%
  ggplot() +
  geom_jitter(aes(x = day_of_week, y = num_outages),
              alpha = 0.02,
              size = 3) +
  facet_grid(~ year)

ggsave(p2,
       filename = here('figures', 
                       'figures_output', 
                       'mean_outages_by_weekday.png'))


# conclusion: it seems that there are slightly more outages on mondays, 
# thursdays, fridays, and saturdays. 

# plan: stratify by month and weekday, but then also we could do another 
# analysis if we need more power where we try to do stratification on 
# MTFS, vs TWTh

