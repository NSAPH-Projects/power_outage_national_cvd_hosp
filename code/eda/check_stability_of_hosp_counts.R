# Check to see where hospitalization counts are stable

pacman::p_load(tidyverse, data.table, here)

hosp <- 
  read_rds(here("data", "num_hosp_by_day_by_county_inc_state.RDS"))

benes <- 
  read_rds(here('data', 'benes_by_county_fips.RDS'))

hosp <- hosp %>% left_join(benes)

hosp %>%
  ggplot() +
  geom_line(aes(x = admission_date, y = n_cvd, color = county))

hosp_100 <- hosp %>% filter(n_benes < 100)
hosp_300 <- hosp %>% filter(n_benes < 300)
hosp_500 <- hosp %>% filter(n_benes < 500)
hosp_1000_2000 <- hosp %>% filter(n_benes > 1000 & n_benes < 2000)

hosp_1000_2000 %>%
  ggplot() +
  geom_line(aes(x = admission_date, y = n_cvd, color = county)) +
  theme(legend.position = "none")
