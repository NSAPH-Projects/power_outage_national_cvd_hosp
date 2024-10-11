
pacman::p_load(arrow, data.table, tidyverse, here)

outages <- read_parquet(here('data', 'analytic_exposure_data_2018.parquet'))

outages %>% ggplot() + geom_point(aes(x = day, y = n_hrs_out_0.01))
