
pacman::p_load(arrow, data.table, tidyverse, here, fst)

outages <- read_parquet(
  here(
    'data',
    'analytic_exposure_data_2018.parquet'
  )
)

ts <-
  outages %>% 
  select(day, five_digit_fips, exposed_8_hrs_0.01) %>%
  group_by(five_digit_fips) %>%
  group_split()

acfs <- list()
for (i in 1:length(ts)){
  l <- acf(ts[[i]]$exposed_8_hrs_0.01, plot = F)
  l <- l$acf[2:6]
  acfs[[i]] <- l
}

num_elements <- length(acfs[[1]])

# Compute the mean for each index
mean_by_index <- sapply(1:num_elements, function(i) {
  mean(sapply(acfs, function(x) x[i]), na.rm = T)
})

# Print the result
print(mean_by_index)

# check if there are weekday differences 
