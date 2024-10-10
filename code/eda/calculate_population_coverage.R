# Calculate how much we are covering

pacman::p_load(tidyverse, here, fst)

# ok so the continental US as we've defined it has 3107 county fips codes 
# that covers everyone and so those are the customers we want to use as the
# denominator.

# eia total denom
eia_customer_estimates <- read_rds(here(
  "data",
  "power_outage_exposure_data_cleaning_output",
  "downscaled_county_customer_estimates.RDS"
))

eia <-
  eia_customer_estimates %>%
  group_by(year) %>%
  summarize(total_customers_from_eia =
              sum(downscaled_county_estimate, na.rm = T))

# pous denoms
denoms <- read_fst(
  here(
    'data',
    'power_outage_exposure_data_cleaning_output',
    'county_customer_denoms_and_p_missing.fst'
  )
)

denoms <- denoms %>%
  mutate(percent_served = ifelse(percent_served > 1, 1, percent_served),
    customer_count_served = county_customers * percent_served,
    customer_count_wo_less_50_p = 
      ifelse(percent_served < 0.5, NA_real_, county_customers),
    customer_count_served_wo_less_50_p = 
      customer_count_wo_less_50_p * percent_served
  )

total_customers <-
  denoms %>%
  group_by(year) %>%
  summarize(
    total_cust = sum(county_customers, na.rm = T),
    total_cust_coverage = sum(customer_count_served, na.rm = T),
    total_cust_coverage_w_exclusion = 
      sum(customer_count_served_wo_less_50_p, na.rm = T),
    proportion_served = total_cust_coverage / total_cust,
    proportion_served_w_exclusion = total_cust_coverage_w_exclusion / total_cust
  )

total_customers <- total_customers %>%
  left_join(eia) %>%
  mutate(proportion_served_eia_denom =
           total_cust_coverage_w_exclusion / total_customers_from_eia)

total_customers %>% knitr::kable()

# we need to account for counties that never even made it into this dataset  
# so that's annoying but coverage is still high 
# need to go back to the file that had the hhs and businesses and get the denom
# from there. 


# eia totals
eia_totals <-
  read_rds(
    here(
      "data",
      "power_outage_exposure_data_cleaning_output",
      "eia_state_total_customers_by_year.RDS"
    )
  ) 

eia_totals <- 
  eia_totals %>%
  group_by(year) %>%
  summarize(n_cust = sum(eia_state_total_cust, na.rm = T))



