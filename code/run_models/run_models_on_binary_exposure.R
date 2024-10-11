# Run models on all cut points and durations of binary exposure for 
# emergency hospitalizations

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm)

# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_emergency_hosp.RDS'))
an_dat <- an_dat %>% mutate(all_hosp = n_cvd + n_resp)

# binary exposures 
exposure_columns <- c(colnames(an_dat)[10:21])

models <- list()

# fit models for each 'exposed_' column
for (col in exposure_columns) {
  # define formula
  formula <-
    as.formula(
      paste(
        "all_hosp ~",
        col,
        "+ max_temp + precip + wind_speed +",
        paste0(col, "_lag_1"),
        "+",
        paste0(col, "_lag_2"),
        "+",
        paste0(col, "_lag_3"),
        "+",
        paste0(col, "_lag_4"),
        "+",
        paste0(col, "_lag_5"),
        "+",
        paste0(col, "_lag_6")
      )
    )
  
  # fit model
  model <- gnm(
    formula,
    offset = n_benes,
    family = 'poisson',
    eliminate = as.factor(stratum),
    data = an_dat
  )
  
  # store model
  models[[col]] <- model
}


summary_list <- list()

for (col in exposure_columns) {
  # Extract cut_point and duration from the column name
  cut_point <- str_extract(col, "0\\.\\d+")
  duration <- str_extract(col, "^\\d+")
  
  # Extract coefficients from the model
  model <- models[[col]]
  coef_summary <- summary(model)$coefficients
  
  # Extract the coefficient for the main exposure and its lags
  main_coef <- coef_summary[col, "Estimate"]
  lag_coefs <- coef_summary[paste0(col, "_lag_", 1:6), "Estimate"]
  
  # Combine the information into a data.table
  summary_dt <- data.table(
    cut_point = cut_point,
    duration = duration,
    coefficient = main_coef,
    lag_1 = lag_coefs[1],
    lag_2 = lag_coefs[2],
    lag_3 = lag_coefs[3],
    lag_4 = lag_coefs[4],
    lag_5 = lag_coefs[5],
    lag_6 = lag_coefs[6]
  )
  
  # Append to the summary list
  summary_list[[col]] <- summary_dt
}


# Combine all summary data.tables into one
summary_table <- rbindlist(summary_list)
summary_table$duration <- c(8, 8, 8, 8, 4, 4, 4, 4, 12, 12, 12, 12) # could fix this if we felt like it

# Print the summary table
print(summary_table)
