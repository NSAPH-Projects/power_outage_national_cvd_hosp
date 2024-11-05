# Run models on all cut points and durations of binary exposure for 
# emergency hospitalizations

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines)

# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_nov_2.RDS'))
an_dat <- an_dat %>% mutate(all_hosp = n_cvd + n_resp)

outcome_col <- 'whatever it will be'

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
        "+ ns(max_temp, df = 3) + ns(precip, df = 3) + ns(wind_speed, df = 3) +",
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
    family = quasipoisson(),
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
  
  # Extract coefficients and standard errors from the model
  model <- models[[col]]
  coef_summary <- summary(model)$coefficients
  
  # Extract the coefficient and standard error for the main exposure and its lags
  main_coef <- coef_summary[col, "Estimate"]
  main_se <- coef_summary[col, "Std. Error"]
  lag_coefs <- coef_summary[paste0(col, "_lag_", 1:6), "Estimate"]
  lag_ses <- coef_summary[paste0(col, "_lag_", 1:6), "Std. Error"]
  
  # Calculate confidence intervals
  main_ci_lower <- main_coef - 1.96 * main_se
  main_ci_upper <- main_coef + 1.96 * main_se
  lag_ci_lower <- lag_coefs - 1.96 * lag_ses
  lag_ci_upper <- lag_coefs + 1.96 * lag_ses
  
  # Combine the information into a data.table
  summary_dt <- data.table(
    cut_point = cut_point,
    duration = duration,
    coefficient = main_coef,
    ci_lower = main_ci_lower,
    ci_upper = main_ci_upper,
    lag_1 = lag_coefs[1],
    lag_1_ci_lower = lag_ci_lower[1],
    lag_1_ci_upper = lag_ci_upper[1],
    lag_2 = lag_coefs[2],
    lag_2_ci_lower = lag_ci_lower[2],
    lag_2_ci_upper = lag_ci_upper[2],
    lag_3 = lag_coefs[3],
    lag_3_ci_lower = lag_ci_lower[3],
    lag_3_ci_upper = lag_ci_upper[3],
    lag_4 = lag_coefs[4],
    lag_4_ci_lower = lag_ci_lower[4],
    lag_4_ci_upper = lag_ci_upper[4],
    lag_5 = lag_coefs[5],
    lag_5_ci_lower = lag_ci_lower[5],
    lag_5_ci_upper = lag_ci_upper[5],
    lag_6 = lag_coefs[6],
    lag_6_ci_lower = lag_ci_lower[6],
    lag_6_ci_upper = lag_ci_upper[6]
  )
  
  # Append to the summary list
  summary_list[[col]] <- summary_dt
}

# Combine all summary data.tables into one
summary_table <- rbindlist(summary_list)
summary_table$duration <- c(8, 8, 8, 8, 4, 4, 4, 4, 12, 12, 12, 12) # could fix this if we felt like it

# Print the summary table
print(summary_table)

summary_long <- melt(summary_table, 
                     id.vars = c("cut_point", "duration"), 
                     measure.vars = patterns("^lag_\\d+$", "^lag_\\d+_ci_lower$", "^lag_\\d+_ci_upper$"),
                     variable.name = "lag",
                     value.name = c("effect", "ci_lower", "ci_upper"))

# Extract the lag number from the variable name
summary_long[, lag := as.numeric(gsub("lag_", "", lag))]

# Create the ggplot
p <- ggplot(summary_long, aes(x = lag, y = effect, color = cut_point)) +
  geom_hline(aes(yintercept =0)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  facet_grid(duration ~ cut_point) +
  labs(x = "Lag", y = "poisson model coefficient", color = "power outage on
cut point") +
  theme_minimal() +
  ggtitle('Association of power outage exposure with emergency respiratory and 
cardiovascular (not hypertension) hospitalizations among medicare benes in counties 
> 500 benes, >50% pous coverage') + labs(subtitle = "y axis right is outage duration - 4, 8, 12 hrs")

# Print the plot
print(p)
