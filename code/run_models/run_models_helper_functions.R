# Helper function for running main models

# function runs a power outage model on 2018 panel data when given an exposure 
# column and outcome column 
run_binary_exposure_model <-
  function(outcome_col, exposure_col, offset_col, po_data) {
    # define formula
    formula <-
      as.formula(
        paste(
          outcome_col,
          " ~ ",
          exposure_col,
          "+ ns(max_temp, df = 3)",
          "+ ns(precip, df = 3)" ,
          "+ ns(wind_speed, df = 3) +",
          paste0(exposure_col, "_lag_1"),
          "+",
          paste0(exposure_col, "_lag_2"),
          "+",
          paste0(exposure_col, "_lag_3"),
          "+",
          paste0(exposure_col, "_lag_4"),
          "+",
          paste0(exposure_col, "_lag_5"),
          "+",
          paste0(exposure_col, "_lag_6"),
          "+",
          offset(offset_col)
        )
      )
    
    # fit model
    model <- gnm(
      formula,
      family = quasipoisson(),
      eliminate = as.factor(stratum),
      data = po_data
    )
    
    return(model)
  }


# function to read results ------------------------------------------------

# function reads results from list of models on different exposure cut points
# and durations, and produces a summary table
read_model_results <- function(models, exposure_columns) {
  summary_list <- list()
  
  for (col in exposure_columns) {
    # Extract cut_point and duration from the column name
    # cut_point <- str_extract(col, "0\\.\\d+")
    # duration <- str_extract(col, "^\\d+")
    cut_point <- str_extract(col, "0\\.\\d+")
    duration <- str_extract(col, "\\d{1,2}")

    
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
      lag_0 = main_coef,
      lag_0_ci_lower = main_ci_lower,
      lag_0_ci_upper = main_ci_upper,
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
    
    summary_dt <-
      summary_dt %>% mutate(duration = ifelse(duration == 1, 12, duration))
    # Append to the summary list
    summary_list[[col]] <- summary_dt
  }
  
  # Combine all summary data.tables into one
  summary_table <- rbindlist(summary_list)
  
  return(summary_table)
}

create_summary_plot <- function(summary_table, title, subtitle) {
  # Reshape the summary table to long format
  summary_long <-
    melt(
      summary_table,
      id.vars = c("cut_point", "duration"),
      measure.vars = patterns("^lag_\\d+$",
                              "^lag_\\d+_ci_lower$",
                              "^lag_\\d+_ci_upper$"),
      variable.name = "lag",
      value.name = c("effect", "ci_lower", "ci_upper")
    )
  
  # Extract the lag number from the variable name
  summary_long[, lag := as.numeric(gsub("lag_", "", lag)) - 1]
  
  # set duration level
  summary_long[, duration := factor(duration, levels = c("4", "8", "12"))]
  
  
  # Create the ggplot
  p <- ggplot(summary_long, aes(x = lag, y = effect, color = cut_point)) +
    geom_hline(aes(yintercept = 0)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    facet_grid(duration ~ cut_point) +
    labs(x = "Lag", y = "Poisson model coefficient", color = "Power outage cut point") +
    theme_minimal() +
    ggtitle(title) +
    labs(subtitle = subtitle) 
  
  return(p)
}


# wrapper for running models ----------------------------------------------

run_models <- function(outcome_col, offset_col, data, exposures) {
  setNames(
    lapply(
      X = exposures,
      FUN = run_binary_exposure_model,
      outcome_col = outcome_col,
      po_data = data,
      offset = offset_col
    ),
    exposures
  )
}

# save model results ------------------------------------------------------

save_model_results <- function(model_result, name) {
  file_path <-
    here("results",
         "model_coefs_errors",
         paste0(name, "_coef_errors.RDS"))
  write_rds(model_result, file_path)
}


# create and save plot ----------------------------------------------------

create_and_save_plot <-
  function(summary_table, title, subtitle, filename) {
    p <- create_summary_plot(summary_table, title, subtitle)
    ggsave(
      p,
      width = 14,
      height = 7,
      filename = here("results", 'plots_of_results', filename)
    )
  }


# read main model results 
# function to read results ------------------------------------------------

# function reads results from list of models on different exposure cut points
# and durations, and produces a summary table
read_main_model_results <- function(models) {
  summary_list <- list()
  col = 'exposed_8_hrs_0.01'
  
  # Extract cut_point and duration from the column name
  # cut_point <- str_extract(col, "0\\.\\d+")
  # duration <- str_extract(col, "^\\d+")
  cut_point <- 0.01
  duration <- 8
  
  # Extract coefficients and standard errors from the model
  for (i in 1:length(models)) {
    coef_summary <- summary(models[[i]])$coefficients
    
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
      lag_0 = main_coef,
      lag_0_ci_lower = main_ci_lower,
      lag_0_ci_upper = main_ci_upper,
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
    m_name <- names(models)[[i]]
    summary_list[[m_name]] <- summary_dt
  }
  
  
  return(summary_list)
}


# create summary plots for main model -------------------------------------

create_summary_plot_main_model <- function(summary_table) {
  # Reshape the summary table to long format
  model_names <- summary_table$model_name
  summary_long <-
    melt(
      summary_table,
      id.vars = c("model_name"),
      measure.vars = patterns("^lag_\\d+$",
                              "^lag_\\d+_ci_lower$",
                              "^lag_\\d+_ci_upper$"),
      variable.name = "lag",
      value.name = c("effect", "ci_lower", "ci_upper")
    )
  
  # Extract the lag number from the variable name
  summary_long[, lag := as.numeric(gsub("lag_", "", lag)) - 1]
  
  # set duration level
  summary_long[, model_name := factor(model_name)]
  
  
  # Create the ggplot
  p <- ggplot(summary_long, aes(x = lag, y = effect)) +
    geom_hline(aes(yintercept = 0)) +
    geom_point(size = 7) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    facet_wrap(~model_name) +
    labs(x = "Lag", y = "Poisson model coefficient", color = "Power outage cut point") +
    theme_minimal() +
    theme(text = element_text(size = 20))
  
  return(p)
}


# Run dlnm ----------------------------------------------------------------

# copilot function 


run_dlnm_po_model_copilot <-
  function(po_data,
           outcome_col,
           exposure_col,
           offset_col,
           precip_dfs,
           po_dfs) {
    # create crossbases
    temp_crossbasis_ns <- crossbasis(
      po_data$max_temp,
      lag = 6,
      argvar = list(fun = "ns", df = 3),
      arglag = list(fun = "ns", df = 3)
    )
    
    power_outage_crossbasis <- crossbasis(
      po_data[[exposure_col]],
      lag = 6,
      argvar = list(fun = "lin"),
      arglag = list(fun = "ns", df = po_dfs)
    )
    
    # Define formula
    formula <- as.formula(
      paste(
        outcome_col,
        "~",
        "power_outage_crossbasis",
        "+",
        "temp_crossbasis_ns",
        "+",
        "ns(precip,",
        "df = ",
        precip_dfs,
        ")",
        "+",
        "ns(wind_speed,",
        "df = 3)",
        "+",
        "offset(log(",
        offset_col,
        "))"
      )
    )
    
    # fit model
    model <- gnm(
      formula,
      family = quasipoisson(),
      eliminate = as.factor(po_data$stratum),
      data = po_data
    )
    return(list(po_model = model, po_cb = power_outage_crossbasis))
  }

run_dlnm_po_model_regular_poisson <-
  function(po_data,
           outcome_col,
           exposure_col,
           offset_col,
           precip_dfs,
           po_dfs) {
    # create crossbases
    temp_crossbasis_ns <- crossbasis(
      po_data$max_temp,
      lag = 6,
      argvar = list(fun = "ns", df = 3),
      arglag = list(fun = "ns", df = 3)
    )
    
    power_outage_crossbasis <- crossbasis(
      po_data[[exposure_col]],
      lag = 6,
      argvar = list(fun = "lin"),
      arglag = list(fun = "ns", df = po_dfs)
    )
    
    # Define formula
    formula <- as.formula(
      paste(
        outcome_col,
        "~",
        "power_outage_crossbasis",
        "+",
        "temp_crossbasis_ns",
        "+",
        "ns(precip,",
        "df = ",
        precip_dfs,
        ")",
        "+",
        "ns(wind_speed,",
        "df = 3)",
        "+",
        "offset(log(",
        offset_col,
        "))"
      )
    )
    
    # fit model
    model <- gnm(
      formula,
      family = poisson(),
      eliminate = as.factor(po_data$stratum),
      data = po_data
    )
    return(list(po_model = model, po_cb = power_outage_crossbasis))
  }


# run linear --------------------------------------------------------------

run_dlnm_po_model_linear_precip <-
  function(po_data,
           outcome_col,
           exposure_col,
           offset_col,
           po_dfs) {
    # create crossbases
    temp_crossbasis_ns <- crossbasis(
      po_data$max_temp,
      lag = 6,
      argvar = list(fun = "ns", df = 3),
      arglag = list(fun = "ns", df = 3)
    )
    
    power_outage_crossbasis <- crossbasis(
      po_data[[exposure_col]],
      lag = 6,
      argvar = list(fun = "lin"),
      arglag = list(fun = "ns", df = po_dfs)
    )
    
    # Define formula
    formula <- as.formula(
      paste(
        outcome_col,
        "~",
        "power_outage_crossbasis",
        "+",
        "temp_crossbasis_ns",
        "+",
        "precip",
        "+",
        "ns(wind_speed,",
        "df = 3)",
        "+",
        "offset(log(",
        offset_col,
        "))"
      )
    )
    
    # fit model
    model <- gnm(
      formula,
      family = quasipoisson(),
      eliminate = as.factor(po_data$stratum),
      data = po_data
    )
    return(list(po_model = model, po_cb = power_outage_crossbasis))
  }

# run linear --------------------------------------------------------------

run_dlnm_po_model_linear_precip_regular_poisson <-
  function(po_data,
           outcome_col,
           exposure_col,
           offset_col,
           po_dfs) {
    # create crossbases
    temp_crossbasis_ns <- crossbasis(
      po_data$max_temp,
      lag = 6,
      argvar = list(fun = "ns", df = 3),
      arglag = list(fun = "ns", df = 3)
    )
    
    power_outage_crossbasis <- crossbasis(
      po_data[[exposure_col]],
      lag = 6,
      argvar = list(fun = "lin"),
      arglag = list(fun = "ns", df = po_dfs)
    )
    
    # Define formula
    formula <- as.formula(
      paste(
        outcome_col,
        "~",
        "power_outage_crossbasis",
        "+",
        "temp_crossbasis_ns",
        "+",
        "precip",
        "+",
        "ns(wind_speed,",
        "df = 3)",
        "+",
        "offset(log(",
        offset_col,
        "))"
      )
    )
    
    # fit model
    model <- gnm(
      formula,
      family = poisson(),
      eliminate = as.factor(po_data$stratum),
      data = po_data
    )
    return(list(po_model = model, po_cb = power_outage_crossbasis))
  }
# old function ------------------------------------------------------------

run_dlnm_po_model <-
  function(po_data,
           outcome_col,
           exposure_col,
           offset_col,
           precip_dfs,
           po_dfs) {
    
    # create crossbases
    temp_crossbasis_ns <- crossbasis(
      po_data$max_temp,
      lag = 6,
      argvar = list(fun = "ns", df = 3),
      arglag = list(fun = "ns", df = 3)
    )
    
    power_outage_crossbasis <- crossbasis(
      po_data[[exposure_col]],
      lag = 6,
      argvar = list(fun = "lin"),
      arglag = list(fun = "ns", df = dfs)
    )
    
    # define formula
    formula <-
      as.formula(
        paste(
          outcome_col,
          " ~ ",
          "temp_crossbasis_ns",
          "+",
          "power_outage_crossbasis",
          "+ ns(precip, df = 2)" ,
          "+ ns(wind_speed, df = 3) +",
          offset((offset_col))
        )
      )
    
    # fit model
      po_model <- gnm(
        formula,
        family = quasipoisson(),
        eliminate = as.factor(stratum),
        data = po_data
      )
    
    model_cb <- list(po_model = po_model, po_cb = power_outage_crossbasis)
    
    return(model_cb)
  }


# run dlnms

run_dlnm_models <-
  function(po_data,
           outcome_col,
           exposure_cols,
           offset_col,
           precip_dfs,
           po_dfs) {
    setNames(
      lapply(
        X = exposure_cols,
        FUN = run_dlnm_po_model_copilot,
        outcome_col = outcome_col,
        po_data = po_data,
        offset = offset_col,
        precip_dfs = precip_dfs, 
        po_dfs = po_dfs
      ),
      exposure_columns
    )
  }

# run dlnms linear precip
run_dlnm_linear_precip <-
  function(po_data,
           outcome_col,
           exposure_cols,
           offset_col,
           po_dfs) {
    setNames(
      lapply(
        X = exposure_cols,
        FUN = run_dlnm_po_model_linear_precip,
        outcome_col = outcome_col,
        po_data = po_data,
        offset = offset_col,
        po_dfs = po_dfs
      ),
      exposure_columns
    )
  }


# get predictions for dlnm 
get_dlnm_pred <- function(model_list) {
  # Initialize an empty list to store predictions
  predictions_list <- list()
  model_names <- names(model_list)
  # Loop through each model object in the list
  for (i in seq_along(model_list)) {
    # Extract the model and crossbasis from the current list element
    model <- model_list[[i]]$po_model
    power_outage_crossbasis <- model_list[[i]]$po_cb
    name <- model_names[[i]]
    
    # Generate predictions using crosspred
    pred <- crosspred(
      basis = power_outage_crossbasis, 
      model = model, 
      at = 0:2, 
      bylag = 1,
      cumul = TRUE
    )
    
    # get RR estimates
    # extract RR estimates and confidence intervals
    est <- pred$matRRfit[2, ]
    ci_high <- pred$matRRhigh[2, ]
    ci_low <- pred$matRRlow[2, ]
    
    # create a data frame with the estimates and confidence intervals
    preds <- data.frame(
      m_name = name,
      est = est,
      ci_low = ci_low,
      ci_high = ci_high,
      lags = seq(0, length(est) - 1)
    )
    
    # Store the predictions in the list
    predictions_list[[i]] <- preds
  }
  
  return(predictions_list)
}
