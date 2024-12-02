# Do analysis of continuous number of hours of power outage 

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines, dlnm)

source(here("code", "run_models", "run_models_helper_functions.R"))


# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_dec_1.RDS'))


# Do ----------------------------------------------------------------------

temp_crossbasis_ns <- crossbasis(
  an_dat$max_temp,
  lag = 6,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

power_outage_crossbasis <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "ns", df = 3, knots = c(2, 5)),
  arglag = list(fun = "ns", df = 5)
)

precip_dfs = 2
offset_col = 'n_benes'
outcome_col = 'n_cvd_no_hem_no_hyp'
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
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)


pred_cont_po <- crosspred(
  # the exposure crossbasis
  basis = power_outage_crossbasis,  
  # the model
  model= model,
  # compute the estimated association for 
  # each integer value of PM2.5
  # between 0 and 55
  at = 0:25, 
  # estimate association along 
  # lags in increments of 0.2 
  # using the natural splines 
  # to interpolate between days
  bylag = 1)

plot(pred_cont_po)

# units of change of independent variable
     col="red", 
     ylab=" ", 
     ci.arg=list(density=15,lwd=2),
     main=expression("Association for a 10-unit Increase in PM"[2.5]), 
     yaxp = c(0.990, 1.015, 10), las =1)
