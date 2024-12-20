# Do analysis of continuous number of hours of power outage 

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines, dlnm)

source(here("code", "run_models", "run_models_helper_functions.R"))


# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_dec_17.RDS'))

# Do ----------------------------------------------------------------------

# same for both 
temp_crossbasis_ns <- crossbasis(
  an_dat$max_temp,
  lag = 6,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

# 3 df for continuous PO exposure, lag dim 3
power_outage_crossbasis_3_df_lag_3_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "ns", df = 3, knots = c(2, 5)),
  arglag = list(fun = "ns", df = 3)
)

# 3 df for continuous PO exposure, lag dim 4
power_outage_crossbasis_3_df_lag_4_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "ns", df = 3, knots = c(2, 5)),
  arglag = list(fun = "ns", df = 4)
)

# 3 df for continuous PO exposure, lag dim 5
power_outage_crossbasis_3_df_lag_5_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "ns", df = 3, knots = c(2, 5)),
  arglag = list(fun = "ns", df = 5)
)

# 3 df for continuous PO exposure, lag dim 6
power_outage_crossbasis_3_df_lag_6_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "ns", df = 3, knots = c(2, 5)),
  arglag = list(fun = "ns", df = 6)
)

# linear for continuous PO exposure, lag df 6
power_outage_crossbasis_linear_lag_6_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "lin"),
  arglag = list(fun = "ns", df = 6)
)

# linear for continuous PO exposure, lag df 5
power_outage_crossbasis_linear_lag_5_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "lin"),
  arglag = list(fun = "ns", df = 5)
)

# linear for continuous PO exposure, lag df 4
power_outage_crossbasis_linear_lag_4_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "lin"),
  arglag = list(fun = "ns", df = 4)
)

# linear for continuous PO exposure, lag df 3
power_outage_crossbasis_linear_lag_3_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "lin"),
  arglag = list(fun = "ns", df = 3)
)


# CVD models quasipoisson -------------------------------------------------

model_linear_lag_df_3 <- gnm(
  n_cvd_no_hyp ~ 
  power_outage_crossbasis_linear_lag_3_df +
  temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

# this is the best fit 
model_linear_lag_df_4 <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_linear_lag_4_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_linear_lag_df_5 <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_linear_lag_5_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_linear_lag_df_6 <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_linear_lag_6_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_3 <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_3_df_lag_3_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_4 <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_3_df_lag_4_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_5 <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_3_df_lag_5_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_6 <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_3_df_lag_6_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

quasipoisson_cvd <-
  list(
    model_linear_lag_df_3,
    model_linear_lag_df_4,
    model_linear_lag_df_5,
    model_linear_lag_df_6,
    model_3_df_lag_df_3, 
    model_3_df_lag_df_4, 
    model_3_df_lag_df_5, 
    model_3_df_lag_df_6
  )

# CVD models regular poisson ----------------------------------------------
model_linear_lag_df_3_p <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_linear_lag_3_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_linear_lag_df_4_p <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_linear_lag_4_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_linear_lag_df_5_p <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_linear_lag_5_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_linear_lag_df_6_p <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_linear_lag_6_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_3_p <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_3_df_lag_3_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_4_p <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_3_df_lag_4_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_5_p <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_3_df_lag_5_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_6_p <- gnm(
  n_cvd_no_hyp ~ 
    power_outage_crossbasis_3_df_lag_6_df +
    temp_crossbasis_ns +
    ns(precip, df = 2) +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)


poisson_cvd <-
  list(
    model_linear_lag_df_3_p,
    model_linear_lag_df_4_p,
    model_linear_lag_df_5_p,
    model_linear_lag_df_6_p,
    model_3_df_lag_df_3_p, 
    model_3_df_lag_df_4_p, 
    model_3_df_lag_df_5_p, 
    model_3_df_lag_df_6_p
  )

# compare models 
# calculate qaics 
get_qaic <- function(quasi_model, model){
  ll <- as.numeric(logLik(model))
  quasi_summary <- summary(quasi_model)
  dispersion <- quasi_summary$dispersion
  dfs <- df.residual(quasi_model)
  qaic <- -2 * ll/dispersion + 2 * dfs
  return(qaic)
}

qaics <-
  mapply(FUN = get_qaic,
         quasi_model = quasipoisson_cvd,
         model = poisson_cvd)

max(qaics)
print(qaics)
# best fit is model #2 in this list, which is linear relationship between 
# hours out and hospitalizations, with 4 dfs on the lag
# dimension


# need to do this for resp now --------------------------------------------
# almost there you're doing great 
model_linear_lag_df_3 <- gnm(
  n_resp ~ 
    power_outage_crossbasis_linear_lag_3_df +
    temp_crossbasis_ns +
    precip +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

# this is the best fit 
model_linear_lag_df_4 <- gnm(
  n_resp ~ 
    power_outage_crossbasis_linear_lag_4_df +
    temp_crossbasis_ns +
    precip + 
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_linear_lag_df_5 <- gnm(
  n_resp ~ 
    power_outage_crossbasis_linear_lag_5_df +
    temp_crossbasis_ns +
    precip + 
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_linear_lag_df_6 <- gnm(
  n_resp ~ 
    power_outage_crossbasis_linear_lag_6_df +
    temp_crossbasis_ns +
    precip + 
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_3 <- gnm(
  n_resp ~ 
    power_outage_crossbasis_3_df_lag_3_df +
    temp_crossbasis_ns +
    precip +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_4 <- gnm(
  n_resp ~ 
    power_outage_crossbasis_3_df_lag_4_df +
    temp_crossbasis_ns +
    precip + 
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_5 <- gnm(
  n_resp ~ 
    power_outage_crossbasis_3_df_lag_5_df +
    temp_crossbasis_ns +
    precip + 
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_6 <- gnm(
  n_resp ~ 
    power_outage_crossbasis_3_df_lag_6_df +
    temp_crossbasis_ns +
    precip + 
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

quasipoisson_resp <-
  list(
    model_linear_lag_df_3,
    model_linear_lag_df_4,
    model_linear_lag_df_5,
    model_linear_lag_df_6,
    model_3_df_lag_df_3, 
    model_3_df_lag_df_4, 
    model_3_df_lag_df_5, 
    model_3_df_lag_df_6
  )

# resp models regular poisson ----------------------------------------------
model_linear_lag_df_3_p <- gnm(
  n_resp ~ 
    power_outage_crossbasis_linear_lag_3_df +
    temp_crossbasis_ns +
    precip + 
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_linear_lag_df_4_p <- gnm(
  n_resp ~ 
    power_outage_crossbasis_linear_lag_4_df +
    temp_crossbasis_ns +
    precip + 
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_linear_lag_df_5_p <- gnm(
  n_resp ~ 
    power_outage_crossbasis_linear_lag_5_df +
    temp_crossbasis_ns +
    precip + 
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_linear_lag_df_6_p <- gnm(
  n_resp ~ 
    power_outage_crossbasis_linear_lag_6_df +
    temp_crossbasis_ns +
    precip + 
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_3_p <- gnm(
  n_resp ~ 
    power_outage_crossbasis_3_df_lag_3_df +
    temp_crossbasis_ns +
    precip + 
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_4_p <- gnm(
  n_resp ~ 
    power_outage_crossbasis_3_df_lag_4_df +
    temp_crossbasis_ns +
    precip + 
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_5_p <- gnm(
  n_resp ~ 
    power_outage_crossbasis_3_df_lag_5_df +
    temp_crossbasis_ns +
    precip + 
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_lag_df_6_p <- gnm(
  n_resp ~ 
    power_outage_crossbasis_3_df_lag_6_df +
    temp_crossbasis_ns +
    precip + 
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = poisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)


poisson_resp <-
  list(
    model_linear_lag_df_3_p,
    model_linear_lag_df_4_p,
    model_linear_lag_df_5_p,
    model_linear_lag_df_6_p,
    model_3_df_lag_df_3_p, 
    model_3_df_lag_df_4_p, 
    model_3_df_lag_df_5_p, 
    model_3_df_lag_df_6_p
  )


# compare models 
# calculate qaics 
get_qaic <- function(quasi_model, model){
  ll <- as.numeric(logLik(model))
  quasi_summary <- summary(quasi_model)
  dispersion <- quasi_summary$dispersion
  dfs <- df.residual(quasi_model)
  qaic <- -2 * ll/dispersion + 2 * dfs
  return(qaic)
}

qaics <-
  mapply(FUN = get_qaic,
         quasi_model = quasipoisson_resp,
         model = poisson_resp)

max(qaics)
print(qaics)

# best fit is first model - linear btw hrs out and respiratory hosp,
# and 3 dfs on the lag dimension 


















# compare models 
anova(model_3_df, model_linear, test ='F')

# not a better fit according to the anova

# get preds 
pred_po_linear <- crosspred(
  basis = power_outage_crossbasis_linear,  
  # the model
  model= model_linear,
  at = 0:24, 
  bylag = 1)

plot(pred_po_linear)



pred_po_3_df <- crosspred(
  basis = power_outage_crossbasis_3_df,  
  model= model_3_df,
  at =0:24, 
  bylag = 1)

plot(pred_po_3_df)

# Plot results and save ---------------------------------------------------

# 3 dfs
x = 0:24
y = 0:6
m = as.matrix(pred_po_3_df$matfit)

png(here('figures', 'figures_output', 'dlnm_n_hrs_out_3_df_cvd.png'),  
    width = 1000, height = 400)

par(mar = c(5,7,4,2) + 0.5)

persp(y, x, z = t(m), theta = 30, phi = 30, col = "lightblue", 
      xlab = " ", ylab = " ", zlab = " ", 
      ticktype = "detailed")
dev.off()

# linear
x = 0:24
y = 0:6
m = as.matrix(pred_po_linear$matfit)

png(here('figures', 'figures_output', 'dlnm_n_hrs_out_linear_cvd.png'),  
    width = 1000, height = 400)

par(mar = c(5,7,4,2) + 0.5)

persp(y, x, z = t(m), theta = 30, phi = 30, col = "lightblue", 
      xlab = " ", ylab = " ", zlab = " ", 
      ticktype = "detailed")
dev.off()


# Resp outcomes -----------------------------------------------------------

# same for both 
temp_crossbasis_ns <- crossbasis(
  an_dat$max_temp,
  lag = 6,
  argvar = list(fun = "ns", df = 3),
  arglag = list(fun = "ns", df = 3)
)

# 3 df for continuous PO exposure 
power_outage_crossbasis_3_df <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "ns", df = 3, knots = c(2, 5)),
  arglag = list(fun = "ns", df = 5)
)

# linear for continuous PO exposure
power_outage_crossbasis_linear <- crossbasis(
  an_dat$n_hrs_out_0.01,
  lag = 6,
  argvar = list(fun = "lin"),
  arglag = list(fun = "ns", df = 5)
)

# models

model_linear_resp <- gnm(
  n_resp ~ 
    power_outage_crossbasis_linear +
    temp_crossbasis_ns +
    precip +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

model_3_df_resp <- gnm(
  n_resp ~ 
    power_outage_crossbasis_3_df +
    temp_crossbasis_ns +
    precip +
    ns(wind_speed, df = 3) + 
    offset(log(n_benes)),
  family = quasipoisson(),
  eliminate = as.factor(an_dat$stratum),
  data = an_dat
)

# compare models 
anova(model_3_df_resp, model_linear_resp, test ='F')

# not a better fit according to the anova

# get preds 
pred_po_linear_resp <- crosspred(
  basis = power_outage_crossbasis_linear,  
  # the model
  model= model_linear_resp,
  at = 0:24, 
  bylag = 1)

plot(pred_po_linear_resp)



pred_po_3_df_resp <- crosspred(
  basis = power_outage_crossbasis_3_df,  
  model= model_3_df_resp,
  at =0:24, 
  bylag = 1)

plot(pred_po_3_df_resp)

# Plot results and save ---------------------------------------------------

# 3 dfs
x = 0:24
y = 0:6
m = as.matrix(pred_po_3_df_resp$matfit)

png(here('figures', 'figures_output', 'dlnm_n_hrs_out_3_df_resp.png'),  
    width = 1000, height = 400)

par(mar = c(5,7,4,2) + 0.5)

persp(y, x, z = t(m), theta = 30, phi = 30, col = "lightblue", 
      xlab = " ", ylab = " ", zlab = " ", 
      ticktype = "detailed")
dev.off()

# linear
x = 0:24
y = 0:6
m = as.matrix(pred_po_linear_resp$matfit)

png(here('figures', 'figures_output', 'dlnm_n_hrs_out_linear_resp.png'),  
    width = 1000, height = 400)

par(mar = c(5,7,4,2) + 0.5)

persp(y, x, z = t(m), theta = 30, phi = 30, col = "lightblue", 
      xlab = " ", ylab = " ", zlab = " ", 
      ticktype = "detailed")
dev.off()

# ok so interesting - 3 dfs is actually a better fit for respiratory hosp 
