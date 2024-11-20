# Test confounder-outcome relationships

# Libraries ---------------------------------------------------------------

pacman::p_load(tidyverse, data.table, here, arrow, gnm, splines)

source(here("code", "run_models", "run_models_helper_functions.R"))


# Read --------------------------------------------------------------------

an_dat <- read_rds(here('data', 'an_dat_urgent_hosp_nov_5.RDS'))

# select relevant cols

test_confound <-
  an_dat %>% select(
    five_digit_fips,
    day,
    exposed_8_hrs_0.01,
    max_temp,
    precip,
    wind_speed,
    n_cvd_no_hem_no_hyp,
    n_resp,
    n_benes,
    day_of_week,
    stratum
  )

# look at confounders
summary(test_confound$wind_speed)
hist(test_confound$wind_speed)

summary(test_confound$precip)
hist(test_confound$precip)


# Run models --------------------------------------------------------------

# want to run model without power outage exposure in it, and test the dfs 
# to use on wind speed and precipitation 

knots_precip = quantile(test_confound$precip, probs=c(.80, .90))


precip_linear_resp = gnm(
  data = test_confound,
  n_resp ~ max_temp + wind_speed + precip + offset(n_benes),
  family = quasipoisson(),
  eliminate = as.factor(stratum)
)

precip_2_df_resp = gnm(
  data = test_confound,
  n_resp ~ max_temp + wind_speed + ns(precip, df = 2) + offset(n_benes),
  family = quasipoisson(),
  eliminate = as.factor(stratum)
)

precip_3_df_resp = gnm(
  data = test_confound,
  n_resp ~ max_temp + wind_speed + ns(precip, knots = knots_precip) + offset(n_benes),
  family = quasipoisson(),
  eliminate = as.factor(stratum)
)

summary(precip_3_df_resp)
anova(precip_2_df_resp, precip_3_df_resp, test = 'F')
anova(precip_linear_resp, precip_2_df_resp, test = 'F')

# looks like 2 dfs is correct.

precip_linear_cvd = gnm(
  data = test_confound,
  n_cvd_no_hem_no_hyp ~ max_temp + wind_speed + precip + offset(n_benes),
  family = quasipoisson(),
  eliminate = as.factor(stratum)
)

precip_2_df_cvd = gnm(
  data = test_confound,
  n_cvd_no_hem_no_hyp ~ max_temp + wind_speed + ns(precip, df = 2) + offset(n_benes),
  family = quasipoisson(),
  eliminate = as.factor(stratum)
)

precip_3_df_cvd = gnm(
  data = test_confound,
  n_cvd_no_hem_no_hyp ~ max_temp + wind_speed + ns(precip, knots = knots_precip) + offset(n_benes),
  family = quasipoisson(),
  eliminate = as.factor(stratum)
)

summary(precip_3_df_cvd)
anova(precip_linear_cvd, precip_2_df_cvd, test = 'F')
anova(precip_2_df_cvd, precip_3_df_cvd, test = 'F')

# again 2 seems to be appropriate 


# Wind --------------------------------------------------------------------

wind_linear_resp = gnm(
  data = test_confound,
  n_resp ~ max_temp + wind_speed + ns(precip, df = 2) + offset(n_benes),
  family = quasipoisson(),
  eliminate = as.factor(stratum)
)

wind_2_df_resp = gnm(
  data = test_confound,
  n_resp ~ max_temp + ns(wind_speed, df = 2) + ns(precip, df = 2) + offset(n_benes),
  family = quasipoisson(),
  eliminate = as.factor(stratum)
)

wind_3_df_resp = gnm(
  data = test_confound,
  n_resp ~ max_temp +  ns(wind_speed, df = 3) + ns(precip, df = 2) + offset(n_benes),
  family = quasipoisson(),
  eliminate = as.factor(stratum)
)

wind_4_df_resp = gnm(
  data = test_confound,
  n_resp ~ max_temp +  ns(wind_speed, df = 4) + ns(precip, df = 2) + offset(n_benes),
  family = quasipoisson(),
  eliminate = as.factor(stratum)
)

summary(wind_3_df_resp)
anova(wind_2_df_resp, wind_3_df_resp, test = 'F')
anova(wind_linear_resp, wind_2_df_resp, test = 'F')
anova(wind_3_df_resp, wind_4_df_resp, test = 'F')

# 3 dfs seems best 

wind_linear_cvd = gnm(
  data = test_confound,
  n_cvd_no_hem_no_hyp ~ max_temp + wind_speed + ns(precip, df = 2) + offset(n_benes),
  family = quasipoisson(),
  eliminate = as.factor(stratum)
)

wind_2_df_cvd = gnm(
  data = test_confound,
  n_cvd_no_hem_no_hyp ~ max_temp + ns(wind_speed, df = 2) + ns(precip, df = 2) + offset(n_benes),
  family = quasipoisson(),
  eliminate = as.factor(stratum)
)

wind_3_df_cvd = gnm(
  data = test_confound,
  n_cvd_no_hem_no_hyp ~ max_temp +  ns(wind_speed, df = 3) + ns(precip, df = 2) + offset(n_benes),
  family = quasipoisson(),
  eliminate = as.factor(stratum)
)

wind_4_df_cvd = gnm(
  data = test_confound,
  n_cvd_no_hem_no_hyp ~ max_temp +  ns(wind_speed, df = 4) + ns(precip, df = 2) + offset(n_benes),
  family = quasipoisson(),
  eliminate = as.factor(stratum)
)

summary(wind_3_df_cvd)
anova(wind_2_df_cvd, wind_3_df_cvd, test = 'F')
anova(wind_linear_cvd, wind_2_df_cvd, test = 'F')
anova(wind_3_df_cvd, wind_4_df_cvd, test = 'F')

# looks like 4 dfs is actually good
# maybe that's not biologically plausible though? 
