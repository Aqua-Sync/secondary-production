library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)
theme_set(theme_default())

# function to estimate stream temperatures from air temperature
# from eq. 2 in Mohseni, O., Erickson, T. R., & Stefan, H. G. (1997). A Non-Linear Regression Model for Weekly Stream Temperatures at 585 Gaging Stations in the US.
estimate_streamtemp = function(x){
  numerator = 26.2                           # values are median parameter values from Mohseni et al. Tbl X.
  denominator = 1 + (exp(0.18*(13.3 - x)))
  0.8 + (numerator/denominator)
}

# Get ranges of temp and precip in the training data and compared to the
# range for predictions

# 1) load raw emergence data and predictors. precip_s is the scale() transformed precipitation in mm/km2
emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds') # created in fit_emergence_predictor_models

# 2) load predictors for each hybas
data_to_predict = hydrobasin_vars_rssa_short %>% 
  filter(SUB_AREA > 0) %>%
  mutate(precip_mm_perkm2 = pre_mm_syr/SUB_AREA,
         precip_s = (precip_mm_perkm2 - attributes(emergence_production_with_vars$precip_s)[[2]])/attributes(emergence_production_with_vars$precip_s)[[3]],
         tmp_dc_syr10 = tmp_dc_syr/10, # put temps in dec C instead of 10*dec C
         stream_temp = estimate_streamtemp(tmp_dc_syr10),
         stream_temp_s = scale(stream_temp),
         stream_temp = (stream_temp_s*attributes(emergence_production_with_vars$stream_temp_s)[[3]]) + attributes(emergence_production_with_vars$stream_temp_s)[[2]])

hist(estimate_streamtemp(data_to_predict_list$tmp_dc_syr/10))
hist(data_to_predict_list$stream_temp)

mean(emergence_production_with_vars$stream_temp, na.rm = T)
sd(emergence_production_with_vars$stream_temp, na.rm = T)
