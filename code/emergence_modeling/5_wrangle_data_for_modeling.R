library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)

# transform and/or standardize predictors.
# result is the raw data to be used in regression models predicting annual insect emergence production

source('code/custom_functions/estimate_streamtemp.R') # this converts air temperature to stream temperatures using a published equation

# load data ---------------------
# use secondary_prod_with_attributes to reattached the scale and center to standardized variables. The scale and center were lost in a bind_rows function earlier
secondary_prod_with_attributes = readRDS(file = "data/secondary_prod_with_attributes.rds") %>% 
  select(id, ends_with("_s"))

# get name of attribute columns to remove below before left_join
attribute_cols = secondary_prod_with_attributes %>% select(-id) %>% names()

emergence_production_with_vars = readRDS(file = "data/emergence_production.rds") %>% 
  mutate(emerge_1 = mean_emergence_mgdmm2y/mean(mean_emergence_mgdmm2y, na.rm = T), #scale to max
         sd_emergence_1 = sd_emergence/max(mean_emergence_mgdmm2y, na.rm = T), # scale sd to max (this is correct, see plots below)
         mean_emergence = mean(mean_emergence_mgdmm2y, na.rm = T),
         sd_emergence = sd(mean_emergence_mgdmm2y, na.rm = T),
         acsp_1 = acsp/max(acsp, na.rm = T),
         stream_temp = estimate_streamtemp(tmp_dc_syr10),
         stream_temp_s = scale(stream_temp),
         stream_temp20 = stream_temp/20) %>%  # reduce range of stream temps to improve model fitting
  mutate(HYBAS_ID = as.character(hybas_id)) %>% 
  select(-all_of(attribute_cols)) %>% # removes cols without attributes
  left_join(secondary_prod_with_attributes)

saveRDS(emergence_production_with_vars, file = 'data/emergence_production_with_vars.rds')


# check that nothing is missing
emergence_production_with_vars %>% 
  select(ends_with("_s")) %>% # i.e., most predictors
  pivot_longer(cols = everything()) %>% 
  filter(is.na(value)) %>% 
  distinct(name) # just log_aisp_mean and log_aisp_sd_s, which is fine.
         
emergence_production_with_vars %>% 
  filter(is.na(emerge_1)) 

emergence_production_with_vars %>% 
  ggplot(aes(x = stream_temp_s, y = mean_emergence_mgdmm2y)) + 
  geom_pointinterval(aes(ymin = mean_emergence_mgdmm2y - sd_emergence,
                         ymax = mean_emergence_mgdmm2y + sd_emergence))

emergence_production_with_vars %>% 
  ggplot(aes(x = stream_temp_s, y = emerge_1)) + 
  geom_pointinterval(aes(ymin = emerge_1 - sd_emergence_1,
                         ymax = emerge_1 + sd_emergence_1))
