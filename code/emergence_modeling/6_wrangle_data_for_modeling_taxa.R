library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)

# transform and/or standardize predictors.
# result is the raw data to be used in regression models predicting annual insect emergence production

source('code/custom_functions/estimate_streamtemp.R') # this converts air temperature to stream temperatures using a published equation

# load data ---------------------
emergence_production_taxa = readRDS(file = "data/emergence_production_taxa.rds") %>% 
  mutate(tmp_dc_syr10 = tmp_dc_syr/10, # put temps in dec C instead of 10*dec C
         pre_cm_syr1000 = pre_mm_syr/1000,
         # precip_s = scale(pre_mm_syr),
         # precip_mm_perkm2 = pre_mm_syr/sub_area, # dont do this. Keeping it here as a reminder. pre_mm_syr is implicitly area corrected as mm/m2
         precip_s = scale(pre_mm_syr),
         log10_precip_s = scale(log10(pre_mm_syr)), # don't need to use this predictor. See "Notes on contaminant modeling.RMD"
         ele_mt_sav_s = scale(ele_mt_sav),
         logdis_m3_pyr_s = scale(log(dis_m3_pyr + 0.05)),
         for_pc_sse_s = scale(for_pc_sse),
         crp_pc_sse_s = scale(crp_pc_sse),
         hft_ix_s93_s = scale(hft_ix_s93),  # "source: https://developers.google.com/earth-engine/datasets/catalog/WWF_HydroATLAS_v1_Basins_level12#table-schema"
         hft_ix_u93_s = scale(hft_ix_u93),
         hft_ix_s09_s = scale(hft_ix_s09),  # "source: https://developers.google.com/earth-engine/datasets/catalog/WWF_HydroATLAS_v1_Basins_level12#table-schema"
         hft_ix_u09_s = scale(hft_ix_u09))  # "hft_ix_xxx are the human footprint index. s93 is the 'spatial extent at subbasin pour point in 1993 or s09 = 2009. u93 is total watershed upstream of point. Try both."

emergence_production_with_vars_taxa = emergence_production_taxa %>% 
  mutate(emerge_1 = mean_emergence_mgdmm2y/mean(mean_emergence_mgdmm2y, na.rm = T),
         mean_emergence = mean(mean_emergence_mgdmm2y, na.rm = T),
         stream_temp = estimate_streamtemp(tmp_dc_syr10),
         stream_temp_s = scale(stream_temp),
         stream_temp20 = stream_temp/20) %>%  # reduce range of stream temps to improve model fitting
  mutate(HYBAS_ID = as.character(hybas_id))

saveRDS(emergence_production_with_vars_taxa, file = 'data/emergence_production_with_vars_taxa.rds')



