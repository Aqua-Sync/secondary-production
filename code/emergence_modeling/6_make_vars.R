library(tidyverse)
library(foreign)

# This code extracts the predictor values for every water basin on earth. We use this data in the next scripts
# to make predictions of emergence given a basin's environmental predictors.
# Nothing here is stochastic. It's just wrangling. You only need to run it once (or can just upload the hydrobasin_vars_rssa_short.rds file)
# load full variable list and condense only to those used in models
# These are variables for all HYBAS_ID's

# hydrobasin_vars_rssa = read.dbf("data/BasinATLAS_v10_lev12_SpatialJoin_RSSA.dbf")

# vars_to_keep = c("HYBAS_ID",
#                  "BAS_ID",
#                  "SUB_AREA",
#                  "BA_km2",
#                  "mnRSSA_pc",
#                  "sdRSSA_pc",
#                 "tmp_dc_syr",
#                  "pre_mm_syr",
#                  "ele_mt_sav",
#                  "dis_m3_pyr",
#                  "for_pc_sse",
#                  "hft_ix_s93",
#                  "hft_ix_u93",
#                  "hft_ix_s09",
#                  "hft_ix_u09")
# 
# hydrobasin_vars_rssa_short = hydrobasin_vars_rssa %>% select(contains(vars_to_keep)) %>% 
#   mutate(region = str_sub(HYBAS_ID, 1, 1))
# 
# saveRDS(hydrobasin_vars_rssa_short, file = "data/hydrobasin_vars_rssa_short.rds")

hydrobasin_vars_rssa_short = readRDS(file = "data/hydrobasin_vars_rssa_short.rds")

hydrobasin_vars_rssa_short %>% select(BAS_ID, HYBAS_ID, BA_km2, SUB_AREA, mnRSSA_pc) %>% 
  group_by(BAS_ID) %>% 
  mutate(water_km2 = SUB_AREA*(mnRSSA_pc/100)) %>% 
  ungroup %>% 
  reframe(global_k2 = sum(water_km2))
