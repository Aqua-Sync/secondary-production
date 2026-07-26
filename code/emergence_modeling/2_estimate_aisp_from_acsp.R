library(brms)
library(tidyverse)
library(janitor)
library(readxl)
library(tidybayes)

source('code/custom_functions/estimate_streamtemp.R') # this converts air temperature to stream temperatures using a published equation


# convert all production data to insect only secondary production (i.e., aisp)
# Some data are already presented as insects only. those stay the same
# Other data are presented as total community production. We use models of the insect/community biomass ratio and then estimate the proportion
# of insects from the raw data to convert to aisp.
# ~20 seconds to 5 minutes (depending on need to compile models)

# 1) load data
# acsp = aquatic community secondary production (insects and non-insects)
# aisp = aquatic insect secondary production (insects only)
secondary_prod = read_csv(file = "data/secondary_prod.csv") %>% 
  select(acsp, aisp, everything())  %>% 
  mutate(across(where(is.character), ~ stringi::stri_trans_general(.x, "Latin-ASCII"))) %>%  # removes umlauts/accents, etc
  mutate(author_year = author) %>% 
  ungroup %>% 
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
         hft_ix_u09_s = scale(hft_ix_u09),
         stream_temp = estimate_streamtemp(tmp_dc_syr10),
         stream_temp_s = scale(stream_temp),
         HYBAS_ID = as.character(hybas_id),
         obs_id = as.character(row_number()))

saveRDS(secondary_prod, file = "data/secondary_prod_with_attributes.rds")

# Estimate insect secondary production from total community production --------------

# number with only acsp
secondary_prod %>% 
  filter(is.na(aisp) | is.na(emerg))

# 2) calculate raw proportion

dat_aispacsp = secondary_prod %>% 
  filter(!is.na(aisp)) %>% 
  filter(!is.na(acsp)) %>% 
  mutate(prop = aisp/acsp,
         prop = case_when(prop >= 1 ~ 0.99,
                           TRUE ~ prop)) 

# 3) get prior ratio from Gratton et al. 
gratton_fi = read_excel("data/gratton_supplement_A.xlsx") %>% 
  mutate(proportion_insects = parse_number(proportion_insects))


# # 4) fit model ----------------------------------------------------------

# brm_prop_prior = brm(prop ~ s(stream_temp_s, precip_s) + (1 | HYBAS_ID) + (1|obs_id),
#                      family = Beta(link = "logit"),
#                      data = dat_aispacsp,
#                      prior = c(prior(normal(0.66, 0.07), class = "Intercept"),
#                                prior(normal(0, 1), class = "b"),
#                                prior(lognormal(1.5, 0.1), class = "phi"),
#                                prior(exponential(2), class = "sd")),
#                      chains = 1, iter = 1000, sample_prior = "only")
# 
# saveRDS(brm_prop_prior, file = "models/brm_prop_prior.rds")

# brm_prop_insects_beta = brm(prop ~ s(stream_temp_s, precip_s) + (1 | HYBAS_ID) + (1|obs_id),
#                             family = Beta(link = "logit"),
#                             data = dat_aispacsp,
#                             prior = c(prior(normal(0.66, 0.07), class = "Intercept"),
#                                       prior(normal(0, 1), class = "b"),
#                                       prior(gamma(2, 0.1), class = "shape"),
#                                       prior(exponential(2), class = "sd")))
# 
# saveRDS(brm_prop_insects_beta, file = "models/brm_prop_insects_beta.rds")

# 5) get posteriors of proportion of insects
brm_prop_insects_beta = readRDS("models/brm_prop_insects_beta.rds")

fi_posts_pred = secondary_prod %>%
  filter(!id %in% dat_aispacsp$id) %>% 
  distinct(HYBAS_ID, obs_id, stream_temp_s, precip_s) %>% 
  add_epred_draws(brm_prop_insects_beta, re_formula = NULL, allow_new_levels = T,
                  value = "fi_sims") %>% 
  mutate(level = "predicted")

# 6) Use posterior proportion to convert acsp to aisp; multiply acsp by the posterior of proportion of insects. 
# then summarize this as a mean and sd
estimate_aisp = secondary_prod %>%
  filter(is.na(aisp)) %>% 
  filter(!is.na(acsp)) %>% 
  left_join(fi_posts_pred %>% filter(.draw <= 1000)) %>% 
  select(acsp, fi_sims, aisp, everything()) %>% 
  mutate(aisp_sim = acsp*fi_sims) %>% 
  select(acsp, fi_sims, aisp, aisp_sim, everything()) %>% 
  group_by(obs_id) %>% 
  reframe(aisp_mean = mean(aisp_sim, na.rm = T),
          aisp_sd = sd(aisp_sim, na.rm = T)) %>% 
  ungroup %>% 
  mutate(log_aisp_mean_s = scale(log10(aisp_mean)),
         log_aisp_sd = log10(aisp_sd),
         log_aisp_sd_s = scale(log_aisp_sd),
         aisp_sd_01 = aisp_sd/max(aisp_sd, na.rm = T)) 

mu_aisp = attributes(estimate_aisp$log_aisp_mean_s)$`scaled:center`
sd_aisp = attributes(estimate_aisp$log_aisp_mean_s)$`scaled:scale`

# # 7) estimate mean and sd relationship.  --------------------------------
# use to fill in missing sd's in the next step
# fit model of mean and sd to predict the unmeasured sd from samples that already have aisp
# brm_mean_sd_fi = brm(aisp_sd_01 ~ log_aisp_mean_s + (1|obs_id),
#                      data = estimate_aisp,
#                      family = Gamma(link = "log"),
#                      prior = c(prior(normal(0, 1), class = "Intercept"),
#                                prior(normal(0, 0.5), class = "b"),
#                                prior(exponential(3), class = "sd")))
# # # 
# saveRDS(brm_mean_sd_fi, file = "models/brm_mean_sd_fi.rds")
brm_mean_sd_fi = readRDS(file = "models/brm_mean_sd_fi.rds")

post_aisp_sd = secondary_prod %>% 
  filter(!obs_id %in% estimate_aisp$obs_id) %>% 
  mutate(log_aisp_mean = log10(aisp),
         log_aisp_mean_s = (log_aisp_mean - mu_aisp)/sd_aisp) %>% 
  distinct(obs_id, log_aisp_mean_s, aisp) %>% 
  add_epred_draws(brm_mean_sd_fi, re_formula = NULL, allow_new_levels = T, ndraws = 1000,
                  value = "aisp_sd") %>% 
  mutate(aisp_sd = 10^aisp_sd) %>% 
  group_by(obs_id, aisp) %>% 
  reframe(aisp_sd = median(aisp_sd)) %>% 
  select(obs_id, aisp_sd)

# 8) add the estimated aisp's to the main data set and save
dat_acsp = secondary_prod %>% 
  filter(obs_id %in% estimate_aisp$obs_id) %>% 
  left_join(estimate_aisp) %>% 
  mutate(aisp = aisp_mean,
         sd_estimate = "from posterior of brm_prop_insects_beta model") %>% 
  select(acsp, aisp, aisp_sd, id, everything())

dat_aisp = secondary_prod %>% 
  filter(!obs_id %in% estimate_aisp$obs_id) %>% 
  left_join(post_aisp_sd) %>% 
  mutate(sd_estimate = "from posterior of brm_mean_sd_fi model") %>% 
  select(acsp, aisp, aisp_sd, id, everything())

secondary_prod_sd = bind_rows(dat_acsp, dat_aisp)

saveRDS(secondary_prod_sd, file = "data/secondary_prod_sd.rds")

