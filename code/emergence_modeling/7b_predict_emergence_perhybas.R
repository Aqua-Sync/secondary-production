library(tidyverse)
library(tidybayes)

# kgdm_peryear ------------------------------------------------------------


# Use the fitted parameters from regression models to predict emergence at unmeasured sites

# 1) load data and models -------------------------------------------------
# load data
emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds')
hybas_filter <- readRDS("data/hybas_filtered.rds")

data_to_predict = readRDS("data/data_to_predict.rds") %>% 
  filter(HYBAS_ID %in% hybas_filter)

data_to_predict_list = data_to_predict %>% 
  group_by(region) %>% group_split() # basin-level predictor variables by continent

hybas_regions <- readRDS("data/hybas_regions.rds")
post_pufa = readRDS(file = "posteriors/post_pufa.rds")

# load models
updated_gams = readRDS("models/updated_gams.rds")

# get max emergence to unstandardize
max_emergence = max(emergence_production_with_vars$mean_emergence_mgdmm2y, na.rm = T)

# load area of water in each hybas (km2)
hybas_area = readRDS("data/HYBAS_surface_area_REDIST.rds") # redistributed surface areas from Jakob. 


# 2) sample posterior -----------------------------------------------------
# dry mass

# estimates median and CrI's of emergence per hybas in standardized units of mgdmm2y/max(mgdmm2y)
post_summary = vector("list", length(data_to_predict_list))

for(i in seq_along(data_to_predict_list)) {
  post_summary[[i]] = data_to_predict_list[[i]] %>%
    # slice(1:5) %>%
    select(HYBAS_ID, precip_s, stream_temp_s) %>%
    mutate(author_year = "new") %>%
    add_epred_draws(updated_gams[[3]], allow_new_levels = TRUE, re_formula = NULL, ndraws = 100) %>%
    group_by(HYBAS_ID, precip_s, stream_temp_s) %>%
    reframe(
      mean = mean(.epred),
      sd = sd(.epred),
      median = median(.epred),
      lower = quantile(.epred, 0.025),
      lower50 = quantile(.epred, 0.25),
      upper50 = quantile(.epred, 0.75),
      upper = quantile(.epred, 0.975)
    )
}

saveRDS(post_summary, file = "posteriors/post_summary.rds")

# PUFA
post_pufa_hybas_summary = list()
# 
for(i in seq_along(data_to_predict_list)) {
  set.seed(20202)
  post_pufa_hybas_summary[[i]] = data_to_predict_list[[i]] %>%
    # slice(1:6000) %>%
    select(HYBAS_ID, precip_s, stream_temp_s) %>%
    mutate(author_year = "new") %>%
    add_epred_draws(updated_gams[[3]], allow_new_levels = TRUE, re_formula = NULL, ndraws = 100) %>%
    mutate(mgdmm2 = .epred*max_emergence/1000/1000) %>% 
    select(-.epred) %>%
    left_join(post_pufa) %>%
    mutate(.epred = mgdmm2*mean_ngPUFA_mgDM,
           .epred = .epred*0.000001) %>% # convert ng to mg
    group_by(HYBAS_ID, precip_s, stream_temp_s) %>%
    reframe(
      median = median(.epred),
      lower = quantile(.epred, 0.025),
      lower50 = quantile(.epred, 0.25),
      upper50 = quantile(.epred, 0.75),
      upper = quantile(.epred, 0.975)
    ) %>%
    mutate(units = "mgPUFAm2")
}

saveRDS(post_pufa_hybas_summary, file = "posteriors/post_pufa_hybas_summary.rds")

post_summary = readRDS(file = "posteriors/post_summary.rds")
post_pufa_hybas_summary = readRDS(file = "posteriors/post_pufa_hybas_summary.rds") 

# 3 estimate total flux per hybas ------------------------------------------
# Summarize and convert dry mass to C, N, P. Then combine with PUFAs
post_flux_kgdm_perm2year_hybas = bind_rows(post_summary) %>% 
  pivot_longer(cols = c(-HYBAS_ID, -precip_s, stream_temp_s)) %>% 
  mutate(kgdmkm2y = value*max_emergence) %>% # kg/km2 is the same as mg/m2
  select(-value) %>% 
  # left_join(hybas_area) %>% # km2 of water
  # mutate(kgdmhybasyr = kgdmkm2y*area.redist) %>% 
  select(HYBAS_ID, name, kgdmkm2y) %>% 
  pivot_wider(names_from = name, values_from = kgdmkm2y) %>% 
  mutate(units = "kgdmkm2_peryear") %>%
  left_join(hybas_regions) %>% 
  group_by(region_name) %>% 
  mutate(median_region = median(median))

saveRDS(post_flux_kgdm_perm2year_hybas, file = "posteriors/post_flux_kgdm_perm2_perhybas.rds")

post_flux_kgdm_peryear_hybas = bind_rows(post_summary) %>% 
  pivot_longer(cols = c(-HYBAS_ID, -precip_s, -stream_temp_s)) %>% 
  mutate(kgdmkm2y = value*max_emergence) %>% # kg/km2 is the same as mg/m2
  select(-value) %>% 
  left_join(hybas_area) %>% # km2 of water
  mutate(kgdmhybasyr = kgdmkm2y*area.redist) %>% 
  select(HYBAS_ID, name, kgdmhybasyr) %>% 
  pivot_wider(names_from = name, values_from = kgdmhybasyr) %>% 
  mutate(units = "kgdm_peryear")

saveRDS(post_flux_kgdm_peryear_hybas, file = "posteriors/hybas_predictions_emergenceDryMass.rds")

post_flux_kgC_peryear_hybas = post_flux_kgdm_peryear_hybas %>%
  mutate_at(vars(2:5), ~ (. * 0.9)/2) %>% 
  mutate(units = "kgC_peryear")

post_flux_kgN_peryear_hybas = post_flux_kgC_peryear_hybas %>%
  mutate_at(vars(2:5), ~ ./6.3) %>% 
  mutate(units = "kgN_peryear")  

post_flux_kgP_peryear_hybas = post_flux_kgC_peryear_hybas %>%
  mutate_at(vars(2:5), ~ ./124) %>% 
  mutate(units = "kgP_peryear")

post_flux_kgPUFA_peryear_hybas = bind_rows(post_pufa_hybas_summary) %>% 
  pivot_longer(cols = c(-HYBAS_ID, -precip_s, -stream_temp_s, -units)) %>%
  left_join(hybas_area) %>% # km2 of water
  mutate(kgPUFAhybasyr = value*area.redist) %>% 
  select(HYBAS_ID, name, kgPUFAhybasyr) %>% 
  pivot_wider(names_from = name, values_from = kgPUFAhybasyr) %>% 
  mutate(units = "kgPUFA_peryear")

hybas_predictions_mass_nutrients = bind_rows(post_flux_kgC_peryear_hybas,
                                        post_flux_kgdm_peryear_hybas,
                                        post_flux_kgN_peryear_hybas,
                                        post_flux_kgP_peryear_hybas,
                                        post_flux_kgPUFA_peryear_hybas) %>% 
  left_join(readRDS("data/hydrobasin_vars_rssa_short.rds") %>% 
              select(HYBAS_ID, SUB_AREA))

saveRDS(hybas_predictions_mass_nutrients, file = "posteriors/hybas_predictions_mass_nutrients.rds")

hybas_predictions_mass_nutrients = readRDS(file = "posteriors/hybas_predictions_mass_nutrients.rds") 

# split and save by separate elements

split_data = split(hybas_predictions_mass_nutrients, hybas_predictions_mass_nutrients$units)

# Save each split tibble as an .rds file
lapply(names(split_data), function(unit) {
  filename <- paste0("posteriors/hybas_predictions_", unit, ".rds")
  saveRDS(split_data[[unit]], file = filename)
})

