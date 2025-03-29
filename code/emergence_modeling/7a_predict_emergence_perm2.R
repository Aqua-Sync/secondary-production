library(tidyverse)
library(brms)
library(tidybayes)
theme_set(theme_default())

emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds')
updated_gams = readRDS("models/updated_gams.rds")
max_emergence <- max(emergence_production_with_vars$mean_emergence_mgdmm2y, na.rm = T)


mod_dat = updated_gams[[1]]$data

# estimate mass, C, N, P. N and P come from Elser et al. 2000 C:N 6.3 and C:P 124 FOR ADULT INSECTS -----------------
post_mass_nutrients = mod_dat %>% 
  distinct(precip_s) %>%
  mutate(HYBAS_ID = "new") %>% 
  add_epred_draws(updated_gams[[1]], re_formula = NULL, allow_new_levels = T) %>% 
  mutate(.epred = .epred*max_emergence) %>% 
  group_by(.draw) %>% 
  reframe(mean_mgDMm2y = mean(.epred))  %>% 
  mutate(mean_gDMm2y = mean_mgDMm2y/1000) %>%
  mutate(mgCm2y = (mean_mgDMm2y*0.9)/2,
         mgNm2y = mgCm2y/6.3,
         mgPm2y = mgCm2y/124)


# estimate PUFA flux ------------------------------------------------------
pufa_mod = readRDS("models/pufa_mod.rds")
pufa_data = readRDS("data/pufa_data.rds")

unique(pufa_data$adult_units)

post_pufa = tibble(pub_name = "new") %>% 
  add_epred_draws(pufa_mod, allow_new_levels = T) %>% 
  mutate(.epred = .epred*unique(pufa_data$max_y),
         pufa_units = "ng_mg_dm") %>% 
  group_by(.draw) %>% 
  reframe(mean_ngPUFA_mgDM = mean(.epred))

saveRDS(post_pufa, file = "posteriors/post_pufa.rds")

post_pufa_flux =  post_pufa %>% 
  left_join(post_mass_nutrients %>% ungroup %>% select(.draw, mean_mgDMm2y)) %>% 
  mutate(ngPUFAm2y = mean_ngPUFA_mgDM*(mean_mgDMm2y)) %>% # multiply pufa concentration by mass concentration. 
  mutate(mgPUFAm2y = ngPUFAm2y/1e6) %>% 
  median_qi(mgPUFAm2y) %>% 
  mutate(chemical = "\u2211mgPUFAm2y") %>% 
  rename(median = mgPUFAm2y)

# combine pufa, mass, nutrients -------------------------------------------

post_mass_nutrients_pufa = post_mass_nutrients %>%
  select(-mean_gDMm2y) %>% 
  pivot_longer(cols = -.draw) %>% 
  group_by(name) %>% 
  median_qi(value) %>% 
  rename(chemical = name,
         median = value) %>% 
  bind_rows(post_pufa_flux) %>% 
  arrange(-median) %>%
  mutate(across(where(is.double), ~ round(.x, -1)))

write_csv(post_mass_nutrients_pufa, file = "tables/post_emergence_perm2.csv")
