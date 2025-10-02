library(tidyverse)
library(brms)
library(tidybayes)
theme_set(theme_default())

emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds')
updated_gams = readRDS("models/updated_gams.rds")
max_emergence <- max(emergence_production_with_vars$mean_emergence_mgdmm2y, na.rm = T)

data_to_predict = readRDS("data/data_to_predict.rds") %>% 
  filter(SUB_AREA > 0) %>% 
  left_join(readRDS("data/hybas_regions.rds")) %>% 
  filter(region_name != "Greenland") %>%
  left_join(readRDS("data/hybas_covariates.rds") %>% select(HYBAS_ID, LAT, terr_biom))

mod = updated_gams[[3]]
mod_dat = mod$data

# estimate mass, C, N, P. N and P come from Sterner and Elser et al. 2002 page 141 C:N 6.3 and C:P 124 FOR ADULT INSECTS -----------------
set.seed(20202)
# post_mass_nutrients = mod_dat %>% 
#   select(-emerge_1, -HYBAS_ID) %>% 
#   distinct() %>%
#   mutate(HYBAS_ID = "new") %>% 
#   add_epred_draws(mod, re_formula = NULL, allow_new_levels = T) %>% 
#   mutate(.epred = .epred*max_emergence) %>% 
#   group_by(.draw) %>% 
#   reframe(mean_mgDMm2y = mean(.epred))  %>% 
#   mutate(mean_gDMm2y = mean_mgDMm2y/1000) %>%
#   mutate(mgCm2y = (mean_mgDMm2y*0.9)/2,
#          mgNm2y = mgCm2y/6.3,
#          mgPm2y = mgCm2y/124)

post_mass_nutrients = data_to_predict %>% 
  distinct(precip_s, stream_temp_s, HYBAS_ID, region) %>% 
  group_by(region) %>% 
  slice_sample(prop = 0.01) %>% 
  add_epred_draws(mod, 
                  re_formula = NULL, 
                  allow_new_levels = T,
                  ndraws = 500) %>% 
  ungroup %>%
  mutate(mgDMm2y = .epred*max_emergence) %>% 
    mutate(mgCm2y = (mgDMm2y*0.9)/2,
           mgNm2y = mgCm2y/6.3,
           mgPm2y = mgCm2y/124)

saveRDS(post_mass_nutrients %>% left_join(readRDS("data/hybas_regions.rds")), file = "posteriors/post_mass_nutrients.rds")

# repeat subsampling above. this shows that averaging over 1% of subbasins in each
# region produces stable estimates of mass. i.e., we do not need to average over all 1.03 million sub-basins
sample_perm2 = function(data_to_predict){
  data_to_predict %>% 
    distinct(precip_s, stream_temp_s, HYBAS_ID, region) %>% 
    group_by(region) %>% 
    slice_sample(prop = 0.01) %>% 
    add_epred_draws(mod, 
                    re_formula = NULL, 
                    allow_new_levels = T,
                    ndraws = 500) %>% 
    ungroup %>%
    mutate(.epred = .epred*max_emergence) %>% 
    median_qi(.epred)
}

# sample_perm2_n = lapply(1:10, function(i) sample_perm2(data_to_predict)) 
# 
# saveRDS(sample_perm2_n, file = "posteriors/sample_perm2_n.rds")

sample_perm2_n = readRDS(file = "posteriors/sample_perm2_n.rds")

bind_rows(sample_perm2_n) %>% 
  rownames_to_column() %>%
  mutate(rowname = parse_number(rowname)) %>% 
  ggplot(aes(x = rowname, y = .epred, ymin = .upper ,ymax = .lower)) +
  geom_pointrange() +
  scale_y_log10()


# sampler_perm2 biome -----------------------------------------------------

post_mass_nutrients_biome = data_to_predict %>% 
  distinct(precip_s, stream_temp_s, HYBAS_ID, region, terr_biom) %>% 
  group_by(terr_biom) %>% 
  slice_sample(prop = 0.01) %>% 
  add_epred_draws(mod, 
                  re_formula = NULL, 
                  allow_new_levels = T,
                  ndraws = 300) %>% 
  ungroup %>%
  mutate(mgDMm2y = .epred*max_emergence) %>% 
  mutate(mgCm2y = (mgDMm2y*0.9)/2,
         mgNm2y = mgCm2y/6.3,
         mgPm2y = mgCm2y/124)
# 
saveRDS(post_mass_nutrients_biome, file = "posteriors/post_mass_nutrients_biome.rds")


# estimate PUFA flux ------------------------------------------------------
pufa_mod = readRDS("models/pufa_mod_taxon_epadha.rds")
pufa_data = readRDS("data/pufa_data.rds")
# 
# unique(pufa_data$adult_units)
# 
# set.seed(20202)
post_pufa = tibble(pub_name = "new",
                   taxon = "new") %>%
  add_epred_draws(pufa_mod, allow_new_levels = T) %>%
  mutate(.epred = .epred*unique(pufa_data$max_y),
         pufa_units = "ng_mg_dm") %>%
  group_by(.draw) %>%
  reframe(mean_ngPUFA_mgDM = mean(.epred))

saveRDS(post_pufa, file = "posteriors/post_pufa.rds")

post_pufa = readRDS(file = "posteriors/post_pufa.rds")

post_pufa_flux =  post_pufa %>% 
  left_join(post_mass_nutrients %>% ungroup %>% select(.draw, mgDMm2y)) %>% 
  mutate(ngPUFAm2y = mean_ngPUFA_mgDM*(mgDMm2y)) %>% # multiply pufa concentration by mass concentration. 
  mutate(mgPUFAm2y = ngPUFAm2y/1e6) %>% 
  median_qi(mgPUFAm2y, na.rm = T) %>% 
  mutate(chemical = "\u2211mgPUFAm2y") %>% 
  rename(median = mgPUFAm2y)

# combine pufa, mass, nutrients -------------------------------------------

post_mass_nutrients_pufa = post_mass_nutrients %>%
  ungroup %>% 
  select(.draw, contains("m2")) %>% 
  pivot_longer(cols = -.draw) %>% 
  group_by(name) %>% 
  median_qi(value, .width = 0.75) %>% 
  rename(chemical = name,
         median = value) %>% 
  bind_rows(post_pufa_flux) %>% 
  arrange(-median) %>%
  mutate(across(where(is.double), ~ round(.x, -1)))

write_csv(post_mass_nutrients_pufa, file = "tables/post_emergence_perm2.csv")
