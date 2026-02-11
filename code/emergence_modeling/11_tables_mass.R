library(tidyverse)
library(brms)
library(tidybayes)
theme_set(theme_default())

post_mass_nutrients_pufa_global = read_csv(file = "tables/post_emergence_global.csv")
post_emergence_perm2 = read_csv(file = "tables/post_emergence_perm2.csv")
post_flux_all_peryear_hybas = readRDS(file = "posteriors/hybas_predictions_mass_nutrients.rds") %>% 
  left_join(readRDS("data/hybas_regions.rds"))

unique(post_flux_all_peryear_hybas$units)
# write_csv(post_flux_all_peryear_hybas %>% rename(chemical = units), file = "posteriors/post_DM_CNP_PUFA_peryear_hybas.csv")
# write_csv(post_emergence_perm2, file = "posteriors/post_DM_CNP_PUFA_perm2.csv")
# write_csv(post_mass_nutrients_pufa_global, file = "posteriors/post_DM_CNP_PUFA_peryear_global.csv")


flux_per_region = post_flux_all_peryear_hybas %>% 
  group_by(region_name, units) %>% 
  add_tally() %>% 
  group_by(region_name, n, units) %>% 
  reframe(median_kgdm_peryear = median(median),
          mean_kgdm_peryear = mean(median)) %>% 
  arrange(units, median_kgdm_peryear) %>% 
  rename(n_basins = n)


write_csv(flux_per_region, file = "tables/flux_per_region.csv")

