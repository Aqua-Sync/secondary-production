# How well does biomass export match with chemical export?
library(tidyverse)
library(janitor)
library(tidybayes)
library(brms)
library(ggthemes)
library(isdbayes)
library(viridis)
library(relaimpo)
theme_set(brms::theme_default())

# water concentrations per hybas
modeled_water <- readRDS("data/modeled_water.rds")

# biomass export per hybas
hybas_predictions_kgdm_peryear <- readRDS("posteriors/hybas_predictions_kgdm_peryear.rds")


# biomass production per hybas
post_flux_kgdm_perm2_perhybas <- readRDS("posteriors/post_flux_kgdm_perm2_perhybas.rds")

# metals export per hybas
hybas_predictions_metals <- readRDS("posteriors/hybas_predictions_metals.rds")

ids = hybas_predictions_kgdm_peryear %>% ungroup %>% sample_n(50000) %>% pull(HYBAS_ID)

biomass_filtered = hybas_predictions_kgdm_peryear %>% filter(HYBAS_ID %in% ids) %>% 
  rename(mean_biomass_mg = mean,
         sd_biomass_mg = sd) %>% 
  mutate(HYBAS_ID = as.character(HYBAS_ID))

metals_filtered = hybas_predictions_metals %>% filter(HYBAS_ID %in% as.character(ids)) %>% 
  left_join(biomass_filtered %>% dplyr::select(HYBAS_ID, mean_biomass_mg , sd_biomass_mg)) %>%
  left_join(modeled_water %>% dplyr::select(cas, HYBAS_ID, mean.conc.year) %>% mutate(HYBAS_ID = as.character(HYBAS_ID))) %>% 
  left_join(post_flux_kgdm_perm2_perhybas %>% dplyr::select(HYBAS_ID, mean) %>% rename(flux_perm2 = mean) %>% 
              mutate(HYBAS_ID = as.character(HYBAS_ID)))
  
  

metals_filtered %>% 
  ggplot(aes(x = mean_biomass_mg + .001, y = chem_flux_mg_year + .001)) +
  geom_point(shape = ".") +
  facet_wrap(~element, scales = "free") +
  scale_x_log10() +
  scale_y_log10()
  

metals_filtered %>% 
  ggplot(aes(x = mean.conc.year, y = chem_flux_mg_year)) +
  geom_point(shape = ".") +
  facet_wrap(~element, scales = "free") +
  geom_smooth() +
  # scale_x_log10() + # mean.conc.year is already log transformed
  scale_y_log10() +
  NULL


metals_filtered %>% 
  filter(element == "Pb") %>% # just need to filter to one element. Doesn't matter which one
  # filter(mean.conc.year > -1.5) %>% 
  # filter(flux_perm2 > 0) %>% 
  ggplot(aes(x = mean.conc.year, y = flux_perm2)) + 
  geom_point(shape = ".") +
  scale_y_log10() + 
  geom_smooth(method = "lm") +
  NULL
  

# linear model to partition variance
metals_filtered_s = metals_filtered %>% group_by(cas) %>% 
  mutate(chem_flux_s = scale(log10(chem_flux_mg_year + 1)),
         biomass_flux_s = scale(log10(mean_biomass_mg + 1)),
         water_conc_s = scale(mean.conc.year + 1)) # mean.conc.year is already log 10 transformed

# fit models on each contaminant, then calculate relative importance of predictors for each contamiant
mod_partition = metals_filtered_s %>% 
  group_by(element) %>% 
  nest() %>% 
  mutate(mod = map(data, ~lm(chem_flux_s ~ 1 + biomass_flux_s + water_conc_s, data = .))) %>% 
  mutate(rel_impo = map(mod, ~calc.relimp(, object = ., type = c("lmg"))))

# get just the relative importance values (borrowed code from Brandt et al. 2024)
lmg_list = NULL

for(i in 1:length(mod_partition$rel_impo)){
  lmg_list[[i]] = tibble(estimate = mod_partition$rel_impo[[i]]@lmg) %>% 
    mutate(term = mod_partition$rel_impo[[i]]@lmg.rank %>% names(),
           chemical = mod_partition$element[i])
}


rel_impo_table = bind_rows(lmg_list) %>% 
  mutate(estimate = signif(estimate, 2)) %>% 
  pivot_wider(names_from = term, values_from = estimate) %>% 
  arrange(-water_conc_s)

write_csv(rel_impo_table, file = "tables/rel_impo_table.csv")

bind_rows(lmg_list) %>% 
  filter(term == "water_conc_s") %>% 
  ggplot(aes(x = chemical, y = estimate, color = term)) + 
  geom_point()

