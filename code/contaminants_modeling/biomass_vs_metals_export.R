# How well does biomass export match with chemical export?
library(tidyverse)
library(janitor)
library(tidybayes)
library(brms)
library(ggthemes)
library(isdbayes)
library(viridis)
theme_set(brms::theme_default())

# water concentrations per hybas
modeled_water <- readRDS("data/modeled_water.rds")

# biomass export per hybas
hybas_predictions_kgdm_peryear <- readRDS("data/hybas_predictions_kgdm_peryear.rds")

# metals export per hybas
hybas_predictions_metals <- readRDS("data/hybas_predictions_metals.rds")

ids = hybas_predictions_kgdm_peryear %>% ungroup %>% sample_n(50000) %>% pull(HYBAS_ID)

biomass_filtered = hybas_predictions_kgdm_peryear %>% filter(HYBAS_ID %in% ids) %>% 
  rename(mean_biomass_mg = mean,
         sd_biomass_mg = sd) %>% 
  mutate(HYBAS_ID = as.character(HYBAS_ID))

metals_filtered = hybas_predictions_metals %>% filter(HYBAS_ID %in% as.character(ids))

metals_filtered %>% 
  left_join(biomass_filtered %>% select(HYBAS_ID, mean_biomass_mg , sd_biomass_mg)) %>% 
  ggplot(aes(x = mean_biomass_mg + .001, y = chem_flux_mg_year + .001)) +
  geom_point(shape = ".") +
  facet_wrap(~element, scales = "free") +
  scale_x_log10() +
  scale_y_log10()
  
