library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)
theme_set(theme_default())

# load raw emergence
emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds') %>% 
  left_join(readRDS("data/HYBAS_surface_area_REDIST.rds")) %>% # add area of HYBAS water
  mutate(raw_kg_perhybas = mean_emergence_mgdmm2y*area.redist) # kg/km2 is the same as mg/m2 so this works to produce kg per hybas

# get model predicted emergence
post_flux_all_peryear_hybas = readRDS(file = "posteriors/hybas_predictions_mass_nutrients.rds") %>% 
  left_join(readRDS("data/hybas_regions.rds")) %>% 
  left_join(readRDS("data/HYBAS_surface_area_REDIST.rds") %>% 
              mutate(HYBAS_ID = as.numeric(HYBAS_ID))) %>% 
  left_join(readRDS("data/hydrobasin_vars_rssa_short.rds") %>% 
              select(HYBAS_ID, BA_km2)) %>% 
  filter(area.redist >= 1e-02) # hacky way to remove water from obvious deserts like the sahara

dm = post_flux_all_peryear_hybas %>%
  filter(grepl("kgdm", units))

# combine and plot

raw_vs_modeled_emergence_per_hybas = emergence_production_with_vars %>% 
  select(HYBAS_ID, raw_kg_perhybas) %>% 
  left_join(dm) %>% 
  ggplot(aes(x = median, y = raw_kg_perhybas)) +
  geom_pointrange(aes(xmin = lower, xmax = upper),
                  size = 0.05, linewidth = 0.05) +
  scale_x_log10(label = comma, breaks = c(1, 10, 100, 1000, 10000, 100000)) +
  scale_y_log10(label = comma, breaks = c(1, 10, 100, 1000, 10000, 100000),
  limits = c(NA, 100000)) +
  labs(y = "Raw kgDM/hybas/year",
       x = "Modeled kgDM/hybas/year") +
  geom_abline()

ggsave(raw_vs_modeled_emergence_per_hybas, file = "plots/raw_vs_modeled_emergence_per_hybas.jpg",
       width = 6.5, height = 6.5)


# Get deviations. Are they related to water quality?
deviations =  emergence_production_with_vars %>% 
  select(HYBAS_ID, raw_kg_perhybas,mean_emergence_mgdmm2y) %>% 
  left_join(dm) %>% 
  mutate(deviation = raw_kg_perhybas - median)

cas_names = readRDS(file = "data/cas_names.rds") %>% 
  mutate(chemical_category = case_when(chemical == "Selenium" ~ "Se",
                                       chemical == "Zinc" ~ "Zn",
                                       chemical == "Mercury" ~ "Hg",
                                       chemical == "Lead" ~ "Pb",
                                       chemical == "Copper" ~ "Cu",
                                       chemical == "Cadmium" ~ "Cd"
  ))
# Wolfram predictions of contaminants. Generated in wrangle_modeled_water.R. It uses water concentrations from Jakob Wolfram on seafile.rlp.net...then reformats.
modeled_water = readRDS(file = "data/modeled_water.rds") # values have been corrected for minimums with essential elements (i.e., if water concentrations indicate zero Se but still has emergence, then we need to assign a minimum amount to flux b/c flux of Se in tissues can't also be zero)

deviations %>% 
  left_join(modeled_water,relationship = "many-to-many") %>% 
  ggplot(aes(x = mean.conc.year, y = deviation)) + 
  geom_point() +
  facet_wrap(~cas, scales = "free_x")

deviations %>% 
  left_join(modeled_water,relationship = "many-to-many") %>% 
  ggplot(aes(x = mean.conc.year, y = raw_kg_perhybas)) + 
  geom_point() +
  scale_y_log10() +
  geom_smooth(method = lm) +
  facet_wrap(~cas)
