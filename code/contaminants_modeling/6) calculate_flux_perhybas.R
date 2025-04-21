library(tidyverse)
library(brms)
library(tidybayes)
library(bit64)
library(cccapi)

# load data and models
# raw contaminants data
contaminants = readRDS(file = "data/contaminants.rds") %>% 
  mutate(chemical = case_when(chemical_category == "Se" ~ "Selenium",
                                       chemical_category == "Zn" ~ "Zinc",
                                       chemical_category == "Hg" ~ "Mercury",
                                       chemical_category == "Pb" ~ "Lead",
                                       chemical_category == "Cu" ~ "Copper",
                                       chemical_category == "Cd" ~ "Cadmium"
  ))

# load dry mass emergence predictions
flux_predictions_all = readRDS("posteriors/flux_predictions_all.rds") %>% 
  left_join(readRDS("data/hybas_regions.rds")) %>% 
  mutate(HYBAS_ID = as.character(HYBAS_ID),
         HYBAS_L12 = bit64::as.integer64(HYBAS_ID)) 

# contaminant cas numbers and names
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

# Models that predict contaminant tissue concentrations as a function of water concentrations. Different models per contaminant.
mod_list = readRDS(file = "models/mod_list.rds") 

# This code generates aquatic concentrations of contaminants. mean.conc.year is the log10 mean ug/L, mean.det.year is detection. 
# 1) load custom function

source("code/custom_functions/get_hybas_contaminant_preds.r") # function to combine biomass and contaminant concentrations, then multiply to get posterior prediction of contaminant flux in each HYBAS

# 2) get list of the chemical names we have data for
chemicals_we_have = cas_names %>% filter(!is.na(chemical_category)) %>%
  pull(chemical_category)

# 3) Filter the models to only match the chemicals we have data for
filtered_mod_list = Filter(function(m) m$data2$chemical %in% chemicals_we_have , mod_list)

# 4) Run function on each model. Result is combined biomass and contaminant concentrations for all HYBAS_IDs and their product (total contaminant flux per year)
hybas_predictions = lapply(filtered_mod_list, get_hybas_contaminant_preds) 

saveRDS(hybas_predictions, file = "posteriors/hybas_predictions.rds")

# summarize ---------------------------------------------------------------
hybas_predictions = readRDS(file = "posteriors/hybas_predictions.rds")

# Global Annual Metric Tons
bind_rows(hybas_predictions) %>% 
  group_by(chemical) %>% 
  median_qi(global_flux_MT_peryr)


