library(tidyverse)
library(brms)
library(tidybayes)
library(bit64)
library(cccapi)

# load data and models
# raw contaminants data
contaminants_ides = readRDS(file = "data/contaminants.rds") %>% 
  filter(grepl("cide", chemical_category)) 

unique(contaminants_ides$chemical)

# load dry mass emergence predictions
flux_predictions_all = readRDS("posteriors/hybas_predictions_mass_nutrients.rds") %>% 
  filter(units == "kgdm_peryear") %>% 
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

# check to match the chemical and cas id
ides_wehave = contaminants_ides %>% distinct(chemical, chemical_category) %>% 
  left_join(cas_names %>% mutate(chemical = str_to_lower(chemical)) %>% select(-chemical_category)) %>% 
  filter(!is.na(cas))

# Jakob Wolframs predictions of contaminants. Generated in wrangle_modeled_water.R. It uses water concentrations from Jakob Wolfram on seafile.rlp.net...then reformats.
modeled_water_ides = readRDS(file = "data/modeled_water.rds") %>%  # values have been corrected for minimums with essential elements (i.e., if water concentrations indicate zero Se but still has emergence, then we need to assign a minimum amount to flux b/c flux of Se in tissues can't also be zero)
  filter(cas %in% c(unique(ides_wehave$cas)))

# combined modeled water ides so that we have mean concentrations of insecticides, fungicides, or herbicides. We need to 
# do this b/c that is what our models of tissue concentrations are based off of (i.e., any concentration of fungicide, not particular types of fungicide)
modeled_water_ides_mean = modeled_water_ides %>% left_join(ides_wehave %>% distinct(cas, chemical_category)) %>% 
  group_by(HYBAS_ID, chemical_category) %>% 
  reframe(mean.conc.year = mean(mean.conc.year, na.rm = T),
          mean.det.year = mean(mean.det.year, na.rm = T),
          max.conc.year = max(max.conc.year, na.rm = T))

modeled_water = modeled_water_ides_mean

# Models that predict contaminant tissue concentrations as a function of water concentrations. Different models per contaminant.
mod_list = readRDS(file = "models/mod_list.rds") 

# This code generates aquatic concentrations of contaminants. mean.conc.year is the log10 mean ug/L, mean.det.year is detection. 
# 1) load custom function

source("code/custom_functions/get_hybas_cide_preds.r") # function to combine biomass and contaminant concentrations, then multiply to get posterior prediction of contaminant flux in each HYBAS

# 2) Filter the models to only match the insecticides/herbcides/or fungicides
mod_list_ides = Filter(function(model) model$data2$chemical_category %in% c("fungicide", "herbicide", "insecticide"), mod_list)

# 3) Run function on each model. Result is combined biomass and contaminant concentrations for all HYBAS_IDs and their product (total contaminant flux per year)
hybas_predictions_ides = lapply(mod_list_ides, get_hybas_cide_preds) 

hybas_predictions_ides_df = bind_rows(hybas_predictions_ides) %>% 
  left_join(readRDS("data/hydrobasin_vars_rssa_short.rds") %>% 
              select(HYBAS_ID, SUB_AREA) %>% 
              mutate(HYBAS_ID = as.character(HYBAS_ID)))

saveRDS(hybas_predictions_ides_df, file = "posteriors/hybas_predictions_pest_herb_fungicide.rds")

rm(modeled_water)

# split and save by separate elements

hybas_predictions_ides_df = readRDS(file = "posteriors/hybas_predictions_pest_herb_fungicide.rds") 

split_data_ides = split(hybas_predictions_ides_df, hybas_predictions_ides_df$element)

# Save each split tibble as an .rds file
lapply(names(split_data_ides), function(element) {
  filename <- paste0("posteriors/hybas_predictions_mgperyear_", element, ".rds")
  saveRDS(split_data_ides[[element]], file = filename)
})

# summarize ---------------------------------------------------------------
hybas_predictions_ides = readRDS(file = "posteriors/hybas_predictions_ides.rds")

# Global Annual Metric Tons
bind_rows(hybas_predictions_ides) %>% 
  group_by(chemical_category) %>% 
  reframe(sum = sum(chem_flux_mg_year))


all_ids = bind_rows(hybas_predictions_ides) 

raw_model_data = lapply(mod_list_ides, function(mod) mod$data)


all_ids %>%
  group_by(chemical_category) %>% 
  sample_n(1000) %>%
  filter(x_s == min(x_s, na.rm = T)|x_s == max(x_s, na.rm = T)) %>%
  ggplot(aes(x = x_s, y = y_s)) +
  geom_lineribbon(alpha = 0.3, aes(ymin = y_s_lower95, ymax = y_s_upper95)) +
  facet_wrap(~chemical_category, scales = "free_y") +
  geom_point(data = bind_rows(raw_model_data), aes(y = y_s, x = x_s)) +
  # scale_x_log10() +
  # scale_y_log10() +
  NULL

plot(conditional_effects(mod_list_ides[[1]]), points = T)
