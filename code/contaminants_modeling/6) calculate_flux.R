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
  mutate(HYBAS_ID = as.character(HYBAS_ID)) 

# contaminant cas numbers and names
cas_names = readRDS(file = "data/cas_names.rds") %>% 
  mutate(chemical_category = case_when(chemical == "Selenium" ~ "Se",
                                      chemical == "Zinc" ~ "Zn",
                                      chemical == "Mercury" ~ "Hg",
                                      chemical == "Lead" ~ "Pb",
                                      chemical == "Copper" ~ "Cu",
                                      chemical == "Cadmium" ~ "Cd"
                                      ))
# Wolfram predictions of contaminants. Generated in get_cas_names.R. It uses water concentrations from Jakob Wolfram on seafile.rlp.net...then reformats. 
modeled_water = readRDS(file = "data/modeled_water.rds") 

# Models that predict contaminant tissue concentrations as a function of water concentrations. Different models per contaminant.
mod_list = readRDS(file = "models/mod_list.rds") 

# This code generates aquatic concentrations of contaminants. mean.conc.year is the log10 mean ug/L, mean.det.year is detection. 
# 1) load custom function

source("code/custom_functions/get_contaminant_preds.r") # function to combine biomass and contaminant concentrations, then multiply to get posterior prediction of contaminant flux in each HYBAS

# 2) get list of the chemical names we have data for
chemicals_we_have = cas_names %>% filter(!is.na(chemical_category)) %>%
  pull(chemical_category)

# 3) Filter the models to only match the chemicals we have data for
filtered_mod_list = Filter(function(m) m$data2$chemical %in% chemicals_we_have , mod_list)

# 4) Run function on each model. Result is combined biomass and contaminant concentrations for all HYBAS_IDs and their product (total contaminant flux per year)
hybas_predictions = lapply(filtered_mod_list, get_contaminant_preds) 

# 6) replace zeros with baseline values (only if contaminant is an essential element, otherwise, they remain as 0)
library(data.table)
source("code/custom_functions/get_baseline_values.r") # function to combine biomass and contaminant concentrations, then multiply to get posterior prediction of contaminant flux in each HYBAS

for(i in 1:length(hybas_predictions)){
  baselines = baseline_values %>% left_join(cas_names) %>% 
    filter(chemical == unique(hybas_predictions[[i]]$chemical))
  baseline_median = baselines %>% select(tissue_ng_mg) %>% pull()
  baseline_lower = baselines %>% select(.lower) %>% pull()
  baseline_upper = baselines %>% select(.upper) %>% pull()
  setDT(hybas_predictions[[i]])[chem_flux_mg_year == 0, chem_flux_mg_year := baseline_median]
  setDT(hybas_predictions[[i]])[chem_flux_mg_year_lower95 == 0, chem_flux_mg_year_lower95 := baseline_lower]
  setDT(hybas_predictions[[i]])[chem_flux_mg_year_upper95 == 0, chem_flux_mg_year_upper95 := baseline_upper]
}


# 7) select columns and save
hybas_predictions_jakob = lapply(hybas_predictions, function(m) {
  m %>% select(HYBAS_ID, HYBAS_L12, cas, chem_flux_mg_year, chem_flux_mg_year_lower95, chem_flux_mg_year_upper95) })

names = lapply(hybas_predictions_jakob, function(m) unique(m$chemical))

save_hybas_predictions <- function(data, name) {
  filename <- file.path("posteriors", paste0("hybas_predictions_", name, ".rds"))
  saveRDS(data, filename)
}

# 6) iterate through each element and save as an RDS file
mapply(save_hybas_predictions, hybas_predictions_jakob, names)

# summarize ---------------------------------------------------------------
# Global Annual Metric Tons
bind_rows(hybas_predictions) %>% 
  group_by(chemical) %>% 
  reframe(sum_MT = sum(chem_flux_mg_year)/1e9,
          sum_MT_low = sum(chem_flux_mg_year_lower95)/1e9,
          sum_MT_high = sum(chem_flux_mg_year_upper95)/1e9) %>% 
  arrange(sum_MT) %>%
  filter(!is.na(chemical))

flux_by_region = bind_rows(hybas_predictions) %>% 
  group_by(chemical, region_name) %>% 
  reframe(sum_MT = sum(chem_flux_mg_year)/1e9,
          sum_MT_low = sum(chem_flux_mg_year_lower95)/1e9,
          sum_MT_high = sum(chem_flux_mg_year_upper95)/1e9) %>% 
  arrange(sum_MT) %>%
  filter(!is.na(chemical))

# plot predictions --------------------------------------------------------

filtered_predictions_list = list()

for(i in 1:length(hybas_predictions)){
  x_intervals <- seq(min(hybas_predictions[[i]]$x_s, na.rm = TRUE), 
                     max(hybas_predictions[[i]]$x_s, na.rm = TRUE), 
                     length.out = 100)
  
  # filter to retain rows where x_s is closest to the interval values
  filtered_predictions_list[[i]] = hybas_predictions[[i]] %>%
    filter(x_s %in% sapply(x_intervals, function(x) x_s[which.min(abs(x_s - x))]))
  
}
filtered_predictions = bind_rows(filtered_predictions_list)


filtered_predictions %>% 
  ggplot(aes(x = water_ug_l_raw, y = y_mg_kg)) +
  geom_line() +
  geom_ribbon(aes(ymin = y_mg_kg_lower95, ymax = y_mg_kg_upper95), alpha = 0.2) +
  geom_ribbon(aes(ymin = y_mg_kg_lower50, ymax = y_mg_kg_upper50), alpha = 0.2)+
  geom_point(data = contaminants,
             aes(y = adult_conc_ng_mg_dm, x = water_conc_ug_l), color = 'red') +
  labs(y = "Contaminant flux (mg/kgDM/y)",
       x = "Contaminant in water (\u03bcg/l)") +
  scale_y_log10() +
  scale_x_log10() +
  theme_classic() + 
  facet_wrap(~chemical) +
  geom_point(data = se_baseline %>% filter(.draw <= 100) %>% 
               mutate(chemical = "Selenium"), aes(x = 1e-03, y = .epred)) +
  NULL

