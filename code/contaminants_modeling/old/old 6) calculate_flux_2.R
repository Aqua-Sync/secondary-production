library(tidyverse)
library(brms)
library(tidybayes)
library(bit64)
library(cccapi)

# load data and models
contaminants = readRDS(file = "data/contaminants.rds")
flux_predictions_all = readRDS("posteriors/flux_predictions_all.rds")
cas_names = readRDS(file = "data/cas_names.rds") %>% 
  mutate(chemical_category = case_when(chemical == "Selenium" ~ "Se",
                                      chemical == "Zinc" ~ "Zn",
                                      chemical == "Mercury" ~ "Hg",
                                      chemical == "Lead" ~ "Pb",
                                      chemical == "Copper" ~ "Cu",
                                      chemical == "Cadmium" ~ "Cd"
                                      ))
modeled_water = readRDS(file = "data/modeled_water.rds")
mod_list = readRDS(file = "models/mod_list.rds")

# This code generates aquatic concentrations of contaminants. # concentrations from Jakob on seafile.rlp.net...then reformat. mean.conc.year is the mean, mean.det.year is detection. 
#1) filter to desired contaminant
sort(unique(cas_names$chemical))
source("code/custom_functions/get_contaminant_preds.r")



hybas_predictions %>%
  filter(x_s == min(x_s, na.rm = T)|x_s == max(x_s, na.rm = T)) %>% 
  bind_rows(hybas_predictions %>%
              arrange(x_s) %>% 
              slice_sample(n = 70000)) %>% 
  # sample_n(80000) %>%
  ggplot(aes(x = exp((x_s*mod$data2$sd_x$`scaled:scale`) + 
                       mod$data2$mean_x$`scaled:center`), y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = y_lower, ymax = y_upper), alpha = 0.2) +
  geom_point(data = mod$data ,
             aes(y = y_s*mod$data2$max_y), color = 'red') +
  labs(y = "Contaminant flux (mg/kgDM/y)",
       x = "Contaminant in water (\u03bcg/l)") +
  scale_y_log10() +
  scale_x_log10() +
  NULL

sum(hybas_predictions$chem_flux_mg_year_lower, na.rm = T)/1e09
sum(hybas_predictions$chem_flux_mg_year, na.rm = T)/1e09
sum(hybas_predictions$chem_flux_mg_year_upper, na.rm = T)/1e09
