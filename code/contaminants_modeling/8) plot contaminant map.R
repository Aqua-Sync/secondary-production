library(tidyverse)
library(brms)
library(tidybayes)
library(bit64)
library(cccapi)
library(viridis)
library(ggmap)
library(maps)

#1) Trim hybas_predictions to only include columns with hybas-level contaminant flux
# hybas_predictions = readRDS(file = "posteriors/hybas_predictions.rds")
# 
# # Read full RDS file but extract only necessary parts lazily
# hybas_contaminant_predictions = lapply(seq_along(hybas_predictions), function(i) {
#   hybas_predictions[[i]] %>% select(HYBAS_ID, starts_with("chem_flux"), element)
# })
# 
# hybas_contaminant_predictions = bind_rows(hybas_contaminant_predictions)
# 
# saveRDS(hybas_contaminant_predictions, file = "posteriors/hybas_contaminant_predictions.rds")

# region_names and lat long

hybas_contaminant_predictions = readRDS(file = "posteriors/hybas_contaminant_predictions.rds") %>% 
  left_join(readRDS(file = "data/hybas_regions.rds") %>% mutate(HYBAS_ID = as.character(HYBAS_ID)))


#2) plot

hybas_contaminant_predictions %>% 
  group_by(element) %>% 
  sample_n(10000) %>% 
  filter(element == "Zn") %>% 
  ggplot(aes(x = lon, y = lat, color = log10(chem_flux_mg_year + 1))) +
  geom_point(shape = 20, size = 0.01) +
  facet_wrap(~element) +
  scale_color_viridis()
