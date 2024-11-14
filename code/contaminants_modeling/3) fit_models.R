library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)

# load data
contaminants = readRDS(file = "data/contaminants.rds")

# standardize data
cont_split = contaminants %>% 
  filter(chemical_category %in% c("Se", "Pb", "Zn", "Hg", "Cu", "Cd",
                                  "insecticide", "fungicide", "herbicide", 
                                  "ECD", "pharmaceuticals")) %>% 
  group_by(chemical_category) %>% 
  group_split()
  
mod_list = list()

for(i in 1:length(cont_split)){
  dat = cont_split[[i]] %>% 
  mutate(max_y = max(adult_conc_ng_mg_dm,na.rm = T),
       y_s = adult_conc_ng_mg_dm/max_y,
       log_water_conc_ugl_01 = log(water_conc_ug_l + 0.001*mean(water_conc_ug_l, na.rm = T)),
       x_s = scale(log_water_conc_ugl_01))
  
  mod_list[[i]] = update(readRDS(file = "models/brm_fung.rds"),
       newdata = dat,
       data2 = list(chemical_category = unique(dat$chemical_category),
                    mean_x = attributes(dat$x_s)[2],
                    sd_x = attributes(dat$x_s)[3],
                    max_y = unique(dat$max_y)))
}

saveRDS(mod_list, file = "models/mod_list.rds")


