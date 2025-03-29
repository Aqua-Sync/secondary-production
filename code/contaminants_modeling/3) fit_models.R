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



# PUFA --------------------------------------------------------------------
pufa_data = contaminants %>% filter(chemical_category == "PUFA") %>% 
  filter(adult_units == "mg_g_dm") %>% # original pufa data units are in mg_g_dm. they were convered to ng_mg_dm to be consistent with other contaminants. But the adult_units here refers to the correct "old" units.
  mutate(max_y = max(adult_conc_ng_mg_dm, na.rm = T),
         y_s = adult_conc_ng_mg_dm/max_y) 

saveRDS(pufa_data, file = "data/pufa_data.rds")

pufa_mod = brm(y_s ~ 1 + (1|pub_name),
               data = pufa_data,
               family = Gamma(link = "log"),
               prior = c(prior(normal(-3, 1), class = Intercept),
                         prior(exponential(2), class = sd)))

saveRDS(pufa_mod, file = "models/pufa_mod.rds")

as_draws_df(pufa_mod) %>%
  mutate(pufa = exp(b_Intercept)*unique(pufa_data$max_y)) %>% 
  ggplot(aes(x = pufa)) +
    stat_halfeye() +
  geom_point(data = pufa_data, aes(x = adult_conc_ng_mg_dm),
             y = -0.02,
             shape = "|")











