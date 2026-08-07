library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)


# contaminants ------------------------------------------------------------

# load data
contaminants = readRDS(file = "data/contaminants.rds")

# standardize data and split by contaminant
cont_split = contaminants %>% 
  filter(chemical_category %in% c("Se", "Pb", "Zn", "Hg", "Cu", "Cd",
                                  "insecticide", "fungicide", "herbicide", 
                                  "ECD", "pharmaceuticals")) %>% 
  group_by(chemical_category) %>% 
  group_split()
  
# fit models to each contaminant (un-silence to run)

## dummy model to update

# dat_dummy = contaminants %>%
#   filter(chemical_category == "fungicide") %>% 
#   mutate(max_y = max(adult_conc_ng_mg_dm,na.rm = T),
#        y_s = adult_conc_ng_mg_dm/max_y,
#        log_water_conc_ugl_01 = log(water_conc_ug_l + 0.001*mean(water_conc_ug_l, na.rm = T)),
#        x_s = scale(log_water_conc_ugl_01))

# brm_dummy = brm(y_s ~ x_s + (1 | pub_name),
#                prior = c(prior(normal(0, 1), class = "b"),
#                          prior(normal(-3, 1), class = "Intercept")),
#                data = dat_dummy,
#                chains = 1, iter = 100) # no need for full sample. Will refit in the update() below with 4 chains and 2000 iterations.
# 
# saveRDS(brm_dummy, file = "models/brm_dummy.rds")

# mod_list = list()
# 
# for(i in 1:length(cont_split)){
  # dat = cont_split[[i]] %>%
  # mutate(max_y = max(adult_conc_ng_mg_dm,na.rm = T),
  #      y_s = adult_conc_ng_mg_dm/max_y,
  #      log_water_conc_ugl_01 = log(water_conc_ug_l + 0.001*mean(water_conc_ug_l, na.rm = T)),
  #      x_s = scale(log_water_conc_ugl_01))
#   
#   mod_list[[i]] = update(readRDS(file = "models/brm_dummy.rds"),
#        newdata = dat,
#        data2 = list(chemical_category = unique(dat$chemical_category),
#                     mean_x = attributes(dat$x_s)[2],
#                     sd_x = attributes(dat$x_s)[3],
#                     max_y = unique(dat$max_y)))
# }
# 
# saveRDS(mod_list, file = "models/mod_list.rds")

mod_list = readRDS(file = "models/mod_list.rds")

# number of records we modeled

model_data_all = mod_list %>% 
  map("data") %>% 
  bind_rows(.id = "model_id")

# number of distinct data points total
nrow(distinct(model_data_all))

# number of distinct data points per model
model_data_all %>% 
  group_by(model_id) %>% 
  tally()

# number of distinct publications
length(unique(model_data_all$pub_name))

# PUFA --------------------------------------------------------------------
emergence_production_with_vars = readRDS("data/emergence_production_with_vars.rds")
pufa_data_short = readRDS("data/pufa_data.rds") %>% 
  filter(chemical == "epa + dha") %>%
  filter(!is.na(stream_temp)) %>% 
  mutate(HYBAS_ID = as.character(HYBAS_ID)) %>% 
  mutate(stream_temp_s = (stream_temp - attributes(emergence_production_with_vars$stream_temp_s)[[2]])/attributes(emergence_production_with_vars$stream_temp_s)[[3]],
         log10_stream_temp = log10(stream_temp),
         log10_stream_temp_s = scale(log10_stream_temp))

length(unique(pufa_data_short$pub_number))

# pufa_mod = brm(y_s ~ 1 + (1|pub_name),
#                data = pufa_data_short,
#                family = Gamma(link = "log"),
#                prior = c(prior(normal(-3, 1), class = Intercept),
#                          prior(exponential(2), class = sd)))

# saveRDS(pufa_mod, file = "models/pufa_mod.rds")

pufa_mod = readRDS(file = "models/pufa_mod.rds")

pufa_mod = update(pufa_mod, newdata = pufa_data_short, formula = . ~ 1 + (1|pub_name) + (1|order) + (1|HYBAS_ID))

saveRDS(pufa_mod, file = "models/pufa_mod.rds")



# re-run with just epa + dha data and taxon as varying intercept

pufa_mod_taxon_epadha = update(pufa_mod, newdata = pufa_data_short, 
                        formula = . ~ (1|pub_name) + (1|order) + (1|HYBAS_ID))

saveRDS(pufa_mod_taxon_epadha, file = "models/pufa_mod_taxon_epadha.rds")

# re-run with temp as a predictor
pufa_mod_taxon_epadha_temp = update(readRDS(file = "models/pufa_mod_taxon_epadha_temp.rds"), newdata = pufa_data_short , 
                               formula = . ~ s(stream_temp_s) + 
                                 (1|pub_name) + (1|order) + (1|HYBAS_ID))

saveRDS(pufa_mod_taxon_epadha_temp, file = "models/pufa_mod_taxon_epadha_temp.rds")

# re-run with log10temp as a predictor
pufa_mod_taxon_epadha_logtemp = update(readRDS(file = "models/pufa_mod_taxon_epadha_temp.rds"), newdata = pufa_data_short, 
                                    formula = . ~ s(log10_stream_temp_s) + 
                                      (1|pub_name) + (1|order) + (1|HYBAS_ID))

saveRDS(pufa_mod_taxon_epadha_logtemp, file = "models/pufa_mod_taxon_epadha_logtemp.rds")

