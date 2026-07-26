library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)


# contaminants ------------------------------------------------------------

# load data
contaminants = readRDS(file = "data/contaminants.rds")

## dummy model to update
dat_dummy = contaminants %>%
  filter(chemical_category == "fungicide") %>% 
  mutate(max_y = max(adult_conc_ng_mg_dm,na.rm = T),
         y_s = adult_conc_ng_mg_dm/max_y,
         log_water_conc_ugl_01 = log(water_conc_ug_l + 0.001*mean(water_conc_ug_l, na.rm = T)),
         x_s = scale(log_water_conc_ugl_01))

brm_contaminant_prior = brm(y_s ~ x_s + (1 | pub_name),
               prior = c(prior(normal(0, 1), class = "b"),
                         prior(normal(-3, 1), class = "Intercept")),
               data = dat_dummy,
               chains = 1, iter = 100, sample_prior = "only") # no need for full sample. Will refit in the update() below with 4 chains and 2000 iterations.

saveRDS(brm_contaminant_prior, file = "models/brm_contaminant_prior.rds")

conditional_effects(brm_contaminant_prior)
conditional_effects(brm_contaminant_prior, method = "predict")
