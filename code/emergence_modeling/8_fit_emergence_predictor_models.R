library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)

emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds')

length(unique(emergence_production_with_vars$author_year))

mean_temp = attributes(emergence_production_with_vars$stream_temp_s)$`scaled:center`
sd_temp = attributes(emergence_production_with_vars$stream_temp_s)$`scaled:scale`

# data_to_predict = readRDS("data/data_to_predict.rds") %>% 
#   mutate(stream_temp = tmp_dc_syr/10,
#          stream_temp_s = (stream_temp - mean_temp)/sd_temp) 
# 
# saveRDS(data_to_predict, file = "data/data_to_predict.rds")

# load prefit models
updated_gams = readRDS("models/updated_gams.rds") # stores all of the individual models below

# fit models ---------------------

# updated_gams_list = list()
# 
# for(i in 1:length(updated_gams)){
#   updated_gams_list[[i]] = update(updated_gams[[i]], 
#                                   newdata = emergence_production_with_vars)
# }
# 
# updated_int_only_gam = update(updated_gams_list[[4]],
#                               prior = c(prior(normal(-2, 1), class = Intercept),
#                                         # prior(normal(0, 2), class = b),
#                                         prior(exponential(6), class = sd),
#                                         prior(exponential(0.01), class = shape)),
#                               control = list(adapt_delta = 0.95))
# 
# updated_gams_list[[4]] <- updated_int_only_gam
# 
# updated_gams = updated_gams_list
# saveRDS(updated_gams, file = "models/updated_gams.rds")

# The individual models below are all stored in a single .rds "models/updated_gams.rds"
# fit full precip model, then use update for subsequent models since the priors remain the same (due to standardized predictors)
fit_gam_precip =  brm(emerge_1 ~ s(precip_s) + (1|author_year) + (1 | HYBAS_ID),
                      family = Gamma(link = "log"),
                      data = emergence_production_with_vars,
                      prior = c(prior(normal(0, 1), class = Intercept),
                                prior(normal(0, 1), class = b),
                                prior(exponential(2), class = sd),
                                prior(lognormal(log(4), 0.5), class = shape)),
                      save_pars = save_pars(all = T),
                      control = list(adapt_delta = 0.85),
                      cores = 4)

fit_gam_temp = update(fit_gam_precip, formula = . ~ s(stream_temp_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
fit_gam_tempprecip = update(fit_gam_precip, formula = . ~ s(precip_s, stream_temp_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
fit_gam_intercept = update(fit_gam_precip, formula = . ~ (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars,
                           prior = c(prior(normal(-5, 2), class = Intercept),
                                     # prior(normal(0, 2), class = b),
                                     prior(exponential(2), class = sd),
                                     prior(exponential(4), class = shape)))
fit_gam_tempaddprecip = update(fit_gam_precip, formula = . ~ s(precip_s) + s(stream_temp_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
fit_gam_footprint_s93 = update(fit_gam_precip, formula = . ~ s(hft_ix_s93_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars,
                               control = list(adapt_delta = 0.95))
fit_gam_footprint_u93 = update(fit_gam_precip, formula = . ~ s(hft_ix_u93_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars,
                               control = list(adapt_delta = 0.95))
fit_gam_footprint_s09 = update(fit_gam_precip, formula = . ~ s(hft_ix_s09_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars,
                               control = list(adapt_delta = 0.95))
fit_gam_footprint_u09 = update(fit_gam_precip, formula = . ~ s(hft_ix_u09_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars,
                               control = list(adapt_delta = 0.95))
fit_gam_elevation = update(fit_gam_precip, formula = . ~ s(ele_mt_sav_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars,
                           control = list(adapt_delta = 0.95))
fit_gam_discharge = update(fit_gam_precip, formula = . ~ s(logdis_m3_pyr_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
fit_gam_forest = update(fit_gam_precip, formula = . ~ s(for_pc_sse_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
fit_gam_cropland = update(fit_gam_precip, formula = . ~ s(crp_pc_sse_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars,
                          control = list(adapt_delta = 0.95))
fit_gam_tempadddischarge = update(fit_gam_precip, formula = . ~ s(stream_temp_s) + s(crp_pc_sse_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
fit_gam_tempadddischargeforest = update(fit_gam_precip, formula = . ~ s(stream_temp_s) + s(crp_pc_sse_s) +
                                         s(for_pc_sse_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)

# put into a list
updated_gams = list(fit_gam_precip,
                    fit_gam_temp,
                    fit_gam_tempprecip,
                    fit_gam_intercept,
                    fit_gam_tempaddprecip,
                    fit_gam_footprint_s93,
                    fit_gam_footprint_u93,
                    fit_gam_footprint_s09,
                    fit_gam_footprint_u09,
                    fit_gam_elevation,
                    fit_gam_discharge,
                    fit_gam_forest,
                    fit_gam_cropland,
                    fit_gam_tempadddischarge,
                    fit_gam_tempadddischargeforest)

saveRDS(updated_gams, file = "models/updated_gams.rds")  



# fit taxa model ---------------------------------------------------------
emergence_production_with_vars_taxa = readRDS(file = 'data/emergence_production_with_vars_taxa.rds')

updated_gams = readRDS("models/updated_gams.rds")

mod_taxa_emerge = update(updated_gams[[3]], 
                         formula = . ~ s(precip_s, stream_temp_s, 
                                         by = taxon_original) + # the new part
                           (1 | HYBAS_ID) + (1|taxon_original),
                         newdata = emergence_production_with_vars_taxa,
                         prior = c(prior(normal(0, 1), class = Intercept),
                                   prior(normal(0, 1), class = b),
                                   prior(exponential(2), class = sd),
                                   prior(lognormal(log(4), 0.5), class = shape)))

saveRDS(mod_taxa_emerge, file = "models/mod_taxa_emerge.rds")



# fit model with prop_diptera as predictor --------------------------------
# load prop taxa, which is the posterior predictions from the model above. then add prop_diptera to the regression models. Does it improve fit?

updated_gams = readRDS("models/updated_gams.rds") # stores all of the individual models below

post_taxa_prop = updated_gams[[3]]$data %>% 
  expand_grid(taxon_original = unique(emergence_production_with_vars_taxa$taxon_original)) %>% 
  mutate(taxon = case_when(taxon_original == "chi_sp" ~ "Diptera",
                           taxon_original == "eph_sp" ~ "Ephemeroptera",
                           taxon_original == "ple_sp" ~ "Plecoptera",
                           taxon_original == "tri_sp" ~ "Trichoptera",
                           T ~ "Other")) %>% 
  add_epred_draws(mod_taxa_emerge, allow_new_levels = T, ndraws = 1000) %>% 
  ungroup %>% group_by(HYBAS_ID, taxon) %>%
  reframe(value = mean(.epred)) %>% 
  group_by(HYBAS_ID) %>% 
  mutate(prop = value/sum(value)) %>% 
  select(-value)

post_taxa_prop_preds = post_taxa_prop  %>% 
  pivot_wider(names_from = taxon, values_from = prop)

dat_with_prop = updated_gams[[3]]$data %>% left_join(post_taxa_prop_preds, relationship = "many-to-many") %>% 
  mutate(ept = Ephemeroptera + Plecoptera + Trichoptera)

mod_diptera_predict = update(updated_gams[[3]], formula = . ~ s(precip_s, stream_temp_s) + (1 | author_year) + (1 | HYBAS_ID) + Diptera,
                             newdata = dat_with_prop)


mod_ept_predict = update(mod_diptera_predict, formula = . ~ s(precip_s, stream_temp_s) + (1 | author_year) + 
                           (1 | HYBAS_ID) + ept,
                             newdata = dat_with_prop)

saveRDS(mod_diptera_predict, file = "models/mod_diptera_predict.rds")
saveRDS(mod_ept_predict, file = "models/mod_ept_predict.rds")

mod_diptera_predict
mod_ept_predict

plot(conditional_effects(mod_diptera_predict), points = T)
plot(conditional_effects(mod_ept_predict), points = T)

updated_gams_new = readRDS("models/updated_gams.rds")
updated_gams_new[[1 + length(updated_gams)]] = mod_diptera_predict
updated_gams_new[[2 + length(updated_gams)]] = mod_ept_predict

model_formulas_list = list()

for(i in 1:length(updated_gams_new)){
  model_formulas_list[[i]] = tibble(formula = deparse(updated_gams_new[[i]]$formula$formula[[3]])) %>% 
    mutate(formula = stringr::str_c(formula, collapse = " ")) %>% 
    pull(formula) %>% 
    str_squish()
  
  model_formulas_list[[i]] = model_formulas_list[[i]][1] # remove duplicates
}

model_list = bind_rows(as_tibble(unlist(model_formulas_list)))


# get_mod_names = function(model){as.character(model$formula$formula[[3]][2])}

ic_gams = lapply(updated_gams_new, FUN = brms::loo) 

# names(ic_gams) = mod_names

elpd_diffs = loo_compare(ic_gams) %>% 
  as_tibble() %>% 
  mutate(lower = elpd_diff - 2*se_diff,
         upper = elpd_diff + 2*se_diff)

