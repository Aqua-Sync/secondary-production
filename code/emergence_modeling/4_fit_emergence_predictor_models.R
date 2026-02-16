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

updated_gams_list = list()

for(i in 1:length(updated_gams)){
  updated_gams_list[[i]] = update(updated_gams[[i]], 
                                  newdata = emergence_production_with_vars)
}

updated_int_only_gam = update(updated_gams_list[[4]],
                              prior = c(prior(normal(-2, 1), class = Intercept),
                                        # prior(normal(0, 2), class = b),
                                        prior(exponential(6), class = sd),
                                        prior(exponential(0.01), class = shape)),
                              control = list(adapt_delta = 0.95))

updated_gams_list[[4]] <- updated_int_only_gam

updated_gams = updated_gams_list
saveRDS(updated_gams, file = "models/updated_gams.rds")

# The individual models below are all stored in a single .rds "models/updated_gams.rds"
# fit full precip model, then use update for subsequent models since the priors remain the same (due to standardized predictors)
fit_gam_precip = brm(emerge_1 ~ s(precip_s) + (1 | author_year) + (1 | HYBAS_ID),
                     family = Gamma(link = "log"),
                     data = emergence_production_with_vars,
                     prior = c(prior(normal(-5, 2), class = Intercept),
                               prior(normal(0, 2), class = b),
                               prior(exponential(4), class = sd),
                               prior(exponential(0.01), class = shape)),
                     save_pars = save_pars(all = T))

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


# make model table --------------------------------------------------------

model_formulas_list = list()

for(i in 1:length(updated_gams)){
  model_formulas_list[[i]] = tibble(formula = deparse(updated_gams[[i]]$formula$formula[[3]])) %>% 
    mutate(formula = stringr::str_c(formula, collapse = " ")) %>% 
    pull(formula) %>% 
    str_squish()
  
  model_formulas_list[[i]] = model_formulas_list[[i]][1] # remove duplicates
}

model_list = bind_rows(as_tibble(unlist(model_formulas_list)))
write_csv(model_list, file = "tables/model_list.csv")


# compare models  ---------------------
# get_mod_names = function(model){as.character(model$formula$formula[[3]][2])}

mod_names = read_csv("tables/model_list.csv") %>% pull(value)

ic_gams = lapply(updated_gams, FUN = brms::loo) 

names(ic_gams) = mod_names

elpd_diffs = loo_compare(ic_gams) %>% 
  as_tibble() %>% 
  mutate(lower = elpd_diff - 2*se_diff,
         upper = elpd_diff + 2*se_diff) %>% 
  mutate(models = mod_names)

write_csv(elpd_diffs, file = "tables/model_selection.csv")

elpd_diffs = read_csv("tables/model_selection.csv")

model_comparison = elpd_diffs  %>% 
  ggplot(aes(x = reorder(models, elpd_diff),
             y = elpd_diff,
             ymin = elpd_diff - se_diff, 
             ymax = elpd_diff + se_diff)) +
  geom_pointrange() +
  geom_hline(yintercept = 0) +
  coord_flip()

ggsave(model_comparison, file = "plots/model_comparison.jpg", width = 8, height = 8)


# check divergences ---------------------

divergence_list = NULL

for(i in 1:length(updated_gams)){
  np = nuts_params(updated_gams[[i]])
  divergence_list[[i]] = tibble(divergences = sum(subset(np, Parameter == "divergent__")$Value),
                                model = mod_names[[i]])
}

bind_rows(divergence_list)

# check rhats -------------------------

rhat_list = NULL

for(i in 1:length(updated_gams)){
  rhat_list[[i]] = brms::rhat(updated_gams[[i]]) %>% as.list() %>% 
    as_tibble() %>% pivot_longer(cols = everything()) %>% 
    mutate(model = mod_names[i])
}

bind_rows(rhat_list) %>% 
  ggplot(aes(y = model, x = value)) + 
  geom_point() +
  geom_vline(xintercept = c(1.01, 1.1))

bind_rows(rhat_list) %>% 
  # filter(!grepl("r_", name)) %>%
  # filter(!grepl("s_", name)) %>%
  # filter(!grepl("z_", name)) %>%
  filter(!grepl("lp", name)) %>% 
  mutate(parameter = str_sub(name, 1, 2)) %>% 
  ggplot(aes(y = model, x = value, color = parameter)) + 
  geom_point() +
  geom_vline(xintercept = c(1.01, 1.1))

# bind_rows(rhat_list) %>% 
#   filter(!grepl("r_", name)) %>% 
#   filter(!grepl("s_", name)) %>% 
#   filter(!grepl("z_", name)) %>% 
#   filter(!grepl("lp", name)) %>% View()

# plot models ------------------
gams_with_effects = updated_gams[-4] # removes intercept_only model

get_cond_plots = function(model){plot(conditional_effects(model), points = T, plot = F, ask = F)}

cond_plots = lapply(gams_with_effects, FUN = get_cond_plots)

library(cowplot)

cond_plots_hydrobasin = plot_grid(cond_plots[[1]]$precip_s + scale_y_log10(),
                                  cond_plots[[2]]$stream_temp_s + scale_y_log10(),
                                  cond_plots[[3]]$`precip_s:stream_temp_s`+ scale_y_log10() + guides(fill = "none", color = "none"),
                                  cond_plots[[4]]$precip_s + scale_y_log10(),
                                  cond_plots[[4]]$stream_temp_s + scale_y_log10(),
                                  cond_plots[[5]]$hft_ix_s93_s + scale_y_log10() ,
                                  cond_plots[[6]]$hft_ix_u93_s + scale_y_log10(),
                                  cond_plots[[7]]$hft_ix_s09_s + scale_y_log10(),
                                  cond_plots[[8]]$hft_ix_u09_s + scale_y_log10(),
                                  cond_plots[[9]]$ele_mt_sav_s + scale_y_log10(),
                                  cond_plots[[10]]$logdis_m3_pyr_s + scale_y_log10(),
                                  cond_plots[[11]]$for_pc_sse_s + scale_y_log10(),
                                  cond_plots[[12]]$crp_pc_sse_s + scale_y_log10(),
                                  cond_plots[[13]]$stream_temp_s + scale_y_log10(),
                                  cond_plots[[13]]$crp_pc_sse_s + scale_y_log10(),
                                  cond_plots[[14]]$stream_temp_s + scale_y_log10(),
                                  cond_plots[[14]]$crp_pc_sse_s + scale_y_log10(),
                                  cond_plots[[14]]$for_pc_sse_s + scale_y_log10())

ggsave(cond_plots_hydrobasin, file = "plots/cond_plots_hydrobasin.jpg", 
       width = 10, height = 10, dpi = 400)

