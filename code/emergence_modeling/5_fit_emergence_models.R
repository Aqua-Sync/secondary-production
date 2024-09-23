library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)

# function to estimate stream temperatures from air temperature
# from eq. 2 in Mohseni, O., Erickson, T. R., & Stefan, H. G. (1997). A Non-Linear Regression Model for Weekly Stream Temperatures at 585 Gaging Stations in the US.
estimate_streamtemp = function(x){
  numerator = 26.2                           # values are median parameter values from Mohseni et al. Tbl X.
  denominator = 1 + (exp(0.18*(13.3 - x)))
  0.8 + (numerator/denominator)
}

# load data ---------------------
hydroatlas_temps_precip = read_csv("data/Production_wHydrobasinID_06262024.csv") %>% 
  mutate(tmp_dc_syr = tmp_dc_syr/10) # put temps in dec C instead of 10*dec C

hydrobasin_vars = read_csv("data/Production_wHydrobasinID_06262024_ALL.csv") %>% 
  mutate(tmp_dc_syr10 = tmp_dc_syr/10, # put temps in dec C instead of 10*dec C
         pre_cm_syr1000 = pre_mm_syr/1000,
         precip_s = scale(pre_mm_syr),
         ele_mt_sav_s = scale(ele_mt_sav),
         logdis_m3_pyr_s = scale(log(dis_m3_pyr)),
         for_pc_sse_s = scale(for_pc_sse),
         crp_pc_sse_s = scale(crp_pc_sse),
         hft_ix_s93_s = scale(hft_ix_s93),  # "source: https://developers.google.com/earth-engine/datasets/catalog/WWF_HydroATLAS_v1_Basins_level12#table-schema"
         hft_ix_u93_s = scale(hft_ix_u93),
         hft_ix_s09_s = scale(hft_ix_s09),  # "source: https://developers.google.com/earth-engine/datasets/catalog/WWF_HydroATLAS_v1_Basins_level12#table-schema"
         hft_ix_u09_s = scale(hft_ix_u09))  # "hft_ix_xxx are the human footprint index. s93 is the 'spatial extent at subbasin pour point in 1993 or s09 = 2009. u93 is total watershed upstream of point. Try both."

emergence_production = read_csv(file = "data/emergence_production.csv") %>% 
  separate(site_id, into = c("author", "author2"), extra = "merge") %>% 
  mutate(year = parse_number(author2)) %>% 
  mutate(author_year = paste(author, year, sep = "_"))

emergence_production_with_vars = emergence_production %>% 
  left_join(hydrobasin_vars, by = "id") %>% 
  mutate(emerge_1 = mean_emergence_mgdmm2y/max(mean_emergence_mgdmm2y, na.rm = T),
         stream_temp = estimate_streamtemp(tmp_dc_syr10),
         stream_temp_s = scale(stream_temp),
         stream_temp20 = stream_temp/20) %>%  # reduce range of stream temps to improve model fitting
  mutate(HYBAS_ID = as.character(HYBAS_ID),
         BAS_ID = as.character(BAS_ID)) %>%  # for updating so that the names in models don't change 
  mutate(human_impact = case_when(glc_cl_smj %in% c(22, 16) ~ "impacted",
                                  TRUE ~ "notimpacted"))

# load prefit models
updated_gams = readRDS("models/updated_gams.rds") # stores all of the individual models below

# fit models ---------------------

# The individudal models below are all stored in a single .rds "models/updated_gams.rds"
# fit full precip model, then use update for subsequent models since the priors remain the same (due to standardized predictors)
# fit_gam_precip = brm(emerge_1 ~ s(precip_s) + (1 | author_year) + (1 | HYBAS_ID),
#                      family = Gamma(link = "log"),
#                      data = emergence_production_with_vars,
#                      prior = c(prior(normal(-5, 2), class = Intercept),
#                                prior(normal(0, 2), class = b),
#                                prior(exponential(2), class = sd),
#                                prior(exponential(4), class = shape)), 
#                      save_pars = save_pars(all = T))
# 
# fit_gam_temp = update(fit_gam_precip, formula = . ~ s(stream_temp_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
# fit_gam_tempprecip = update(fit_gam_precip, formula = . ~ s(precip_s, stream_temp_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
# fit_gam_intercept = update(fit_gam_precip, formula = . ~ (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars,
#                            prior = c(prior(normal(-5, 2), class = Intercept),
#                                      # prior(normal(0, 2), class = b),
#                                      prior(exponential(2), class = sd),
#                                      prior(exponential(4), class = shape)))
# fit_gam_tempaddprecip = update(fit_gam_precip, formula = . ~ s(precip_s) + s(stream_temp_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
# fit_gam_footprint_s93 = update(fit_gam_precip, formula = . ~ s(hft_ix_s93_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
# fit_gam_footprint_u93 = update(fit_gam_precip, formula = . ~ s(hft_ix_u93_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
# fit_gam_footprint_s09 = update(fit_gam_precip, formula = . ~ s(hft_ix_s09_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
# fit_gam_footprint_u09 = update(fit_gam_precip, formula = . ~ s(hft_ix_u09_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
# fit_gam_footprint_u09 = update(fit_gam_precip, formula = . ~ s(hft_ix_u09_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
# fit_gam_elevation = update(fit_gam_precip, formula = . ~ s(ele_mt_sav_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
# fit_gam_discharge = update(fit_gam_precip, formula = . ~ s(logdis_m3_pyr_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
# fit_gam_forest = update(fit_gam_precip, formula = . ~ s(for_pc_sse_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
# fit_gam_cropland = update(fit_gam_precip, formula = . ~ s(crp_pc_sse_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
# fit_gam_tempadddischarge = update(fit_gam_precip, formula = . ~ s(stream_temp_s) + s(crp_pc_sse_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)
# fit_gam_tempadddischargeforest = update(fit_gam_precip, formula = . ~ s(stream_temp_s) + s(crp_pc_sse_s) +
#                                          s(for_pc_sse_s) + (1 | author_year) + (1 | HYBAS_ID), newdata = emergence_production_with_vars)

# put into a list
# updated_gams = list(fit_gam_precip,
#                     fit_gam_temp,
#                     fit_gam_tempprecip,
#                     fit_gam_intercept,
#                     fit_gam_tempaddprecip,
#                     fit_gam_footprint_s93,
#                     fit_gam_footprint_u93,
#                     fit_gam_footprint_s09,
#                     fit_gam_footprint_u09,
#                     fit_gam_elevation,
#                     fit_gam_discharge,
#                     fit_gam_forest,
#                     fit_gam_cropland,
#                     fit_gam_tempadddischarge,
#                     fit_gam_tempadddischargeforest)

# saveRDS(updated_gams, file = "models/updated_gams.rds")  # 


# compare models  ---------------------
get_mod_names = function(model){as.character(model$formula$formula[[3]][2])}

mod_names = lapply(updated_gams, FUN = get_mod_names) %>% unlist()

ic_gams = lapply(updated_gams, FUN = brms::loo) 

names(ic_gams) = mod_names

elpd_diffs = loo_compare(ic_gams) %>% 
  as_tibble() %>% 
  mutate(lower = elpd_diff - 2*se_diff,
         upper = elpd_diff + 2*se_diff) %>% 
  mutate(models = mod_names)

model_comparison = elpd_diffs  %>% 
  ggplot(aes(x = reorder(models, elpd_diff),
             y = elpd_diff,
             ymin = lower, 
             ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept = 0) +
  coord_flip()

ggsave(model_comparison, file = "plots/model_comparison.jpg", width = 8, height = 8)

elpd_diffs  %>% 
  glimpse() %>% 
  ggplot(aes(x = reorder(models, elpd_diff),
             y = looic,
             ymin = looic - se_looic, 
             ymax = looic + se_looic)) +
  geom_pointrange() +
  # geom_hline(yintercept = 0) +
  coord_flip()

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

# plot models ------------------
gams_with_effects = updated_gams[-4]

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

