library(brms)
library(tidyverse)
library(janitor)
library(ggthemes)
library(tidybayes)

# Convert insect secondary production to emergence production using e:p ratios
# ~2 seconds to 5 minutes (depending on need to compile models)

# 1) load data
secondary_prod_sd = readRDS(file = "data/secondary_prod_sd.rds") %>% 
  mutate(lat = parse_number(as.character(lat)),
         lon = parse_number(as.character(lon)))

# 2) fit model from Gratton data
# prior for E:P from Gratton et al. and Raitif et al.  
gratton_ep = read_csv("data/e_p_ratios.csv") %>% clean_names() %>% 
  filter(type == "Streams")

# ep_model = brm(e_p_ratio ~ 1 + (1|reference) + (1|taxa_measured),
#                family = Beta(link = "logit"),
#                data = gratton_ep,
#                prior = c(prior(exponential(2), class = "sd"),
#                          prior(normal(-1.45, 0.5), class = "Intercept")))
# 
# saveRDS(ep_model, file = "models/ep_model.rds")

ep_model = readRDS("models/ep_model.rds")

# ep_model = update(ep_model, newdata = gratton_ep)

# 3) get posteriors 
ep_posts = predicted_draws(
  ep_model,
  newdata = tibble(reference = "new", taxa_measured = "new"),
  allow_new_levels = TRUE,
  ndraws = 1000) %>% 
  ungroup()

rm(ep_model)

# 4) estimate emergence production as a proportion of insect secondary production
# throws a warning in the shape/scale b/c some observations only have direct emergence measured, not aisp. not a problem.
# This produces sd for emergence for most measures, but not for direct emergence measures. That will be added next.

# get posterior draws of emergence that was converted from acsp (add direct emergence draws a few steps later)
posts_secondary = secondary_prod_sd %>% 
  ungroup %>% 
  cross_join(ep_posts %>% slice(1:1000) %>% ungroup %>% select(.prediction, .draw) %>% rename(ep = .prediction)) %>% 
  select(acsp, aisp, aisp_sd, ep, everything()) %>% 
  mutate(shape = aisp^2/aisp_sd^2,
         scale = aisp_sd^2/aisp) %>% 
  mutate(aisp_draw = rgamma(n(), shape = shape, scale = scale)) %>% 
  mutate(emergence = aisp_draw*ep) 

# wrangle secondary production into means and sds
secondary_prod_sd_wrangled = posts_secondary %>% 
  group_by(id) %>% 
  mutate(emergence = case_when(is.na(emerg) ~ emergence,  # add empirical measures
                               TRUE ~ emerg),
         emergence_kg = emergence/1e6) %>% 
  reframe(mean_emergence_mgdmm2y = mean(emergence),
          sd_emergence = sd(emergence),
          mean_emergence_kgdmm2y = mean(emergence_kg),
          sd_emergence_kg = sd(emergence_kg)) %>% 
  mutate(obs_id = as.character(id)) %>% 
  mutate(log_mean_emergence_mgdmm2y = log10(mean_emergence_mgdmm2y),
         log_mean_emergence_mgdmm2y_s = scale(log_mean_emergence_mgdmm2y))

emergence_temp =  secondary_prod_sd_wrangled %>% 
  filter(sd_emergence > 0) %>% # removes data that were directly measured as emergence
  mutate(log_sd_emergence = log10(sd_emergence),
         log_sd_emergence_s = scale(log_sd_emergence),
         sd_emergence_01 = sd_emergence/max(sd_emergence, na.rm = T)) 


# 5) estimate sd for observations that were directly measured emer --------

# model relationship between mean and sd emergence for data that were NOT directly measured as emergence
# brm_meansd_emerge = brm(sd_emergence_01 ~ log_mean_emergence_mgdmm2y_s + (1|obs_id),
#                         family = Gamma(link = "log"),
#                         prior = c(prior(exponential(2), class = "sd"),
#                                   prior(normal(0, 0.5), class = "Intercept"),
#                                   prior(normal(0, 0.1), class = "b"),
#                                   prior(gamma(2, 0.1), class = "shape")),
#                         data = emergence_temp,
#                         control = list(adapt_delta = 0.9))
# #
# saveRDS(brm_meansd_emerge, file = "models/brm_meansd_emerge.rds")

brm_meansd_emerg = readRDS(file = "models/brm_meansd_emerge.rds")

# get posterior standard deviation of emergence
post_meansd_emerg = secondary_prod_sd_wrangled %>% filter(sd_emergence == 0) %>% 
  add_epred_draws(brm_meansd_emerg, re_formula = NULL, allow_new_levels = T) %>% 
  mutate(.epred = .epred*max(emergence_temp$sd_emergence)) %>% 
  ungroup() %>% 
  group_by(obs_id) %>% 
  reframe(median_sd = median(.epred))

# this data set is used to estimate model selection
emergence_production = secondary_prod_sd_wrangled %>% 
  left_join(post_meansd_emerg) %>%
  select(id, mean_emergence_mgdmm2y, sd_emergence, median_sd, everything()) %>%
  mutate(empirical_emergence = case_when(sd_emergence > 0 ~ "no", 
                                         TRUE ~ "yes")) %>% 
  mutate(sd_emergence = case_when(sd_emergence == 0 ~ median_sd, 
                                  T ~ sd_emergence)) %>% # replace missing sds with modeled sds
  left_join(secondary_prod_sd %>% select(-obs_id) , by = "id") %>% # add predictors back and add aisp, aicp, etc.
  select(-median_sd)

saveRDS(emergence_production, file = "data/emergence_production.rds")


# get posterior draws for brm_multiple() model ----------------------------
# this is the data we'll use for the final model. It has posterior draws of emergence,
# we will use the result to fit ~100s of versions of the final model. Each version is fit to 
# a different draw from the posterior (i.e., draw_1 = 299 observations_k1, draw_2 = 299 observations_k2, ... draw_n = 299 observations_kn)
emergence_production = readRDS(file = "data/emergence_production.rds")

post_empirical_emergence = emergence_production %>% 
  filter(empirical_emergence == "yes") %>% 
  mutate(shape = mean_emergence_mgdmm2y^2 / sd_emergence^2,
         rate = mean_emergence_mgdmm2y/sd_emergence^2) %>% 
  expand_grid(.draw = 1:1000) %>% 
  mutate(raw_mean_emergence_mgdmm2y = mean_emergence_mgdmm2y) %>%
  mutate(mean_emergence_mgdmm2y = rgamma(n(), shape = shape, rate = rate),
         HYBAS_ID = as.character(HYBAS_ID)) %>%
  # select(mean_emergence_mgdmm2y, id) %>% 
  mutate(empirical_emergence = "yes") %>% 
  select(-obs_id)

# check that it worked
post_empirical_emergence %>% 
  ggplot(aes(x = raw_mean_emergence_mgdmm2y, y = mean_emergence_mgdmm2y)) +
  geom_point(shape = ".") +
  geom_abline() +
  scale_x_log10() +
  scale_y_log10()

posts_emergence = posts_secondary %>%
  filter(is.na(emerg)) %>% 
  select(-obs_id) %>%
  rename(mean_emergence_mgdmm2y = emergence) %>% 
  mutate(empirical_emergence = "no") %>% 
  bind_rows(post_empirical_emergence) %>% 
  ungroup %>% 
  mutate(emerge_mean_centered = mean_emergence_mgdmm2y/mean(mean_emergence_mgdmm2y, na.rm = T))

saveRDS(posts_emergence, file = "posteriors/posts_emergence.rds")

# check
posts_emergence %>% 
  ggplot(aes(x = stream_temp_s, y = emerge_mean_centered)) + 
  stat_pointinterval(alpha = 0.2)


# compare old vs new, where new is the revised version after adding sd_emergence
compare_emerge = emergence_production %>% left_join(readRDS("data/emergence_production_with_vars.rds") %>% ungroup %>% select(id, mean_emergence_mgdmm2y) %>% 
                                     rename(old_emergence = mean_emergence_mgdmm2y))

compare_emerge_plot = compare_emerge %>% 
  ggplot(aes(x = old_emergence, y = mean_emergence_mgdmm2y)) +
  geom_pointinterval(aes(ymax = mean_emergence_mgdmm2y + sd_emergence,
                         ymin = mean_emergence_mgdmm2y - sd_emergence)) +
  geom_abline() +
  scale_y_log10() +
  scale_x_log10() +
  labs(caption = "The outlier on the left is OK. It was wrong in\nthe old_emergence and was corrected in the new.")

saveRDS(compare_emerge_plot, file = "plots/compare_emerge_plot.rds")

# 5) plot

emergence_production = read_csv(file = "data/emergence_production.csv") %>% 
  mutate(source = case_when(empirical_emergence == "no" ~ "Converted from ACSP",
                            TRUE ~ "Directly Measured")) %>% 
  arrange(mean_emergence_mgdmm2y) 


# make three data source: all data, converted only, and emergence only. then plot to show comparison
# this is to address a reviewer comment that a single category on the x-axis is confusing (which we agree)

emer = bind_rows(emergence_production %>% mutate(grouping = "All Data"),
                 emergence_production %>% filter(source == "Directly Measured") %>% 
                   mutate(grouping = "Directly Measured"),
                 emergence_production %>% filter(source != "Directly Measured") %>% 
                   mutate(grouping = "Converted from ACSP")) %>% 
  mutate(jitter_offset = rnorm(nrow(.), 0, 0.1))



emergence_compared_raw_acsp_revised = emer %>% 
  ggplot(aes(x = grouping, y = mean_emergence_mgdmm2y)) +
  geom_jitter(width = 0.05, height = 0, aes(color = source,
                               alpha = source)) +
  ggthemes::scale_color_colorblind() +
  theme_default() +
  labs(y = expression("Annual Insect Emergence Production (mgDM m"^-2*" yr"^-1*")"),
       x = "") +
  scale_alpha_manual(values = c(0.2, 0.8)) +
  theme(legend.text = element_text(size = 8),
        legend.position = "top",
        legend.title = element_blank()) +
  NULL

ggsave(emergence_compared_raw_acsp_revised , file = "plots/emergence_compared_raw_acsp_revised .jpg",
       width = 6, height = 5, dpi = 400)

emergence_compared_raw_acsp = emergence_production %>% 
  ggplot(aes(y = mean_emergence_mgdmm2y,
             color = source,
             alpha = source,
             x = NA)) + 
  geom_jitter(width = 0.2) +
  # scale_y_log10() +
  theme_default() +
  labs(y = expression("Annual Emergence Production (mg m"^-2*" yr"^-1*" dry mass)"),
       x = "All Emergence Estimates",
       color = "Method",
       alpha = "Method") +
  theme(legend.text = element_text(size = 8),
        axis.text.x = element_blank()) +
  ggthemes::scale_color_colorblind() + 
  NULL

ggsave(emergence_compared_raw_acsp, file = "plots/emergence_compared_raw_acsp.jpg",
       width = 5, height = 5, dpi = 400)



