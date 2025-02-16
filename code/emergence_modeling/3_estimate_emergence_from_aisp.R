library(brms)
library(tidyverse)
library(janitor)

# Estimate emergence from secondary production using e:p ratios
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
#                prior = c(prior(exponential(2), class = "sd")))
# 
# saveRDS(ep_model, file = "models/ep_model.rds")

ep_model = readRDS("models/ep_model.rds")

# ep_model = update(ep_model, newdata = gratton_ep)

# 3) get posteriors 
ep_posts = ep_model %>% 
  as_draws_df() %>% 
  mutate(ep_posts = inv_logit_scaled(b_Intercept)) 

# 4) estimate emergence production as a proportion of insect secondary production
emergence = secondary_prod_sd %>% 
  expand_grid(ep = ep_posts %>% slice(1:1000) %>% select(ep_posts) %>% pull) %>% 
  select(acsp, aisp_original, aisp, aisp_sd, ep, everything()) %>% 
  mutate(emergence = aisp*ep) %>% 
  group_by(id) %>% 
  mutate(emergence = case_when(is.na(emerg) ~ emergence,  # add empirical measures
                               TRUE ~ emerg),
         emergence_kg = emergence/1e6) %>% 
  reframe(mean_emergence_mgdmm2y = mean(emergence),
          sd_emergence = sd(emergence),
          mean_emergence_kgdmm2y = mean(emergence_kg),
          sd_emergence_kg = sd(emergence_kg)) 

emergence_production = secondary_prod_sd %>% 
  left_join(emergence) %>% 
  mutate(empirical_emergence = case_when(is.na(emerg) ~ "no", 
                                         TRUE ~ "yes")) %>% 
  filter(!grepl("Moio_2017", site_id)) # delete b/c it was estimated from just a few days of the year

write_csv(emergence_production, file = "data/emergence_production.csv")

# 5) plot
emergence_production %>% 
  ggplot(aes(y = mean_emergence_mgdmm2y, x = id, color = empirical_emergence)) + 
  geom_point() +
  geom_linerange(aes(ymin = mean_emergence_kgdmm2y - sd_emergence_kg,
                     ymax = mean_emergence_kgdmm2y + sd_emergence_kg)) +
  # scale_y_log10() +
  NULL
