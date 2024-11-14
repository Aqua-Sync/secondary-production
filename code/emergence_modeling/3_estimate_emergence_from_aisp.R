library(brms)
library(tidyverse)
library(janitor)

# Estimate emergence from secondary production using e:p ratios
# 1) load data
secondary_prod_sd = readRDS(file = "data/secondary_prod_sd.rds") %>% 
  mutate(lat = parse_number(as.character(lat)),
         lon = parse_number(as.character(lon)))

# 2) fit model from Gratton data
# prior for E:P from Gratton et al. 
gratton_ep = read_csv("data/gratton_supplement_C.csv") %>% clean_names() %>% 
  filter(type == "Streams")

# ep_model = brm(e_p_ratio ~ 1 + (1|reference) + (1|taxa_measured),
#                family = Beta(link = "logit"),
#                data = gratton_ep)
# 
# saveRDS(ep_model, file = "models/ep_model.rds")
ep_model = readRDS("models/ep_model.rds")

# 3) get posteriors 
ep_posts = ep_model %>% 
  as_draws_df() %>% 
  mutate(ep_posts = inv_logit_scaled(b_Intercept)) 

# 4) estimate emergence production as a proportion of insect secondary production
emergence = secondary_prod_sd %>% 
  expand_grid(ep = ep_posts %>% slice(1:1000) %>% select(ep_posts) %>% pull) %>% 
  select(acsp, aisp_original, aisp, aisp_sd, ep, everything()) %>% 
  mutate(emergence = aisp*ep,
         emergence_kg = emergence/1e6) %>% 
  group_by(id) %>% 
  reframe(mean_emergence_mgdmm2y = mean(emergence),
          sd_emergence = sd(emergence),
          mean_emergence_kgdmm2y = mean(emergence_kg),
          sd_emergence_kg = sd(emergence_kg))

emergence_production = secondary_prod_sd %>% 
  left_join(emergence)

write_csv(emergence_production, file = "data/emergence_production.csv")

# 5) plot
emergence_production %>% 
  ggplot(aes(y = mean_emergence_kgdmm2y, x = index)) + 
  geom_point() +
  geom_linerange(aes(ymin = mean_emergence_kgdmm2y - sd_emergence_kg,
                     ymax = mean_emergence_kgdmm2y + sd_emergence_kg))
