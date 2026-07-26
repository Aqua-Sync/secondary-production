# response to reviewer concern that the e:p fluxes affect global export and may contribute a lot of uncertainty to the flux estimates.
# this script re-calculates emergence using a fixed value of e:p. Then we can compare the uncertainty in global flux to the original and 
# describe how the e:p uncertainty contributes to uncertainty in global flux. 

library(brms)
library(tidyverse)
library(janitor)
library(ggthemes)

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
ep_posts = ep_model %>% 
  as_draws_df() %>% 
  mutate(ep_posts = inv_logit_scaled(b_Intercept)) 

# 4) estimate emergence production as a proportion of insect secondary production
emergence_noep_uncertainty = secondary_prod_sd %>% 
  # expand_grid(ep = ep_posts %>% slice(1:1000) %>% select(ep_posts) %>% pull) %>% 
  mutate(ep = median(ep_posts$ep_posts)) %>% 
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

emergence_production_noep_uncertainty = secondary_prod_sd %>% 
  left_join(emergence_noep_uncertainty) %>% 
  mutate(empirical_emergence = case_when(is.na(emerg) ~ "no", 
                                         TRUE ~ "yes")) 

write_csv(emergence_production_noep_uncertainty, file = "data/emergence_production_noep_uncertainty.csv")


# compare to main analysis that includes ep uncertainty -----------------------------------------------------
emergence_production = readRDS(file = "data/emergence_production.rds") %>% mutate(version = "ep uncertainty")
emergence_production_noep_uncertainty = read_csv(file = "data/emergence_production_noep_uncertainty.csv") %>% mutate(version = "no ep uncertainty") 

bind_rows(emergence_production, emergence_production_noep_uncertainty) %>%
  arrange(version, mean_emergence_mgdmm2y) %>% 
  group_by(version) %>% 
  mutate(rank = 1:max(row_number())) %>% 
  mutate(lower = mean_emergence_mgdmm2y - sd_emergence,
         upper = mean_emergence_mgdmm2y + sd_emergence) %>% 
  ggplot(aes(x = rank, y = mean_emergence_mgdmm2y, ymin = lower, ymax = upper, color = version)) +
    geom_pointinterval() +
  facet_wrap(~version, scales = "free")
  
  




