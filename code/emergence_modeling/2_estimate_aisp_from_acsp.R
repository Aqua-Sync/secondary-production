library(brms)
library(tidyverse)
library(janitor)

secondary_prod = read_csv(file = "data/secondary_prod.csv") %>% 
  select(acsp, aisp, everything())

# Estimate insect secondary production from total production --------------

dat_aispacsp = secondary_prod %>% 
  filter(!is.na(aisp)) %>% 
  filter(!is.na(acsp)) %>% 
  mutate(prop = aisp/acsp,
         prop = case_when(prop >= 1 ~ 0.99,
                           TRUE ~ prop))

# prior for ratio from Gratton et al. 
gratton_fi = read_excel("data/gratton_supplement_A.xlsx") %>% 
  mutate(proportion_insects = parse_number(proportion_insects))


gratton_fi %>% 
  reframe(mean = mean(proportion_insects, na.rm = T),
          sd = sd(proportion_insects, na.rm = T),
          mean_invlogit = mean(inv_logit_scaled(proportion_insects), na.rm = T),
          sd_invlogit = sd(inv_logit_scaled(proportion_insects), na.rm = T))



# Estimate fi: beta model estimating the proportion of insects in total secondary production

# brm_prop_insects_beta = brm(prop ~ 1, 
#                             family = Beta(link = "logit"),
#                             data = dat_aispacsp,
#                             prior = c(prior(normal(0.66, 0.07), class = "Intercept")))

# saveRDS(brm_prop_insects_beta, file = "models/brm_prop_insects_beta.rds")

# get posteriors of proportion of insects
brm_prop_insects_beta = readRDS("models/brm_prop_insects_beta.rds")
brm_prop_insects_beta = update(brm_prop_insects_beta, newdata = dat_aispacsp)


fi_posts = brm_prop_insects_beta %>% 
  as_draws_df() %>% 
  mutate(fi = inv_logit_scaled(b_Intercept))

# multiply acsp by the posterior of proportion of insects. 
# then summarize this as a mean and sd
estimate_aisp = secondary_prod %>%
  filter(is.na(aisp)) %>% 
  filter(!is.na(acsp)) %>% 
  expand_grid(fi_sims = fi_posts %>% slice(1:1000) %>% select(fi) %>% pull()) %>% 
  select(acsp, fi_sims, aisp, everything()) %>% 
  mutate(aisp_sim = acsp*fi_sims) %>% 
  select(acsp, fi_sims, aisp, aisp_sim, everything()) %>% 
  group_by(id) %>% 
  reframe(aisp_mean = mean(aisp_sim, na.rm = T),
          aisp_sd = sd(aisp_sim, na.rm = T)) 

# add the estimated aisp's to the main data set
secondary_prod_sd = secondary_prod %>% 
  left_join(estimate_aisp) %>% 
  rename(aisp_original = aisp) %>% 
  mutate(aisp = case_when(is.na(aisp_original) ~ aisp_mean,
                          TRUE ~ aisp_original),
         aisp_sd = case_when(is.na(aisp_original) ~ aisp_sd,
                             TRUE ~ 0)) %>% 
  select(acsp, aisp_original, aisp, aisp_sd, everything())

saveRDS(secondary_prod_sd, file = "data/secondary_prod_sd.rds")

# Estimate emergence from secondary production ----------------------------

secondary_prod_sd = readRDS(file = "data/secondary_prod_sd.rds") %>% 
  mutate(lat = parse_number(as.character(lat)),
         lon = parse_number(as.character(lon)))

# prior for E:P from Gratton et al. 
gratton_ep = read_csv("data/gratton_supplement_C.csv") %>% clean_names() %>% 
  filter(type == "Streams")

# ep_model = brm(e_p_ratio ~ 1 + (1|reference) + (1|taxa_measured),
#                family = Beta(link = "logit"),
#                data = gratton_ep)
# 
# saveRDS(ep_model, file = "models/ep_model.rds")
ep_model = readRDS("models/ep_model.rds")

ep_posts = ep_model %>% 
  as_draws_df() %>% 
  mutate(ep_posts = inv_logit_scaled(b_Intercept)) 

# estimate emergence production
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

emergence_production %>% 
  ggplot(aes(x = mean_emergence_kgdmm2y)) + 
  geom_histogram() +
  scale_x_log10() +
  geom_vline(aes(xintercept = 1.05*2*(100/90)/1000)) +# Gratton estimate of insect emergence
  NULL

emergence_production %>% 
  ggplot(aes(x = abs(lat), y = mean_emergence_kgdmm2y)) +
  geom_pointrange(aes(ymin = mean_emergence_kgdmm2y - sd_emergence_kg,
                      ymax = mean_emergence_kgdmm2y + sd_emergence_kg)) +
  # geom_smooth() +
  scale_x_log10() +
  scale_y_log10()

