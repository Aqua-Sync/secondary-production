library(brms)
library(tidyverse)
library(janitor)

# 1) load data
# acsp = aquatic community secondary production (insects and non-insects)
# aisp = aquatic insect secondary production (insects only)
secondary_prod = read_csv(file = "data/secondary_prod.csv") %>% 
  select(acsp, aisp, everything())

# Estimate insect secondary production from total community production --------------

# 2) calculate raw proportion
dat_aispacsp = secondary_prod %>% 
  filter(!is.na(aisp)) %>% 
  filter(!is.na(acsp)) %>% 
  mutate(prop = aisp/acsp,
         prop = case_when(prop >= 1 ~ 0.99,
                           TRUE ~ prop))

# 3) get prior ratio from Gratton et al. 
gratton_fi = read_excel("data/gratton_supplement_A.xlsx") %>% 
  mutate(proportion_insects = parse_number(proportion_insects))


# 4) fit model
# Estimates fi: beta model estimating the proportion of insects in total secondary production

# brm_prop_insects_beta = brm(prop ~ 1, 
#                             family = Beta(link = "logit"),
#                             data = dat_aispacsp,
#                             prior = c(prior(normal(0.66, 0.07), class = "Intercept")))

# saveRDS(brm_prop_insects_beta, file = "models/brm_prop_insects_beta.rds")
# brm_prop_insects_beta = update(readRDS(file = "models/brm_prop_insects_beta.rds"),
#                                newdata = dat_aispacsp)
# saveRDS(brm_prop_insects_beta, file = "models/brm_prop_insects_beta.rds")

# 5) get posteriors of proportion of insects
brm_prop_insects_beta = readRDS("models/brm_prop_insects_beta.rds")

fi_posts = brm_prop_insects_beta %>% 
  as_draws_df() %>% 
  mutate(fi = inv_logit_scaled(b_Intercept))

# 6) Use posterior proportion to convert acsp to aisp; multiply acsp by the posterior of proportion of insects. 
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

# 7) add the estimated aisp's to the main data set and save
secondary_prod_sd = secondary_prod %>% 
  left_join(estimate_aisp) %>% 
  rename(aisp_original = aisp) %>% 
  mutate(aisp = case_when(is.na(aisp_original) ~ aisp_mean,
                          TRUE ~ aisp_original),
         aisp_sd = case_when(is.na(aisp_original) ~ aisp_sd,
                             TRUE ~ 0)) %>% 
  select(acsp, aisp_original, aisp, aisp_sd, everything())

saveRDS(secondary_prod_sd, file = "data/secondary_prod_sd.rds")

