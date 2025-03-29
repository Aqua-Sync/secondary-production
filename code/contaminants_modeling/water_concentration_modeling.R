library(tidyverse)
library(brms)
library(tidybayes)
library(bit64)

# water estimates from Jakob on 2/25/2025. He modeled these using RF. I'll try it with a bayes model
# In the RF model, the main predictors were r_mean, ele_mean, tmp_dc_smx, and run_mm_syr, along with ph05_mean, slp_dg_uav, ero_kg_uav, and pre_mm_syr

lead_jeff_positiveonly  = readRDS("data/Lead_Jeff.rds") %>% 
  filter(ndq == "X") # filter to only quantified concentrations

lead_jeff_positiveonly %>% 
  ggplot(aes(x = conc)) + 
  geom_histogram() +
  scale_x_log10() +
  geom_vline(xintercept = 0.00021)

lead_jeff = readRDS("data/Lead_Jeff.rds") %>% 
  mutate(conc = replace_na(conc, replace = 0))

lead_jeff %>% 
  sample_n(10000) %>% 
  mutate(lat_short = round(lat, 0)) %>% 
  ggplot(aes(x = ele_mean, y = conc + 1)) + 
  facet_wrap(~strahl.hydro) +
  geom_point() +
  scale_y_log10() +
  scale_x_log10()



# simulate ----------------------------------------------------------------
set.seed(2020202)
lead_test = lead_jeff %>% 
  sample_n(1000) %>% 
  mutate(x = (lat - mean(lat))/sd(lat), 
         y = (lng - mean(lng))/sd(lng), 
         mean_conc = mean(conc, na.rm = T),
         y_sim = conc/mean(conc),
         det = case_when(conc == 0 ~ 0, TRUE ~ 1))

# gam_fit = brm(bf(y_sim ~ gp(x,y, k=20, c=5/4) + (1|year),
#                  hu ~ gp(x, y, k = 20, c = 5/4)),
#               data = lead_test,
#               family = hurdle_gamma(link = "log"),
#               chains = 1, iter = 200,
#               prior = c(prior(normal(3, 0.5), class = "Intercept"),
#                         prior(normal(0, 0.1), class = "b"),
#                         prior(exponential(2), class = "sd"),
#                         prior(normal(5, 1), class = "Intercept", dpar = "hu")))

# gam_fit = update(gam_fit, iter = 1000, chains = 2,
#                  newdata = lead_test,
#                  prior = c(prior(normal(-3, 0.5), class = "Intercept"),
#                            prior(normal(0, 0.1), class = "b"),
#                            prior(exponential(2), class = "sd"),
#                            prior(normal(5, 1), class = "Intercept", dpar = "hu")))

gam_fit = update(gam_fit, newdata = lead_test, data2 = list(mean_conc = unique(lead_test$mean_conc)),
                 iter = 1000, chains = 2,
                 prior = c(prior(normal(-3, 0.5), class = "Intercept"),
                           prior(normal(0, 0.1), class = "b"),
                           prior(exponential(2), class = "sd"),
                           prior(normal(3, 1), class = "Intercept", dpar = "hu"))) # about an 10% prob of detection

gam_prior_model = update(gam_fit, sample_prior = "only")

saveRDS(gam_fit, file = "models/gam_fit_temp.rds")
saveRDS(gam_prior_model, file = "models/gam_prior_model.rds")

# priors
tibble(x = seq(min(gam_fit$data$x), 
               max(gam_fit$data$x), length.out = 40)) %>% 
  expand_grid(y = c(-1, 0, 1)) %>% 
  mutate(r_mean = 0) %>% 
  add_epred_draws(gam_prior_model, re_formula = NA, dpar = T) %>% 
  ggplot(aes(x = x, y = .epred*mean(lead_test$conc) + 1)) + 
  stat_lineribbon(.width = 0.95) +
  geom_point(data = lead_test, aes(y = conc + 1)) +
  scale_y_log10() +
  NULL

tibble(x = seq(-3, 2.5, length.out = 40)) %>% 
  expand_grid(y = c(-1, 0, 1)) %>% 
  mutate(r_mean = 0) %>% 
  add_epred_draws(gam_prior_model, re_formula = NA, dpar = T) %>% 
  ggplot(aes(x = x, y = 1 - hu)) + 
  stat_lineribbon(.width = 0.95) +
  ylim(0, 1)

# posteriors
tibble(x = seq(min(gam_fit$data$x), 
               max(gam_fit$data$x), length.out = 40)) %>% 
  expand_grid(y = c(-1, 0, 1)) %>% 
  mutate(r_mean = 0,
         mean_conc = gam_fit$data2$mean_conc) %>% 
  add_epred_draws(gam_fit, re_formula = NA, dpar = T) %>% 
  ggplot(aes(x = x, y = .epred*mean_conc + 1)) + 
  stat_lineribbon(.width = 0.95) +
  geom_point(data = lead_test, aes(y = conc + 1)) +
  scale_y_log10() +
  NULL

tibble(x = seq(-3, 2.5, length.out = 40)) %>% 
  expand_grid(y = c(-1, 0, 1)) %>% 
  mutate(r_mean = 0) %>% 
  add_epred_draws(gam_fit, re_formula = NA, dpar = T) %>% 
  ggplot(aes(x = x, y = 1 - hu)) + 
  stat_lineribbon(.width = 0.95) +
  ylim(0, 1)

test_check = pp_check(gam_fit)
test_check$data  %>% 
  ggplot(aes(x = value + 0.000001)) + 
  geom_density(aes(group = rep_id, color = is_y)) +
  scale_x_log10()

lead_validate = lead_jeff %>% anti_join(lead_test, by = "spat") %>% 
           mutate(x = (lat - mean(lead_test$lat))/sd(lead_test$lat), 
                  y = (lng - mean(lead_test$lng))/sd(lead_test$lng), 
                  y_sim = conc/mean(conc))

lead_validate_sample = lead_validate %>% sample_n(500) %>% 
  select(x, y, r_mean, year, conc) %>% 
  rename(y_sim = conc)

posts_validate = lead_validate_sample %>% 
  add_epred_draws(gam_fit, dpar = T, allow_new_levels = T) %>% 
  mutate(.epred = .epred*gam_fit$data2$mean_conc)

posts_culled = posts_validate %>% 
  filter(.draw <= 100) %>% 
  mutate(.epred = case_when(.epred <= 0.00021 ~ 0, TRUE ~ .epred)) 

posts_culled %>% 
  ggplot(aes(x = .epred + 1, y = y_sim + 1)) +
  geom_point() +
  scale_x_log10() + 
  scale_y_log10() +
  geom_abline()

validate_check = pp_check(gam_fit, newdata = lead_validate %>% sample_n(5000) %>% 
                            select(x, y, r_mean, year, conc) %>% 
                            rename(y_sim = conc), allow_new_levels = T)

validate_check$data  %>% 
  ggplot(aes(x = value + 0.00001)) + 
  geom_density(aes(group = rep_id, color = is_y)) +
  scale_x_log10() 


lead_validate_sample = lead_validate %>% sample_n(500) %>% 
  select(x, y, r_mean, year, conc) %>% 
  rename(y_sim = conc)


predictions = predict(gam_fit, lead_validate_sample, allow_new_levels = T) %>% as_tibble() %>% 
  mutate(Estimate = case_when(Estimate*gam_fit$data2$mean_conc <= 0.00021 ~ 0, TRUE ~ Estimate),
         Q2.5 = case_when(Estimate*gam_fit$data2$mean_conc <= 0.00021 ~ 0, TRUE ~ Q2.5),
         Q97.5 = case_when(Estimate*gam_fit$data2$mean_conc <= 0.00021 ~ 0, TRUE ~ Q97.5)) 

lead_validate_sample %>% 
  mutate(median = (predictions$Estimate + 0.0001),
         lower = predictions$Q2.5 + 0.0001,
         upper = predictions$Q97.5 + 0.0001) %>% 
  ggplot(aes(x = y_sim + 0.0001, y = median)) + 
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  geom_hline(yintercept = 0.00021) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Observed",
       y = "Predicted") +
  NULL









