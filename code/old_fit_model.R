library(tidyverse)
library(tidybayes)
library(brms)

# 1) load data and standardize -----------------
dat = read_csv("data/secondary_production_data.csv") %>% 
  mutate(mat_deg_c = bio1_mat*0.1,  # recapture units in variables by adjusting for the scale: https://chelsa-climate.org/bioclim/
         precip_kg_m2 = bio12_precip*0.1) %>% 
  mutate(mat_deg_c_s = scale(mat_deg_c), # standardize
         precip_s = scale(precip_kg_m2),
         AISP_mean = mean(AISP_mgAFDM_m2_y),
         AISP_s = AISP_mgAFDM_m2_y/AISP_mean) %>% # scales the response as a proportion of the mean. Keeps values positive for Gamma likelihood
  separate(SiteID, into = c("author1", "author2", "pub_year", "site1", "site2"), remove = F) %>% 
  mutate(site1 = case_when(author1 == "Miller" ~ pub_year,  # account for single author papers so that year and site are in the correct place
                           author1 == "Fisher" ~ pub_year,
                           TRUE ~ site1),
         pub_year = case_when(author1 == "Miller" ~ author2,
                              author1 == "Fisher" ~ author2,
                              TRUE ~ pub_year),
         author2 = case_when(author1 == "Miller" ~ "only",
                              author1 == "Fisher" ~ "only",
                              TRUE ~ author2))

saveRDS(dat, file = "data/secondary_production_data_wrangled.rds")

# 2) plot ---------------------------------------
dat %>% 
  ggplot(aes(x = mat_deg_c_s, y = precip_s)) + 
  geom_point()

# 3) sim priors ----------------------------------

mod_priors = brm(AISP_s ~ mat_deg_c_s + precip_s + (1|author1) + (1|site1) + (1|pub_year),
                 family = Gamma(link = "log"),
                 data = dat,
                 prior = c(prior(normal(1, 3), class = "Intercept"),
                           prior(normal(0, 1), class = "b"),
                           prior(exponential(2), class = "sd")),
                 sample_prior = "only",
                 iter = 1000,
                 chains = 1,
                 file_refit = "on_change",
                 file = "models/mod_priors.rds")


mod_priors = update(mod_priors, prior = c(prior(normal(1, 1), class = "Intercept"),
                                          prior(normal(0, 1), class = "b"),
                                          prior(exponential(2), class = "sd")),
                    file_refit = "on_change",
                    file = "models/mod_priors.rds")


# From Huryn and Wallace 2000: Highest measured secondary production in streams is ~ 1kg/m2/year AFDM
# That would be 1e06 mg/m2/year, which is 1e06/15157 = 65 times higher than the mean in our data

prior_plot = plot(conditional_effects(mod_priors))
prior_plot$mat_deg_c_s + scale_y_log10() + 
  geom_hline(yintercept = 65) +
  scale_y_log10()


# 4) fit models ----------------------------
mod_priors = readRDS("models/mod_priors.rds")

# additive
mod_posterior = update(mod_priors, file = "models/mod_posterior.rds",
                      iter = 2000, chains = 4,
                      sample_prior = F)


# interaction
mod_posterior_int = update(mod_priors, file = "models/mod_posterior_int.rds",
                       iter = 2000, chains = 4,
                       formula = . ~ mat_deg_c_s*precip_s + + (1|author1) + (1|site1) + (1|pub_year),
                       sample_prior = F,
                       newdata = mod_posterior$data)

# conditional plots
cond_plot = plot(conditional_effects(mod_posterior, method = "predict"), points = T)
plot(conditional_effects(mod_posterior_int, method = "predict"), points = T)

saveRDS(cond_plot, file = "plots/cond_plot.rds")

# 5) check models ----------------------------
# interaction doesn't improve model fit. Use additive model
WAIC(mod_posterior, mod_posterior_int)


pp_check(mod_posterior, type = "boxplot") + scale_y_log10() # looks good
pp_check(mod_posterior) # looks good
