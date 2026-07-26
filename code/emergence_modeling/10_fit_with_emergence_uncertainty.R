library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)
# fit the selected model repeatedly to draws from the posterior of emergence.
# this is how we push forward the uncertainty from emergence estimates into the final projections
# k = draw
# n = number of data points
# m = model
# z = number of repeated models to fit
# for k = 1:z, fit model to the kth draw of data, where each draws has n data points (i.e., a single draw of simulated n data

# read posterior draws of emergence
posts_emergence = readRDS("posteriors/posts_emergence.rds")

# create list of data that has length of k draws
k = 100

draws_list <- lapply(1:k, function(i) {
  df = posts_emergence %>% mutate(emerge_1 = emerge_mean_centered) %>% filter(.draw == i)
  df
})

# fit the selected model to each of the k data sets in draws_list
# the result is a typical brms object that includes the uncertainty of the repeated draws
# 
# final_mod = brm_multiple(emerge_1 ~ s(precip_s, stream_temp_s) + (1 | HYBAS_ID),
#                      family = Gamma(link = "log"),
#                      data = draws_list,
#                      prior = c(prior(normal(0,1), class = Intercept),
#                                prior(normal(0,1), class = b),
#                                prior(exponential(4), class = sd),
#                                prior(lognormal(log(4),0.5), class = shape)),
#                      chains = 4, iter = 2000,
#                      control = list(adapt_delta = 0.85))

final_mod = update(readRDS("models/final_mod.rds"), chains = 4, iter = 2000, newdata = draws_list)

saveRDS(final_mod, file = 'models/final_mod.rds')



# get rhats for each imputation (got this from claude, hence the arrows!)
final_mod = readRDS(file = 'models/final_mod.rds')

library(posterior)

draws <- as_draws_array(final_mod)

chains_per_imp <- 4
m <- length(draws_list)

draws_per_dat <- lapply(1:m, function(i) {
  chain_ids <- ((i - 1) * chains_per_imp + 1):(i * chains_per_imp)
  subset_draws(draws, chain = chain_ids)
})

final_mod_rhats = lapply(draws_per_dat, summarise_draws, default_convergence_measures())
saveRDS(final_mod_rhats, file = "models/final_mod_rhats.rds")

final_mod_rhats = readRDS("models/final_mod_rhats.rds")
