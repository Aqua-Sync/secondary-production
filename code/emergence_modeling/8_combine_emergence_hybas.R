library(tidyverse)
library(tidybayes)

# load data
data_to_predict_list = readRDS("data/data_to_predict.rds") %>% group_by(region) %>% group_split()

# load flux summaries -----------------------------------------------------

# List all files that start with "flux_predictionstest"
flux_files <- list.files("posteriors", pattern = "^flux_predictionstest", full.names = TRUE)

# Load each file into a list
flux_list <- lapply(flux_files, readRDS)

flux_predictions = bind_rows(flux_list) %>% mutate(units = "kg_dm_per_hybas_per_yr")

saveRDS(flux_predictions, file = "posteriors/flux_predictions_all.rds")

# plot checks

# From Gratton, an average stream has emergence flux of ~1 gC/m2/yr. Using conversions described in Wesner et al. 2020,
# that converts to 2000 mgDM/m2/yr. Allen and Pavelsky estimate 773000 km2 of river globally, which is 7.73e+11 m2.
# So 2000*7.73e+11 = 1.54e+15 mgDM/yr globally, or 1.54e+09 kgDM/yr. That would be the prior expectation.

# Compare the prior expectation to the sum of median flux across each hydro basin

flux_predictions = readRDS(file = "posteriors/flux_predictions_all.rds")

flux_predictions %>%
  reframe(kgDMperyr = sum(median))

# load flux posteriors ----------------------------------------------------

# List all files that start with "hybas_predictionstest"
flux_posterior_files <- list.files("posteriors", pattern = "^hybas_predictionstest", full.names = TRUE)

# Load each file into a list
flux_posterior_list <- lapply(flux_posterior_files, readRDS)

flux_posterior_filtered = list()

for(i in 1:2){
  flux_posterior_filtered[[i]] = as.data.table(flux_posterior_list[[i]])[.draw <= 100, by = HYBAS_ID]
}


flux_posterior_bind = data.table::rbindlist(flux_posterior_list)


flux_global = bind_rows(flux_posterior_list)  %>% 
  group_by(.draw) %>% 
  reframe(kgyr_global = sum(kgdmhybasyr))

saveRDS(flux_global, file = "posteriors/flux_global.rds")

# flux_region_list = NULL
# for(i in 1:length(flux_posterior_list)){
#   flux_region_list[[i]] = flux_posterior_list[[i]] %>% 
#     group_by(.draw) %>% 
#     reframe(kgyr_global = sum(kgdmhybasyr),
#             region = i)
# }
# flux_region = bind_rows(flux_region_list)
# 
# saveRDS(flux_region, file = "posteriors/flux_region.rds")


