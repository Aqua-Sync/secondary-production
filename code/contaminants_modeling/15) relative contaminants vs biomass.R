library(tidyverse)
library(janitor)

# get proportion contaminants -----------------------------------------------------
# 1) load posteriors for each contaminant
hybas_predictions_mgperyear_fungicide <- readRDS("posteriors/hybas_predictions_mgperyear_fungicide.rds") %>% 
  select(HYBAS_ID, starts_with("chem_flux"), element)
hybas_predictions_mgperyear_herbicide <- readRDS("posteriors/hybas_predictions_mgperyear_herbicide.rds") %>% 
  select(HYBAS_ID, starts_with("chem_flux"), element)
hybas_predictions_mgperyear_insecticide <- readRDS("posteriors/hybas_predictions_mgperyear_insecticide.rds") %>% 
  select(HYBAS_ID, starts_with("chem_flux"), element)
hybas_predictions_metals <- readRDS("posteriors/hybas_predictions_metals.rds") %>% 
  select(HYBAS_ID, starts_with("chem_flux"), element) 

# 2) bind them
hybas_bind_contaminants = bind_rows(hybas_predictions_mgperyear_fungicide,
                                    hybas_predictions_mgperyear_herbicide,
                                    hybas_predictions_mgperyear_insecticide,
                                    hybas_predictions_metals) %>% 
  left_join(readRDS("data/hybas_regions.rds") %>% 
              mutate(HYBAS_ID = as.character(HYBAS_ID)))

# 3) log10 transform. We just have them summarized as medians and CrI's. So to approximate the mean and sd, we 1) log10 transform to make them lognormal,
# then 2) use the theory of large numbers to extract the sd from the CrI (https://stats.stackexchange.com/questions/640075/converting-confidence-interval-to-se-for-lnrate-ratio)

hybas_logs = hybas_bind_contaminants %>% 
  # filter(HYBAS_ID %in% c("7120084570", "7120324170")) %>% 
  # sample_n(1000000) %>%
  filter(chem_flux_mg_year > 0) %>% 
  filter(chem_flux_mg_year_lower95 > 0) %>% 
  filter(chem_flux_mg_year_upper95 > 0) %>% 
  mutate(log10median = log10(chem_flux_mg_year),
         log10lower = log10(chem_flux_mg_year_lower95),
         log10upper = log10(chem_flux_mg_year_upper95),
         log10sd = (log10upper - log10lower)/3.92) %>% 
  select(HYBAS_ID, region_name, starts_with("log10")) 

# 4) make a function: From the log10mean and log10sd, sample repeatedly from each basin to get a distribution of flux per contaminant. 
# Then sum all contaminants per basin and per globe.
# simulate_flux_sum <- function(iter_num, df) {
#   df %>%
#     mutate(pred = 10^rnorm(nrow(.), mean = log10median, sd = log10sd)) %>%
#     group_by(HYBAS_ID) %>%
#     summarise(sum_hybas_cont = sum(pred), .groups = "drop") %>%
#     mutate(sum_global_cont = sum(sum_hybas_cont),
#            iter = iter_num,
#            prop_contaminants = sum_hybas_cont/sum_global_cont)
# }
# 
# # 5) apply the function: repeat simulation X times to get uncertainty
# hybas_sums_list_contaminants <- lapply(1:100, simulate_flux_sum, df = hybas_logs)
# 
# saveRDS(hybas_sums_list_contaminants, file = "posteriors/hybas_sums_list_contaminants.rds")

# 6) bind rows and summarize
hybas_sums = bind_rows(hybas_sums_list)

prop_contaminant_summary = hybas_sums %>% 
  mutate(HYBAS_ID = as.numeric(HYBAS_ID)) %>% 
  # sample_n(10000) %>% 
  group_by(HYBAS_ID) %>% 
  reframe(median = median(sum_hybas/sum_global),
          lower = quantile(sum_hybas/sum_global, probs = 0.025),
          higher = quantile(sum_hybas/sum_global, probs = 0.975))

prop_contaminant_summary %>% 
  ggplot(aes(x = median)) +
  geom_histogram(bins = 200) +
  xlim(NA, 0.00001)

# get proportion biomass and nutrients --------------------------------------------------

# 1) load posteriors for each contaminant and bind them
# Note, we only need to work with dry mass and PUFAs, because C/N/P are linear transformations of dry mass so are redundant for calculating a proportion.
hybas_bind_nutrients = bind_rows(readRDS("posteriors/hybas_predictions_kgPUFA_peryear.rds"),
                                    readRDS("posteriors/hybas_predictions_kgdm_peryear.rds")) %>% 
  left_join(readRDS("data/hybas_regions.rds"))

# 3) log10 transform. We just have them summarized as medians and CrI's. So to approximate the mean and sd, we 1) log10 transform to make them lognormal,
# then 2) use the theory of large numbers to extract the sd from the CrI (https://stats.stackexchange.com/questions/640075/converting-confidence-interval-to-se-for-lnrate-ratio)

hybas_logs_nutrients = hybas_bind_nutrients %>% 
  # filter(HYBAS_ID %in% c("7120084570", "7120324170")) %>% 
  # sample_n(1000000) %>%
  filter(median > 0) %>% 
  filter(lower > 0) %>% 
  filter(upper > 0) %>% 
  mutate(log10median = log10(median),
         log10lower = log10(lower),
         log10upper = log10(upper),
         log10sd = (log10upper - log10lower)/3.92) %>% 
  select(HYBAS_ID, region_name, starts_with("log10")) 

# 4) make a function: From the log10mean and log10sd, sample repeatedly from each basin to get a distribution of flux per nutrient. 
# Then sum all nutrients per basin and per globe.
# simulate_flux_sum_nutrients <- function(iter_num, df) {
#   df %>%
#     mutate(pred = 10^rnorm(nrow(.), mean = log10median, sd = log10sd)) %>%
#     group_by(HYBAS_ID) %>%
#     summarise(sum_hybas_nut = sum(pred), .groups = "drop") %>%
#     mutate(sum_global_nut = sum(sum_hybas_nut),
#            iter = iter_num,
#            prop_nutrients = sum_hybas_nut/sum_global_nut)
# }
# 
# # 5) apply the function: repeat simulation X times to get uncertainty
# hybas_sums_list_nutrients <- lapply(1:100, simulate_flux_sum_nutrients, df = hybas_logs_nutrients)
# saveRDS(hybas_sums_list_nutrients, file = "posteriors/hybas_sums_list_nutrients.rds")


# combine contaminants and nutrients --------------------------------------

hybas_sums_list_nutrients = readRDS(file = "posteriors/hybas_sums_list_nutrients.rds")
hybas_sums_list_contaminants = readRDS(file = "posteriors/hybas_sums_list_contaminants.rds")

set.seed(202020)
test = left_join(hybas_sums_list_contaminants[[3]] %>% mutate(HYBAS_ID = as.numeric(HYBAS_ID)), 
                 hybas_sums_list_nutrients[[3]]) %>% 
  mutate(nutrientprop_minus_contaminantprop = prop_nutrients - prop_contaminants) %>% 
  select(HYBAS_ID, nutrientprop_minus_contaminantprop, iter, prop_nutrients, prop_contaminants) %>% 
  left_join(readRDS("data/hybas_regions.rds")) %>% 
  left_join(readRDS("data/HYBAS_surface_area_REDIST.rds")) 


left_join(hybas_sums_list_contaminants[[3]] %>% mutate(HYBAS_ID = as.numeric(HYBAS_ID)), 
          hybas_sums_list_nutrients[[3]]) %>% 
  mutate(nutrientprop_minus_contaminantprop = prop_nutrients - prop_contaminants) %>% 
  select(HYBAS_ID, nutrientprop_minus_contaminantprop, iter, prop_nutrients, prop_contaminants)

prop_summaries = left_join(bind_rows(hybas_sums_list_contaminants[1:5]) %>% mutate(HYBAS_ID = as.numeric(HYBAS_ID)),
                           bind_rows(hybas_sums_list_nutrients[1:5])) %>% 
  mutate(nutrientprop_minus_contaminantprop = prop_nutrients - prop_contaminants) %>% 
  select(HYBAS_ID, nutrientprop_minus_contaminantprop, iter, prop_nutrients, prop_contaminants) %>% 
  group_by(HYBAS_ID) %>% 
  reframe(median = median(nutrientprop_minus_contaminantprop),
          lower = quantile(nutrientprop_minus_contaminantprop, probs = 0.025),
          upper = quantile(nutrientprop_minus_contaminantprop, probs = 0.975))

library(data.table)

contaminants_df <- map_dfr(hybas_sums_list_contaminants, ~ mutate(.x, HYBAS_ID = as.numeric(HYBAS_ID)))
nutrients_df <- bind_rows(hybas_sums_list_nutrients)

combined <- left_join(contaminants_df, nutrients_df, by = c("HYBAS_ID", "iter"))

dt <- as.data.table(combined)
dt[, nutrientprop_minus_contaminantprop := prop_nutrients - prop_contaminants]

summary_dt <- dt[, .(
  median = median(nutrientprop_minus_contaminantprop, na.rm = T),
  lower = quantile(nutrientprop_minus_contaminantprop, 0.025, na.rm = T),
  upper = quantile(nutrientprop_minus_contaminantprop, 0.975, na.rm = T),
  median_prop_nutrients = median(prop_nutrients, na.rm = T),
  lower_prop_nutrients = quantile(prop_nutrients, 0.025, na.rm = T),
  upper_prop_nutrients = quantile(prop_nutrients, 0.975, na.rm = T),
  median_prop_contaminants = median(prop_contaminants, na.rm = T),
  lower_prop_contaminants = quantile(prop_contaminants, 0.025, na.rm = T),
  upper_prop_contaminants = quantile(prop_nutrients, 0.975, na.rm = T)
), by = HYBAS_ID]

relative_flux = summary_dt %>% 
  left_join(readRDS("data/hybas_regions.rds")) %>% 
  left_join(readRDS("data/HYBAS_surface_area_REDIST.rds")) 

saveRDS(relative_flux, file = "posteriors/relative_flux.rds")

set.seed(202020)
relative_flux %>% 
  # filter(lon >= -95 & lon <= -88) %>% 
  # filter(lat > 27 & lat < 32) %>%
  # filter(median > -0.001) %>% 
  # filter(median < 0.001) %>% 
  sample_n(10000) %>%
  ggplot(aes(x = lon, y = lat, color = median + 1)) +
  geom_point(size = 0.4) +
  coord_map() +
  scale_color_gradient2(midpoint = 1, high = "#2c7bb6", low = "#d7191c", transform = "log10") # colorblind friendly diverging from colorbrewer2.org


  
relative_flux %>% 
  sample_n(10000) %>% 
  ggplot(aes(x = median_prop_contaminants,
             y = median_prop_nutrients)) + 
  geom_point(shape = ".") + 
  facet_wrap(~region_name) +
  geom_abline() +
  scale_x_log10() + 
  scale_y_log10()


scale_to_unit_zero_centered <- function(x) {
  x <- x - mean(x, na.rm = TRUE)
  max_abs <- max(abs(x), na.rm = TRUE)
  scaled <- x / max_abs
  return(scaled)
}

relative_flux %>% 
  # sample_n(100000) %>% 
  group_by(region_name) %>% 
  mutate(region_median = median(median, na.rm = T)) %>%
  ungroup %>% 
  filter(!is.na(median)) %>% 
  mutate(median_s = scale_to_unit_zero_centered(median),
         median_s = log10(median + 1)) %>% 
  ggplot(aes(x = median_s, y = reorder(region_name, region_median), fill = after_stat(x))) +
  ggridges::geom_density_ridges_gradient() +
  # tidybayes::stat_halfeye(.width = c(0.5, 0.75)) +
  scale_fill_viridis(limits = c(-2e-9, 2e-9),
    oob = scales::squish) +
  xlim(-0.0000002, 0.0000002) +
  geom_vline(xintercept = log10(1)) +
  NULL


relative_flux %>% 
  # sample_n(100000) %>% 
  group_by(region_name) %>% 
  mutate(region_median = median(median, na.rm = T)) %>%
  ungroup %>% 
  mutate(median_s = scale_to_unit_zero_centered(median)) %>% 
  group_by(region_name) %>% 
  tidybayes::median_qi(median, na.rm = T, .width = 0.8) %>% 
  ggplot(aes(x = median, xmin = .lower, xmax = .upper)) +
  geom_pointrange(aes(y = region_name))
