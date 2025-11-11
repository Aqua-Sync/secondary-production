library(tidyverse)
library(janitor)

# get proportion contaminants -----------------------------------------------------
# 1) load posteriors for each contaminant
hybas_predictions_mgperyear_fungicide <- readRDS("posteriors/hybas_predictions_mgperyear_filtered_fungicide.rds") %>% 
  select(HYBAS_ID, starts_with("chem_flux"), element)
hybas_predictions_mgperyear_herbicide <- readRDS("posteriors/hybas_predictions_mgperyear_filtered_herbicide.rds") %>% 
  select(HYBAS_ID, starts_with("chem_flux"), element)
hybas_predictions_mgperyear_insecticide <- readRDS("posteriors/hybas_predictions_mgperyear_filtered_insecticide.rds") %>% 
  select(HYBAS_ID, starts_with("chem_flux"), element)
hybas_predictions_metals <- readRDS("posteriors/hybas_predictions_metals.rds") %>% 
  select(HYBAS_ID, starts_with("chem_flux"), element) %>% 
  filter(element %in% c("Cd", "Pb", "Hg")) # filter to only non-essential metals

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

library(data.table)

simulate_flux_sum <- function(iter_num, df) {
  dt <- as.data.table(df)
  
  # Simulate in-place
  dt[, pred := 10^rnorm(.N, mean = log10median, sd = log10sd)]
  
  # Summarize by HYBAS_ID
  result <- dt[, .(sum_hybas_cont = sum(pred, na.rm = T)), by = HYBAS_ID]
  
  # Compute global and proportions in one step
  sum_global <- sum(result$sum_hybas_cont, na.rm = T)
  result[, `:=`(
    sum_global_cont = sum_global,
    iter = iter_num,
    prop_contaminants = sum_hybas_cont / sum_global
  )]
  
  return(result)
}
# 
# # 5) apply the function: repeat simulation X times to get uncertainty
hybas_sums_list_contaminants <- lapply(1:100, simulate_flux_sum, df = hybas_logs)

saveRDS(hybas_sums_list_contaminants, file = "posteriors/hybas_sums_list_contaminants.rds")

# 6) bind rows and summarize

hybas_sums_list_contaminants = readRDS(file = "posteriors/hybas_sums_list_contaminants.rds")
hybas_sums = bind_rows(hybas_sums_list_contaminants)

prop_contaminant_summary = hybas_sums %>% 
  mutate(HYBAS_ID = as.numeric(HYBAS_ID)) %>% 
  # sample_n(10000) %>% 
  group_by(HYBAS_ID) %>% 
  reframe(median = median(prop_contaminants),
          lower = quantile(prop_contaminants, probs = 0.025),
          higher = quantile(prop_contaminants, probs = 0.975))

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
simulate_flux_sum_nutrients <- function(iter_num, df) {
  dt <- as.data.table(df)
  
  # Simulate in-place
  dt[, pred := 10^rnorm(.N, mean = log10median, sd = log10sd)]
  
  # Summarize by HYBAS_ID
  result <- dt[, .(sum_hybas_nut = sum(pred, na.rm = T)), by = HYBAS_ID]
  
  # Compute global and proportions in one step
  sum_global_nut <- sum(result$sum_hybas_nut)
  result[, `:=`(
    iter = iter_num,
    prop_nutrients = sum_hybas_nut / sum_global_nut
  )]
  
  return(result)
}
# 5) apply the function: repeat simulation X times to get uncertainty
hybas_sums_list_nutrients <- lapply(1:100, simulate_flux_sum_nutrients, df = hybas_logs_nutrients)
saveRDS(hybas_sums_list_nutrients, file = "posteriors/hybas_sums_list_nutrients.rds")


# combine contaminants and nutrients --------------------------------------

hybas_sums_list_nutrients = readRDS(file = "posteriors/hybas_sums_list_nutrients.rds")
hybas_sums_list_contaminants = readRDS(file = "posteriors/hybas_sums_list_contaminants.rds")

a = hybas_sums_list_contaminants[[3]] %>% 
            select(HYBAS_ID, prop_contaminants)

b = hybas_sums_list_nutrients[[3]] %>% select(HYBAS_ID, prop_nutrients) %>% 
            mutate(HYBAS_ID = as.character(HYBAS_ID)) 

ab = left_join(b, a)

# check for uniqueness. Are there any HYBAS in contaminants that are not present in nutrients? (there shouldn't be)
setdiff(a$HYBAS_ID, b$HYBAS_ID) #0

# check for uniqueness. And vice versa...there should be HYBAS in nutrients that are not in contaminants, b/c not every place has contamiant predictions
setdiff(b$HYBAS_ID, a$HYBAS_ID) # ~84000

# calculate difference

nut_cont_diffs_list = NULL

for(i in 1:3){
  nut_cont_diffs_list = bind_rows(left_join(hybas_sums_list_contaminants[[i]] %>% 
                                    select(HYBAS_ID, prop_contaminants),
                                  hybas_sums_list_nutrients[[i]] %>% 
                                    select(HYBAS_ID, prop_nutrients) %>% 
                                    mutate(HYBAS_ID = as.character(HYBAS_ID)))) %>% 
    mutate(diff = prop_nutrients - prop_contaminants) %>% 
    group_by(HYBAS_ID) %>% 
    reframe(median = median(diff))
}

ab = left_join(b,a) %>% 
  replace_na(list(prop_contaminants = 0)) %>% 
  mutate(diff = prop_nutrients - prop_contaminants) 



library(data.table)

# Initialize an empty list to collect all diffs
all_diffs <- list()

for (i in 1:100) {
  dt_cont <- as.data.table(hybas_sums_list_contaminants[[i]])[, .(HYBAS_ID, prop_contaminants)]
  dt_nutr <- as.data.table(hybas_sums_list_nutrients[[i]])[, .(HYBAS_ID, prop_nutrients)]
  dt_nutr[, HYBAS_ID := as.character(HYBAS_ID)]
  
  merged <- merge(dt_nutr, dt_cont, by = "HYBAS_ID", all = TRUE)
  merged[, diff := prop_nutrients - prop_contaminants]
  
  all_diffs[[i]] <- merged[, .(HYBAS_ID, diff)]
}

# Combine all diffs into one data.table
combined_diffs <- rbindlist(all_diffs)

# Summarize across all i's by HYBAS_ID
nut_cont_diffs <- combined_diffs[, .(
  median = median(diff, na.rm = TRUE),
  sd = sd(diff, na.rm = TRUE)
), by = HYBAS_ID]

saveRDS(nut_cont_diffs, file = "posteriors/nut_cont_diffs.rds")


# latitude profile --------------------------------------------------------

hybas_covariates = readRDS("data/hybas_covariates.rds") %>% rename(lat = LAT)
nut_cont_diffs = readRDS(file = "posteriors/nut_cont_diffs.rds") %>% mutate(HYBAS_ID = as.numeric(HYBAS_ID)) %>% left_join(hybas_covariates)

saveRDS(nut_cont_diffs, file = "posteriors/nut_cont_diffs.rds")
latitude_profile_relative <- nut_cont_diffs %>%
  mutate(HYBAS_ID = as.numeric(HYBAS_ID)) %>% 
  left_join(hybas_covariates) %>% 
  # group_by(terr_biom) %>% 
  mutate(bin_index = cut_interval(lat, 300, labels = FALSE)) %>% # create 1000 bins per biome
  group_by(bin_index) %>%
  mutate(lat = min(lat, na.rm = T)) %>% 
  reframe(median = median(median, na.rm = TRUE), lat = first(lat)) %>% # calculate median flux per biome and bin
  mutate(pos_neg = case_when(median >= 0 ~ "pos", TRUE ~ "neg"))

# plot
(latitude_profile_plot_relative = latitude_profile_relative %>% 
    ggplot(aes(x = lat, y = median, fill = pos_neg)) +
    geom_bar(stat = "identity") +
    # geom_segment(aes(y = lat, yend = lat, x = 0, xend = median)) +
    ylim(-max(latitude_profile_relative$median, na.rm = T),
           max(latitude_profile_relative$median, na.rm = T)) +
    coord_flip() +
    scale_fill_manual(values = c("#FFB000", "#648FFF" )) +
    labs(x = "Latitude",
         y = "Relative contribution to flux") +
    annotate(geom = "text", label = "Relatively more contaminants", y = -2e-07, x = 50, color = "#ffb000",
             size = 2.5) +
    annotate(geom = "text", label = "Relatively more nutrients", y = 2e-07, x = 50, color = "#648fff",
             size = 2.5) +
    guides(fill = "none") +
    NULL
)

ggsave(latitude_profile_plot_relative, file = "plots/latitude_profile_plot_relative.jpg", width = 4, height = 8)

