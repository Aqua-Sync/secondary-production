library(tidyverse)
library(tidybayes)
library(viridis)

# Use the fitted parameters from regression models to predict emergence at unmeasured sites

# 1) load data and models -----------------------------
emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds')
hybas_filter <- readRDS("data/hybas_filtered.rds")
data_to_predict_list = readRDS("data/data_to_predict.rds") %>% 
  filter(HYBAS_ID %in% hybas_filter) %>% 
  group_by(region) %>% 
  group_split() # basin-level predictor variables by continent
hybas_regions <- readRDS("data/hybas_regions.rds")
final_mod = readRDS("models/final_mod.rds")
mean_emergence = mean(emergence_production_with_vars$mean_emergence_mgdmm2y, na.rm = T)
hybas_area = readRDS("data/HYBAS_surface_area_REDIST.rds") # redistributed surface areas from Jakob.
post_pufa = readRDS(file = "posteriors/post_pufa.rds")

mean_temp = attributes(emergence_production_with_vars$stream_temp_s)$`scaled:center`
sd_temp = attributes(emergence_production_with_vars$stream_temp_s)$`scaled:scale`

# 2) Get total mass ----------------------------------------------------------

post_total_region_summary = vector("list", length(data_to_predict_list))
system.time(
  for(i in seq_along(data_to_predict_list)) {
    post_total_region_summary[[i]] = data_to_predict_list[[i]] %>%
      select(HYBAS_ID, precip_s, stream_temp_s) %>%
      # slice(1:100) %>%
      mutate(author_year = "new") %>%
      left_join(hybas_area) %>%
      left_join(hybas_regions) %>%
      add_epred_draws(final_mod, allow_new_levels = TRUE, re_formula = NULL, ndraws = 500) %>%
      mutate(.epred = .epred*mean_emergence)%>%
      mutate(kgdmhybasyr = (.epred*(area.redist*1e6))/1e6) %>% # convert water area to m2. Multiply by mg/m2. It yields mg/hybas. Then divide by 1e6 to get kg/hybas
      group_by(.draw, region_name) %>%
      reframe(flux = sum(kgdmhybasyr)) %>%
      mutate(units = "kg_per_y_per_hybas")
  }
)
#
saveRDS(post_total_region_summary, file = "posteriors/post_total_region_summary.rds")
post_total_region_summary = readRDS(file = "posteriors/post_total_region_summary.rds")

# Get total pufa ----------------------------------------------------------
# monte carlo multiply pufa concentrations by emergence flux. Result is pufa flux globally.
# pufa with "_corrected" is not used in the main results. It is used to test what would happen if
# we assumed a strong linear decline in pufa concentrations with temperature. 

# this generates a "correction" that assumes that pufa concentrations decline by 50% linearly across
# the global temperature gradient (water temperature).
temp_range_s = diff(range(bind_rows(data_to_predict_list)$stream_temp_s))
proportion_change = 0.5
slope = -proportion_change/temp_range_s
zero_prop = 1 - max(bind_rows(data_to_predict_list)$stream_temp_s)/temp_range_s # for determining where 0 is for the intercept. It is not centered. It occurs at 38% of the range of stream_temp_s 
intercept = 1 - proportion_change*zero_prop

# estimate pufa flux
post_pufatotal_summary = list()
#
for(i in seq_along(data_to_predict_list)) {
  set.seed(20202)
  post_pufatotal_summary[[i]] = data_to_predict_list[[i]] %>%
    # slice(1:600) %>%
    select(HYBAS_ID, precip_s, stream_temp_s) %>%
    mutate(author_year = "new") %>%
    left_join(hybas_area) %>%
    mutate(pufa_correction = intercept + slope*stream_temp_s) %>% 
    add_epred_draws(final_mod, allow_new_levels = TRUE, re_formula = NULL, ndraws = 100) %>%
    ungroup %>%
    mutate(.epred = .epred*mean_emergence) %>%
    mutate(mean_ngPUFA_kgDM = sample(1e+06*post_pufa$mean_ngPUFA_mgDM, size = nrow(.), replace = T),
           kgdmhybasyr = (.epred*(area.redist*1e6))/1e6,
           ngPUFAhybasyr = kgdmhybasyr*mean_ngPUFA_kgDM,
           ngPUFAhybasyr_corrected = kgdmhybasyr*(mean_ngPUFA_kgDM*pufa_correction)) %>%  # convert water area to m2. Multiply by mg/m2. It yields mg/hybas. Then divide by 1e6 to get kg/hybas
    select(HYBAS_ID, ngPUFAhybasyr,ngPUFAhybasyr_corrected, .draw) %>%
    group_by(.draw) %>%
    reframe(sum_kgPUFAyr = sum(ngPUFAhybasyr)/1e+12,
            sum_kgPUFAyr_corrected = sum(ngPUFAhybasyr_corrected)/1e+12) # 1e+12 to get from ng to kg
}

saveRDS(post_pufatotal_summary, file = "posteriors/post_pufa_summary.rds")

post_pufatotal_summary = readRDS(file = "posteriors/post_pufa_summary.rds")

post_total_dm = bind_rows(post_total_region_summary) %>%
  arrange(.draw) %>% 
  group_by(.draw) %>% 
  reframe(flux = sum(flux),
          chemical = "dm",
          units = "kg_per_y_global")

post_total_C = post_total_dm %>% 
  mutate(flux = (flux*0.9)/2,
         chemical = "C")

post_total_N = post_total_C %>% 
  mutate(flux = flux/6.3,
         chemical = "N")

post_total_P = post_total_C %>% 
  mutate(flux = flux/124,
         chemical = "P")

post_total_pufa = bind_rows(post_pufatotal_summary) %>% 
  group_by(.draw) %>%
  reframe(sum_kgPUFAyr = sum(sum_kgPUFAyr)) %>%
  rename(flux = sum_kgPUFAyr) %>% 
  mutate(flux = flux,
         chemical = "PUFA",
         units = "kg_per_y_global")

post_total_pufa_tempcorrected = bind_rows(post_pufatotal_summary) %>% 
  group_by(.draw) %>%
  reframe(sum_kgPUFAyr_corrected = sum(sum_kgPUFAyr_corrected)) %>%
  rename(flux = sum_kgPUFAyr_corrected) %>% 
  mutate(flux = flux,
         chemical = "PUFA temp corrected",
         units = "kg_per_y_global")

post_total_all = bind_rows(post_total_dm, 
                           post_total_C,
                           post_total_N, 
                           post_total_P,
                           post_total_pufa)

saveRDS(post_total_all, file = "posteriors/post_total_all.rds")

# From Gratton, an average stream has emergence flux of ~1 gC/m2/yr. Using conversions described in Wesner et al. 2020,
# that converts to 2000 mgDM/m2/yr. Allen and Pavelsky estimate 773000 km2 of river globally, which is 7.73e+11 m2.
# So 2000*7.73e+11 = 1.54e+15 mgDM/yr globally, or 1.54e+09 kgDM/yr. That would be the prior expectation.
# C back of the envelope calculation
kg_c_envelope = (1*(7.73e+11))/1000

post_mass_nutrients_pufa_global = post_total_all %>% 
  group_by(chemical) %>% 
  median_qi(median = flux/1000) %>% 
  mutate(units = "Metric Tons") %>% 
  arrange(-median) %>%
  mutate(across(where(is.double), ~ round(.x, -3)))

write_csv(post_mass_nutrients_pufa_global, 
          file = "tables/post_emergence_global.csv") 

# Bar-on estimate that terrestrial arthropods make up 0.2 gigatons of C (compared to 550 gigatons of total earth biomass).
# We estimate that emerging aquatic insects make up ~0.8 million tons of C. 1 gigaton = 1,000,000,000 metric tons. 0.2 gigatons
# = 200 million metric tons. So emerging aquatic insects make up ~ 0.7/200 = 0.0035 (i.e., 0.35%). This is a good sanity check.
# It matches roughly with the proportion of each habitat on earth (i.e., terrestrial vs river habitat). So we are in the ballpark.


# compare pufa with temp corrected
bind_rows(post_total_pufa,
          post_total_pufa_tempcorrected) %>% 
  group_by(chemical) %>% 
  median_qi(flux/1000)
