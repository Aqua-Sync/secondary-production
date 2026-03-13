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
updated_gams = readRDS("models/updated_gams.rds")
max_emergence = max(emergence_production_with_vars$mean_emergence_mgdmm2y, na.rm = T)
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
      add_epred_draws(updated_gams[[3]], allow_new_levels = TRUE, re_formula = NULL, ndraws = 500) %>%
      mutate(.epred = .epred*max_emergence)%>%
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

post_pufatotal_summary = list()
#
for(i in seq_along(data_to_predict_list)) {
  set.seed(20202)
  post_pufatotal_summary[[i]] = data_to_predict_list[[i]] %>%
    # slice(1:600) %>%
    select(HYBAS_ID, precip_s, stream_temp_s) %>%
    mutate(author_year = "new") %>%
    left_join(hybas_area) %>%
    add_epred_draws(updated_gams[[3]], allow_new_levels = TRUE, re_formula = NULL, ndraws = 500) %>%
    ungroup %>%
    mutate(.epred = .epred*max_emergence) %>%
    mutate(kgdmhybasyr = (.epred*(area.redist*1e6))/1e6) %>%  # convert water area to m2. Multiply by mg/m2. It yields mg/hybas. Then divide by 1e6 to get kg/hybas
    select(HYBAS_ID, kgdmhybasyr, .draw) %>%
    group_by(.draw) %>%
    reframe(sum_kgdmyr = sum(kgdmhybasyr)) %>%
    left_join(post_pufa) %>%
    mutate(kgPUFAhybasyr = (mean_ngPUFA_mgDM*(sum_kgdmyr*10e6))/10e12) %>% # convert kgdm to kgPUFA. Multiply pufa concentration (ng/mg) by total flux (kg/10e6 = mg). Then convert ng to kg by dividing by 10e12
    group_by(.draw) %>%
    reframe(sum_kgPUFAyr = sum(kgPUFAhybasyr))
}
#
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

post_total_all %>% 
  # filter(chemical == "C") %>% 
  ggplot(aes(x = flux/1000, fill = chemical)) +
  # stat_slab() +
  # ggridges::geom_density_ridges() +
  geom_density() +
  facet_wrap(~chemical, ncol = 1) +
  scale_x_log10() +
  NULL

post_mass_nutrients_pufa_global = post_total_all %>% 
  group_by(chemical) %>% 
  median_qi(median = flux/1000) %>% 
  mutate(units = "Metric Tons") %>% 
  arrange(-median) %>%
  mutate(across(where(is.double), ~ round(.x, -3)))

write_csv(post_mass_nutrients_pufa_global, 
          file = "tables/post_emergence_global.csv") 

# Bar-on estimate that terrestrial arthropods make up 0.2 gigatons of C (compared to 550 gigatons of total earth biomass).
# We estimate that emerging aquatic insects make up ~0.7 million tons of C. 1 gigaton = 1,000,000,000 metric tons. 0.2 gigatons
# = 200 million metric tons. So emerging aquatic insects make up ~ 0.7/200 = 0.0035 (i.e., 0.35%). This is a good sanity check.
# It matches roughly with the proportion of each habitat on earth (i.e., terrestrial vs river habitat). So we are in the ballpark.


# plot --------------------------------------------------------------------
post_total_region_summary = bind_rows(readRDS(file = "posteriors/post_total_region_summary.rds"))

post_total_global = post_total_region_summary %>% 
  group_by(.draw) %>% 
  reframe(sum_kgdmyr = sum(sum_kgdmyr)) %>% 
  mutate(region_name = "Earth",
         median_region = median(sum_kgdmyr))

regional_and_global_flux = post_total_region_summary %>% 
  filter(region_name != "Greenland") %>%
  group_by(region_name) %>% 
  mutate(median_region = median(sum_kgdmyr)) %>% 
  bind_rows(post_total_global) %>% 
  ggplot(aes(x = sum_kgdmyr, y = reorder(region_name, -median_region),
             fill = log(median_region))) +
  labs(x = bquote("Total aquatic insect emergence (kgDM"%.% y^-1 ~")"),
       y = "Region") +
  stat_halfeye(size = 0.2) +
  scale_fill_viridis() +
  scale_x_log10() +
  guides(fill = "none") +
  theme_bw() +
  geom_vline(xintercept = 1.54e+09) +
  NULL

ggsave(regional_and_global_flux, file = "plots/regional_and_global_flux.jpg", width = 4.5, height = 5,
       dpi = 400)
