library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)
library(readxl)

contaminants = read_excel("data/AquaSync-Contaminant_transfer-2024-6-28.xlsx", 
                          sheet = "main_data_good_names") %>% 
  clean_names() %>%
  mutate(adults_ng_mg_dm = parse_number(as.character(adult_conc_ng_mg_dm)),
         emergence_mgdm_m2_y = str_replace(emergence_mgdm_m2_y, "-", ""),
         emergence_mg_m2_y = parse_number(emergence_mgdm_m2_y),
         water_conc_ugl = str_replace(water_conc_ugl, "-", ""),
         water_conc_ugl = parse_number(water_conc_ugl),
         mean_adults = mean(adults_ng_mg_dm, na.rm = T),
         adults_ng_mg_dm_s = adults_ng_mg_dm/mean_adults,
         sediment_conc_ugg = parse_number(sediment_conc_ugg),
         log10_water_conc_ugl = log10(water_conc_ugl + 0.001),
         log10_sediment_conc_ugg = log10(sediment_conc_ugg + 0.001),
         log10_sediment_conc_ugg_c = log10_sediment_conc_ugg - mean(log10_sediment_conc_ugg, na.rm = T),
         log10_water_conc_ugl_c = log10_water_conc_ugl - mean(log10_water_conc_ugl, na.rm = T)) %>% 
  mutate(chemical_original = chemical) %>% 
  mutate(chemical = case_when(grepl("Hg", chemical) ~ "Hg",
                              TRUE ~ chemical)) %>% 
  group_by(chemical) %>% 
  filter(!is.na(adults_ng_mg_dm)) %>% 
  mutate(mean_adults_grouped = mean(adults_ng_mg_dm, na.rm = T),
         adults_ng_mg_dm_s_grouped = adults_ng_mg_dm/mean_adults_grouped,
         mean_log10_water = mean(log10_water_conc_ugl, na.rm = T),
         sd_log10_water = sd(log10_water_conc_ugl, na.rm = T),
         log10_water_ugl_s_grouped = (log10_water_conc_ugl - mean_log10_water)/sd_log10_water,
         log10_water_s = scale(log10_water_conc_ugl)) %>% 
  filter(!is.na(chemical)) %>% 
  filter(adults_ng_mg_dm_s <= 60) %>% # CHECK THIS Hg ENTRY. IT IS LIKELY MIS-TYPED. REMOVE FOR NOW
  glimpse %>% 
  ungroup %>% 
  mutate(chemical_category_broad = case_when(chemical_category == "Cu" ~ "metal",
                                             chemical_category == "Pb" ~ "metal",
                                             chemical_category == "other metals" ~ "metal",
                                             chemical_category == "thiacloprid" ~ "insecticide",
                                             grepl("Hg", chemical_category) ~ "Hg",
                                             chemical_category %in% c("HOP", "PCB", "PBDE", "PAH") ~ "organics",
                                             TRUE ~ chemical_category))

contaminants %>% 
  distinct(chemical_category_broad) %>% 
  print(n = Inf)


contaminants %>%
  pivot_longer(cols = c(water_conc_ugl, sediment_conc_ugg, adult_conc_ng_mg_dm)) %>% 
  filter(!is.na(value)) %>% 
  group_by(name, chemical) %>% 
  tally() %>% 
  ggplot(aes(y = reorder(chemical, n), 
             x = n, color = name)) +
  geom_point() +
  facet_wrap(~name) +
  labs(x = "Number of records",
       y = "")

a = contaminants %>%
  filter(!is.na(water_conc_ugl)) %>% 
  filter(!is.na(adults_ng_mg_dm)) %>% 
  group_by(chemical) %>% 
  tally() %>% 
  ggplot(aes(y = reorder(chemical, n), 
             x = n)) +
  geom_point() +
  labs(x = "Number of records",
       y = "",
       title = "Number of records with water and adult concentrations in the same study")


b = contaminants %>%
  filter(!is.na(sediment_conc_ugg)) %>% 
  filter(!is.na(adults_ng_mg_dm)) %>% 
  group_by(chemical) %>% 
  tally() %>% 
  ggplot(aes(y = reorder(chemical, n), 
             x = n)) +
  geom_point() +
  labs(x = "Number of records",
       y = "",
       title = "Number of records with sediment and adult concentrations in the same study")

c = contaminants %>%
  filter(!is.na(sediment_conc_ugg)) %>% 
  filter(!is.na(water_conc_ugl)) %>% 
  group_by(chemical) %>% 
  tally() %>% 
  ggplot(aes(y = reorder(chemical, n), 
             x = n)) +
  geom_point() +
  labs(x = "Number of records",
       y = "",
       subtitle = "Number of records with sediment and water concentrations in the same study")


library(patchwork)

number_of_records_plots = a / b / c + plot_layout(heights = c(1, 0.5, 0.2))
ggsave(number_of_records_plots, file = "plots/number_of_records_plots.jpg", width = 6.5, height = 10)



# plot scaled water and scaled adult concentrations
contaminants %>% 
  ggplot(aes(x = log10_water_s, y = adults_ng_mg_dm_s_grouped)) + 
  geom_point() +
  geom_hline(yintercept = 1) +
  scale_y_log10() +
  NULL






# Thiacloprid model

# try with log10(water_conc_ugl) to match Jakob's modeling. Jakob is estimating log10(water_conc_ugl) for Europe.
# brm_thiac = brm(adults_ng_mg_dm_s_grouped ~ log10_water_s + (1|pub_name),
#                 family = Gamma(link = "log"),
#                 prior = c(prior(normal(0.01, 1), class = "Intercept"),
#                            prior(normal(0, 1), class = "b"),
#                            prior(exponential(4), class = "sd")),
#                 data = contaminants %>% filter(chemical == "Thiacloprid"))
# # 
# saveRDS(brm_thiac, file = "models/brm_thiac.rds")

brm_thiac = readRDS(file = "models/brm_thiac.rds")

test = plot(conditional_effects(brm_thiac), points = T)

test$water_conc_ugl + scale_x_log10() +
  scale_y_log10()

brm_se = update(brm_thiac, newdata = contaminants %>% filter(chemical == "Se"))
saveRDS(brm_se, file = "models/brm_se.rds")
brm_cu = update(brm_thiac, newdata = contaminants %>% filter(chemical == "Cu"))
saveRDS(brm_se, file = "models/brm_cu.rds")
brm_hg = update(brm_thiac, newdata = contaminants %>% filter(grepl("Hg", chemical)),
                formula = . ~ log10_sediment_conc_ugg_c + (1|pub_name))
saveRDS(brm_hg, file = "models/brm_hg.rds")

plot(conditional_effects(brm_se), points = T)$log10_water_s + scale_y_log10()
plot(conditional_effects(brm_cu), points = T)$log10_water_s + scale_y_log10()
plot(conditional_effects(brm_thiac), points = T)$log10_water_s + scale_y_log10()
plot(conditional_effects(brm_hg), points = T)$log10_sediment_conc_ugg_c + scale_y_log10()


# emap data mercury ---------------------------------------------------------------

# https://archive.epa.gov/emap/archive-emap/web/txt/nssids4.txt

emap_streams = read_csv("data/emap_streams.txt") %>% 
  mutate(stream_width_m = parse_number(STRMWD)) %>%
  filter(stream_width_m > 0) %>% # ~ 10 streams have zero widths
  filter(!is.na(stream_width_m))  # only 44 sites have missing values

emap_streams %>% 
  filter(stream_width_m <= 60.9) %>%  # ranges used in Peterson et al. Environ. Sci. Technol. 2007, 41, 1, 58–65 
  filter(stream_width_m >= 1.8) %>% 
  ggplot(aes(x = stream_width_m)) + 
  geom_histogram() +
  # scale_x_log10() +
  NULL

gm_width = exp(mean(log(emap_streams$stream_width_m)))
low_width = quantile(emap_streams$stream_width_m, probs = 0.25)
high_width = quantile(emap_streams$stream_width_m, probs = 0.75)

peterson_km = 304544 # page 58 in their article. They assessed "...304,433 km (189,244 miles)."
peterson_percent_affected = 0.11 # their abstract says that "11% of the assessed stream length>" had fish that exceeded 0.1 ug H/g (deemed protective for fish-eating mammals)

streams_affected = tibble(gm_width = gm_width, 
                          low_width = low_width,
                          high_width = high_width,
                          peterson_km = peterson_km,
                          perc_km_affected = peterson_percent_affected,
                          peterson_km_affected = peterson_km*perc_km_affected) %>% 
  pivot_longer(cols = c(gm_width, low_width, high_width)) %>% 
  mutate(km2_sampled = value*peterson_km,
         km2_affected = value*peterson_km_affected,
         perc_km2_affected = km2_affected/km2_sampled)

# what proportion of stream lengths are less than 60m (since <60m is what peterson sampled)

# test proportion less than 60 using the pareto parameter from fig 3. in Allen. They show that 
# stream area frequency is described by either a pareto shape parameter of 0.83 or 0.9 (class a vs class b streams).
# Therefore, we can simulate 10000 stream widths from those parameters and estimate the proportion that are <60m
nsims = 1000

sim_widths = tibble(shape = c(0.83, 0.9)) %>% 
  expand_grid(sims = 1:nsims) %>% 
  mutate(min_width = 0.32) %>% # minimum width of stream in Allen is set to 0.32 m
  mutate(sim_widths = VGAM::rpareto(nrow(.), scale = min_width, shape = shape))

sim_widths %>% 
  group_by(shape) %>% 
  reframe(prop_less_than_60m = sum(sim_widths <= 60)/nsims)

# both reveal 99% of stream widths are <60m. This is stable at 1000 sims (tested with 10000, 1000000 as well)

prop_less_than_60m = sim_widths %>% 
  filter(shape == 0.83) %>% 
  reframe(prop_less_than_60m = sum(sim_widths <= 60)/nsims)

# So, of Earth's 773000 square km, 99 percent is "small" and 11% of that is impacted by mercury

# redo with modeled Hg and modeled emergence
global_flux = readRDS(file = "posteriors/global_flux.rds")# modeled emergence flux posteriors (accounting for temperature)
brm_hg = readRDS(file = "models/brm_hg.rds") # mean mercury concentrations

ndraws = 500

mean_hg = contaminants %>% filter(chemical == "Hg") %>% reframe(mean = mean(adults_ng_mg_dm, na.rm = T)) %>% pull(mean)
sd_hg = contaminants %>% filter(chemical == "Hg") %>% reframe(sd = sd(adults_ng_mg_dm, na.rm = T)) %>% pull(sd)

mercury_flux_global = tibble(as_draws_df(brm_hg) %>% select(b_Intercept, .draw) %>% mutate(b_Intercept = exp(b_Intercept))) %>% 
  filter(.draw <= ndraws) %>% 
  mutate(b_Intercept = b_Intercept*mean_hg) %>% 
  expand_grid(global_MTdmy = global_flux %>% filter(.draw <= ndraws) %>% pull(global_MTdmy)) %>% 
  mutate(prop_less_than_60m = prop_less_than_60m$prop_less_than_60m) %>% 
  expand_grid(prop_impacted_less_than_60m = c(0.01, 0.05, 0.11, 0.17, 0.23)) %>% # best estimate is 11% from Peterson. Others are for range finding
  mutate(proportion_with_mercury = prop_less_than_60m*prop_impacted_less_than_60m) %>% 
  mutate(global_MTdmy_impacted_by_mercury = global_MTdmy*proportion_with_mercury,
         global_mgdmy_impacted_by_mercury = global_MTdmy_impacted_by_mercury*1e+9,
         global_flux_mercury_kg_y = (b_Intercept*global_mgdmy_impacted_by_mercury)/1e+12)

saveRDS(mercury_flux_global, file = "posteriors/mercury_flux_global.rds")

mercury_flux_global %>% 
  group_by(prop_impacted_less_than_60m) %>% 
  median_qi(global_flux_mercury_kg_y)

# By comparison, Pacific salmon transport ~ 7kg Hg per year from marine to freshwater streams in North America via migration (Brandt et al. in review)


# Mining impacted ---------------------------------------------------------

macklin_km_affected = 480700 # global length of rivers affected by mines from Macklin et al. 2023 Science.



