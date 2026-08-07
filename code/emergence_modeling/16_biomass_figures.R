library(tidyverse)
library(tidybayes)
library(ggridges)
library(tidybayes)
library(brms)
library(patchwork)
library(cowplot)
library(ggthemes)
library(directlabels)
theme_set(theme_default())

# Figure 1c: plot flux per m2 per biome --------------------------------------------------------
source("code/custom_functions/make_summary_table.R")
post_flux_kgdm_perm2_perhybas = readRDS("posteriors/post_flux_kgdm_perm2_perhybas.rds")
hybas_filter <- readRDS("data/hybas_filtered.rds")

d_biome = post_flux_kgdm_perm2_perhybas %>% 
  ungroup %>% 
  left_join(readRDS("data/hybas_covariates.rds")) %>% #loads biome names
  filter(HYBAS_ID %in% hybas_filter) %>% 
  group_by(terr_biom) %>%
  mutate(median_region = median(median, na.rm = T))   %>%
  filter(terr_biom != "NA") %>% 
  mutate(mgDMm2y = median)

d_biome_summary = d_biome %>% 
  group_by(terr_biom, median_region) %>% 
  median_qi(mgDMm2y) %>% 
  make_summary_table(center = "mgDMm2y", digits = 0)

write_csv(d_biome_summary, file = "tables/biome_perm2.csv")

plot_biome_perm2 = d_biome %>% 
  filter(terr_biom != "NA") %>% 
  ggplot(aes(x = mgDMm2y, y = reorder(terr_biom, -median_region))) + 
  stat_density_ridges(aes(fill = as.factor(round(median_region, -2)))) +
  scale_fill_grey(start = 0.8, end = 0.2) +
  guides(fill = "none",
         color = "none") +
  labs(y = "",
       x = expression("Annual Emergence Production (mg m"^-2*" yr"^-1*" dry mass)")) +
  xlim(NA, 5000) +
  NULL

ggsave(plot_biome_perm2, file = "plots/Figure_1c.jpg",
       width = 6.5, height = 6.5, dpi = 400)

ggsave(plot_biome_perm2, file = "plots/Figure_1c.svg",
       width = 6.5, height = 6.5, dpi = 400)

d_biome %>% 
  group_by(terr_biom) %>% 
  tally()



# Figure 1d: phosphorous compare to doughty --------------------------------

post_total_all = readRDS(file = "posteriors/post_total_all.rds")

aquasync_p = post_total_all %>% 
  group_by(units, chemical) %>% 
  median_qi(flux) %>% 
  filter(chemical %in% c("P")) %>% 
  select(chemical, flux) %>% 
  pivot_wider(names_from = chemical, values_from = flux) %>% 
  rename(p_flux_annualkg = P) %>% 
  mutate(source = "AquaSync",
         species = "Aquatic insects",
         ecosystem = "Global Rivers",
         mechanism = "emergence")

compare_to_doughty = post_total_all %>% 
  filter(chemical == "P") %>% 
  ggplot(aes(x = flux/1000)) +
  stat_halfeye() +
  geom_segment(aes(x= 5.6e6/1000,
                   xend = 5.6e6/1000,
                   y = 0,
                   yend = 0.88),
               linetype = "dashed") +
  geom_segment(aes(x= 6.3e6/1000,
                   xend = 6.3e6/1000,
                   y = 0,
                   yend = 0.88),
               linetype = "dotted") +
  geom_segment(aes(x = median(post_total_all %>% filter(chemical == "P") %>% pull(flux))/1000,
                   xend = median(post_total_all %>% filter(chemical == "P") %>% pull(flux))/1000,
                   y = 0,
                   yend = 0.88)) +
  scale_x_log10(labels = comma,
                breaks = c(2500, 5000, 10000, 20000)) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) +
  labs(x = "Global P export per year (t)")

ggsave(compare_to_doughty, file = "plots/Figure_1d.jpg", width = 6, height = 6)  
ggsave(compare_to_doughty, file = "plots/Figure_1d.svg", width = 6, height = 6, dpi = 500)  


# Figure S4: predicted vs observed ----------------------------------------

emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds') %>% 
  mutate(source = case_when(empirical_emergence == "no" ~ "Converted from ACSP",
                            TRUE ~ "Directly Measured")) %>% 
  filter(!is.na(emerge_1))

final_mod = readRDS(   if (file.exists("models/final_mod.rds"))     "models/final_mod.rds"   else     "models/final_mod_small.rds" )
mean_emergence <- mean(emergence_production_with_vars$mean_emergence_mgdmm2y, na.rm = T)

d = emergence_production_with_vars

mean_emergence = mean(d$mean_emergence_mgdmm2y, na.rm = T)

mod_dat = final_mod$data %>% 
  mutate(precip_raw = (precip_s*attributes(d$precip_s)[[3]]) + attributes(d$precip_s)[[2]]) %>% 
  mutate(stream_temp = (stream_temp_s*attributes(d$stream_temp_s)[[3]]) + attributes(d$stream_temp_s)[[2]]) 

data_to_predict = readRDS("data/data_to_predict.rds") # abiotic variables for all 1 million HYBAS

preds_precip = tibble(precip_s = seq(min(data_to_predict$precip_s),
                                     max(data_to_predict$precip_s),
                                     length.out = 30)) %>% 
  add_row(precip_s = c(min(mod_dat$precip_s),
                       max(mod_dat$precip_s))) %>% 
  mutate(HYBAS_ID = "new",
         stream_temp_s = 0) %>% 
  mutate(precip_raw = (precip_s*attributes(d$precip_s)[[3]]) + attributes(d$precip_s)[[2]])  %>% 
  add_epred_draws(final_mod, re_formula = NULL, allow_new_levels = T) %>% 
  mutate(outside_inside = case_when(precip_raw > max(mod_dat$precip_raw) ~ "outside",
                                    precip_raw < min(mod_dat$precip_raw) ~ "outside",
                                    TRUE ~ "inside"))

# plot emergence conditional
precip_emergence_plot = preds_precip %>% 
  ggplot(aes(x = precip_raw, y = (.epred*mean_emergence)/1000)) +
  stat_lineribbon(alpha = 0.25) +
  stat_lineribbon(data = . %>% filter(outside_inside == "inside")) +
  geom_point(data = mod_dat, aes(y = (emerge_mean_centered*mean_emergence)/1000),
             size = 0.5) +
  scale_fill_brewer(palette = "Greens") +
  # guides(fill = "none") +
  labs(y = expression("Annual Emergence Production (g m"^-2*" yr"^-1*" dry mass)"),
       x = expression("Annual Precipitation (mm m"^-2*" yr"^-1*")"),
       fill = "Uncertainty\nInterval",
       subtitle = "A") +
  theme(legend.position = c(0.8, 0.8),
        text = element_text(family = "sans"),
        plot.subtitle = element_text(face = "bold")) 


preds_stream_temp = tibble(stream_temp_s = seq(min(data_to_predict$stream_temp_s),
                                               max(data_to_predict$stream_temp_s),
                                               length.out = 30)) %>% 
  add_row(stream_temp_s = c(min(mod_dat$stream_temp_s),
                            max(mod_dat$stream_temp_s))) %>% 
  mutate(HYBAS_ID = "new",
         precip_s = 0) %>% 
  mutate(stream_temp = (stream_temp_s*attributes(d$stream_temp_s)[[3]]) + attributes(d$stream_temp_s)[[2]],)  %>% 
  add_epred_draws(final_mod, re_formula = NULL, allow_new_levels = T) %>% 
  mutate(outside_inside = case_when(stream_temp > max(mod_dat$stream_temp) ~ "outside",
                                    stream_temp < min(mod_dat$stream_temp) ~ "outside",
                                    TRUE ~ "inside"))

temp_emergence_plot = preds_stream_temp %>% 
  ggplot(aes(x = stream_temp, y = (.epred*mean_emergence)/1000)) +
  stat_lineribbon(alpha = 0.25) +
  stat_lineribbon(data = . %>% filter(outside_inside == "inside")) +
  geom_point(data = mod_dat, aes(y = (emerge_mean_centered*mean_emergence)/1000),
             size = 0.5) +
  scale_fill_brewer(palette = "Greens") +
  guides(fill = "none") +
  labs(y = expression("Annual Emergence Production (g m"^-2*" yr"^-1*" dry mass)"),
       x = "Mean Annual Temperature (\u00b0C)",
       fill = "Uncertainty\nInterval",
       subtitle = "B") +
  theme(text = element_text(family = "sans"),
        plot.subtitle = element_text(face = "bold")) 

library(patchwork)

emergence_two_plots = precip_emergence_plot/temp_emergence_plot + plot_layout(axis_titles = "collect")

post_raw_preds = d %>% 
  rename(emerge_mean_centered = emerge_1) %>% 
  select(emerge_mean_centered, HYBAS_ID, precip_s, stream_temp_s) %>% 
  # mutate(HYBAS_ID = "new") %>% 
  add_epred_draws(final_mod, re_formula = NULL, allow_new_levels = T, ndraws = 1000) %>% 
  mutate(emerge_raw = emerge_mean_centered*mean_emergence, 
         .epred = .epred*mean_emergence) %>%   
  group_by(emerge_raw, HYBAS_ID, precip_s, stream_temp_s) %>% 
  median_qi(.epred)

raw_v_modeled = post_raw_preds %>% 
  ggplot(aes(x = .epred, y = emerge_raw)) +
  geom_pointrange(aes(xmin = .lower, xmax = .upper), alpha = 0.2) +
  labs(x = expression("Modeled kg hybas"^-1*" yr"^-1*")"),
       y = expression("Raw kg hybas"^-1*" yr"^-1*")"),
       subtitle = "C") +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline() +
  theme(text = element_text(family = "sans"),
        plot.subtitle = element_text(face = "bold"))

emergence_prediction = emergence_two_plots | raw_v_modeled + 
  plot_layout(ncol = 2, widths = c(0.8, 0.2))

ggsave(emergence_prediction, file = "plots/Figure_S7.jpg", width = 9, height = 5, dpi = 400)


# Figure S7: direct vs converted measures of emergence --------------------
emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds') %>% 
  mutate(source = case_when(empirical_emergence == "no" ~ "Converted from ACSP",
                            TRUE ~ "Directly Measured")) %>% 
  filter(!is.na(emerge_1)) %>% 
  mutate(emerge_mean_centered = emerge_1,
         precip_raw = pre_cm_syr1000)

a_dir_conv = preds_precip %>% 
  ggplot(aes(x = precip_raw, y = (.epred*mean_emergence)/1000)) +
  geom_point(data = emergence_production_with_vars, aes(y = (emerge_mean_centered*mean_emergence)/1000, 
                                 color = source,
                                 alpha = source),
             size = 1) +
  scale_fill_brewer(palette = "Greens") +
  scale_color_colorblind() +
  scale_alpha_manual(values = c(0.3, 0.9)) +
  guides(color = guide_legend(override.aes = list(alpha = 0.5)),
         alpha = "none") +
  labs(y = expression("Annual Emergence Production (g m"^-2*" yr"^-1*" dry mass)"),
       x = expression("Annual Precipitation (mm m"^-2*" yr"^-1*")"),
       fill = "Uncertainty\nInterval",
       subtitle = "A",
       color = "") +
  theme(legend.position = c(0.8, 0.8),
        legend.title = element_blank(),
        legend.background = element_rect(color = "black", linewidth = 0.1)) +
  # geom_dl(data = emergence_production_with_vars, aes(y = (emerge_mean_centered*mean_emergence)/1000, label = source, 
  #                             color = source, alpha = source), 
  #         method = list("top.points", cex = 0.5,
  #                       dl.trans(y = y + 0.1))) +
  NULL

b_dir_conv = preds_stream_temp %>% 
  ggplot(aes(x = stream_temp, y = (.epred*mean_emergence)/1000)) +
  geom_point(data = emergence_production_with_vars, aes(y = (emerge_mean_centered*mean_emergence)/1000, color = source,
                                 alpha = source, 
                                 size = 1.5),
             size = 1) +
  scale_fill_brewer(palette = "Greens") +
  scale_color_colorblind() +
  scale_alpha_manual(values = c(0.3, 0.9)) +
  guides(alpha = "none", 
         color = "none") +
  labs(y = expression("Annual Emergence Production (g m"^-2*" yr"^-1*" dry mass)"),
       x = "Mean Annual Temperature (\u00b0C)",
       fill = "Uncertainty\nInterval",
       subtitle = "B",
       color = "") +
  NULL

compare_measures_plot = a_dir_conv/b_dir_conv + plot_layout(axis_titles = "collect") & theme(text = element_text(size = 9, family = "sans"),
                                                                                             legend.text = element_text(size = 8),
                                                                                             plot.subtitle = element_text(face = "bold"))

ggsave(compare_measures_plot, file = "plots/Figure_S7.jpg", width = 5, height = 6)


# Figure S13: Simulate PUFA temperature effect ----------------------------

# Simulate PUFA flux under assumption that it declines by 50% across global temperatures. This would be an extreme scenario. The purpose is to check how
# much of a difference even this extreme scenario makes to global flux. i.e., we did not correct for temperature in our PUFA modeling, so this asks "what if we did"?

# Use the fitted parameters from regression models to predict emergence at unmeasured sites

# load data
emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds')
hybas_filter <- readRDS("data/hybas_filtered.rds")

mean_temp = attributes(emergence_production_with_vars$stream_temp_s)$`scaled:center`
sd_temp = attributes(emergence_production_with_vars$stream_temp_s)$`scaled:scale`

data_to_predict = readRDS("data/data_to_predict.rds") %>% 
  filter(HYBAS_ID %in% hybas_filter)

hybas_regions <- readRDS("data/hybas_regions.rds")
post_pufa = readRDS(file = "posteriors/post_pufa.rds")

# load models
final_mod = readRDS(   if (file.exists("models/final_mod.rds"))     "models/final_mod.rds"   else     "models/final_mod_small.rds" )

# get mean emergence to unstandardize
mean_emergence = mean(emergence_production_with_vars$mean_emergence_mgdmm2y, na.rm = T)

# load area of water in each hybas (km2)
hybas_area = readRDS("data/HYBAS_surface_area_REDIST.rds") # redistributed surface areas from Jakob. 


temp_range_s = diff(range(data_to_predict$stream_temp_s))

proportion_change = 0.5
slope = -proportion_change/temp_range_s
zero_prop = 1 - max(data_to_predict$stream_temp_s)/temp_range_s # for determining where 0 is for the intercept. It is not centered. It occurs at 38% of the range of stream_temp_s 
intercept = 1 - proportion_change*zero_prop


# plot temperature correction 
# check - make sure that the pufa correction of proportion change occurs at the maximum stream temp
effect_size_plot = data_to_predict %>%
  mutate(`Temperature corrected` = intercept + slope*stream_temp_s,
         `Not temperature corrected` = 1) %>%
  sample_n(500) %>%
  pivot_longer(cols = c(`Temperature corrected`, `Not temperature corrected`)) %>% 
  mutate(stream_temp = stream_temp_s*sd_temp + mean_temp) %>% 
  ggplot(aes(x = stream_temp, y = value, color = name)) +
  geom_line() +
  facet_wrap(~name) + 
  labs(y = "Effect size",
       x = "Water Temperature °C") +
  ylim(0, 1.19) +
  brms::theme_default() +
  theme(text = element_text(family = "sans"),
        plot.subtitle = element_text(face = "bold"),
        strip.text = element_text(hjust = 0, face = "bold"),
        panel.grid.major.y = element_line(linetype = "dotted",
                                          color = "grey80")) +
  guides(color = "none") +
  scale_color_colorblind()

hybas_predictions_kgdm_peryear = readRDS("posteriors/hybas_predictions_kgdm_peryear.rds")

simulate_pufa_correction = data_to_predict %>% 
  distinct(stream_temp_s, .keep_all = T) %>%
  sample_n(100) %>%
  mutate(author_year = "new") %>% 
  add_epred_draws(final_mod, allow_new_levels = TRUE, 
                  re_formula = NULL, ndraws = 1000) %>% 
  mutate(.epred = .epred*mean_emergence) %>% #mgDM/m2/y
  ungroup %>% 
  mutate(mean_ngPUFA_mgDM = sample(post_pufa$mean_ngPUFA_mgDM, size = nrow(.), replace = T),
         temp_correction = intercept + slope*stream_temp_s,
         mean_ngPUFA_mgDM_tempcorrected = sample(post_pufa$mean_ngPUFA_mgDM, size = nrow(.), replace = T)*temp_correction) %>% 
  pivot_longer(cols = starts_with("mean_ng")) %>% 
  mutate(pufa_concentration = value/1000, #ng to mg
         pufa_flux = pufa_concentration*.epred) %>% #mg/m2/y
  mutate(name = case_when(grepl("corrected", name) ~ "B (EPA + DHA (temperature corrected))",
                          T ~ "A (EPA + DHA)"))

simulate_pufatemp_plot = simulate_pufa_correction %>% 
  ggplot(aes(x = stream_temp, y = pufa_concentration, fill = name)) +
  stat_lineribbon(alpha = 0.25) +
  facet_wrap(~name) +
  brms::theme_default() +
  labs(y = "Tissue Concentration",
       # y = expression("Tissue Concentration (mg g"^-1*" dry mass)"),
       x = "Water Temperature °C", 
       fill = "Interval") +
  theme(text = element_text(family = "sans"),
        plot.subtitle = element_text(face = "bold"),
        strip.text = element_blank()) +
  guides(fill = "none") +
  scale_fill_colorblind()

simulate_pufatemp_flux_plot = simulate_pufa_correction %>% 
  ggplot(aes(x = stream_temp, y = pufa_flux, fill = name)) +
  stat_lineribbon(alpha = 0.25) +
  facet_wrap(~name) +
  brms::theme_default() +
  labs(y = "Export",
       # y = expression("EPA + DHA export (mg m"^-2*" y"^-1*")"),
       x = "Water Temperature °C", 
       fill = "Interval") +
  theme(text = element_text(family = "sans"),
        plot.subtitle = element_text(face = "bold"),
        strip.text = element_blank()) +
  scale_y_log10() +
  guides(fill = "none") +
  scale_fill_colorblind()


library(cowplot)
simulate_pufa_temp_plot = plot_grid(effect_size_plot + labs(x = ""),
                                    simulate_pufatemp_plot + labs(x = ""), 
                                    simulate_pufatemp_flux_plot,
                                    ncol = 1, align = "v",
                                    labels = "AUTO", label_x = 0, label_y = 1)

ggsave(simulate_pufa_temp_plot, file = "plots/Figure_S13.jpg", width = 6, height = 9, dpi = 400)



# Figure S14: PUFA by taxa effect -----------------------------------------

plot_taxa_emerge = readRDS("plots/plot_taxa_emerge.rds")
plot_taxonomic_effect = readRDS("plots/plot_taxonomic_effect.rds")

plot_taxa_emerge/plot_taxonomic_effect

