library(janitor)
library(tidyverse)
library(patchwork)
library(tidybayes)
library(ggthemes)

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

# get max emergence to unstandardize
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

ggsave(simulate_pufa_temp_plot, file = "plots/simulate_pufa_temp_plot.jpg", width = 6, height = 9, dpi = 400)



pufa_diffs = simulate_pufa_correction %>% 
  select(.draw, stream_temp, pufa_flux, name, HYBAS_ID) %>% 
  mutate(name = str_sub(name, 1, 1)) %>% 
  pivot_wider(names_from = name, values_from = pufa_flux) %>% 
  mutate(diff = B - A) %>%
  group_by(HYBAS_ID) %>% 
  mutate(median = median(diff))

pufa_diffs %>% 
  ggplot(aes(x = diff, y = reorder(as.character(stream_temp), stream_temp),
             color = stream_temp)) +
  ggridges::geom_density_ridges() +
  theme(axis.text.y = element_blank()) +
  xlim(-1e+4, 1e+4)
  

simulate_pufadiff_plot = pufa_diffs %>% 
  ggplot(aes(x = stream_temp, y = diff/1000)) +
  stat_lineribbon(color = "white", alpha = 0.4) +
  geom_hline(yintercept = 0, color = "black", linewidth = 1) +
  coord_cartesian(ylim = c(-100, 100)) +
  brms::theme_default() +
  theme(text = element_text(family = "sans")) +
  labs(y = "Export Difference",
       # y = expression("Predicted difference in export\n(mg m"^-2*" y"^-1*")"),
       x = "Water Temperature °C", 
       fill = "Interval")

simulate_pufatemp_plot/simulate_pufatemp_flux_plot/simulate_pufadiff_plot +
  plot_layout(axis_titles = "collect",
              guides = "collect")
                  
