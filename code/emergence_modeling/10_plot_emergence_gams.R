library(tidyverse)
library(brms)
library(tidybayes)
theme_set(theme_default())

emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds')
updated_gams = readRDS("models/updated_gams.rds")
max_emergence <- max(emergence_production_with_vars$mean_emergence_mgdmm2y, na.rm = T)

d = emergence_production_with_vars
mod_dat = updated_gams[[3]]$data %>% 
  mutate(precip_raw = (precip_s*attributes(d$precip_s)[[3]]) + attributes(d$precip_s)[[2]]) %>% 
  mutate(stream_temp = (stream_temp_s*attributes(d$stream_temp_s)[[3]]) + attributes(d$stream_temp_s)[[2]]) 

data_to_predict = readRDS("data/data_to_predict.rds") # abiotic variables for all 1 million HYBAS


preds_precip = tibble(precip_s = seq(min(data_to_predict$precip_s),
                      max(data_to_predict$precip_s),
                      length.out = 30)) %>% 
  add_row(precip_s = c(min(mod_dat$precip_s),
                       max(mod_dat$precip_s))) %>% 
  mutate(HYBAS_ID = "new",
         author_year = "new",
         stream_temp_s = 0) %>% 
  mutate(precip_raw = (precip_s*attributes(d$precip_s)[[3]]) + attributes(d$precip_s)[[2]])  %>% 
  add_epred_draws(updated_gams[[3]], re_formula = NULL, allow_new_levels = T) %>% 
  mutate(outside_inside = case_when(precip_raw > max(mod_dat$precip_raw) ~ "outside",
                                    precip_raw < min(mod_dat$precip_raw) ~ "outside",
                                    TRUE ~ "inside"))

# plot emergence conditional
precip_emergence_plot = preds_precip %>% 
  ggplot(aes(x = precip_raw, y = (.epred*max_emergence)/1000)) +
  stat_lineribbon(alpha = 0.25) +
  stat_lineribbon(data = . %>% filter(outside_inside == "inside")) +
  geom_point(data = mod_dat, aes(y = (emerge_1*max_emergence)/1000),
             size = 0.5) +
  scale_fill_brewer(palette = "Greens") +
  # guides(fill = "none") +
  labs(y = expression("Annual Emergence Production (g m"^-2*" yr"^-1*" dry mass)"),
       x = expression("Annual Precipitation (mm m"^-2*" yr"^-1*")"),
       fill = "Uncertainty\nInterval",
       subtitle = "a)") +
  theme(legend.position = c(0.8, 0.8))


preds_stream_temp = tibble(stream_temp_s = seq(min(data_to_predict$stream_temp_s),
                                     max(data_to_predict$stream_temp_s),
                                     length.out = 30)) %>% 
  add_row(stream_temp_s = c(min(mod_dat$stream_temp_s),
                       max(mod_dat$stream_temp_s))) %>% 
  mutate(HYBAS_ID = "new",
         author_year = "new",
         precip_s = 0) %>% 
  mutate(stream_temp = (stream_temp_s*attributes(d$stream_temp_s)[[3]]) + attributes(d$stream_temp_s)[[2]])  %>% 
  add_epred_draws(updated_gams[[3]], re_formula = NULL, allow_new_levels = T) %>% 
  mutate(outside_inside = case_when(stream_temp > max(mod_dat$stream_temp) ~ "outside",
                                    stream_temp < min(mod_dat$stream_temp) ~ "outside",
                                    TRUE ~ "inside"))

temp_emergence_plot = preds_stream_temp %>% 
  ggplot(aes(x = stream_temp, y = (.epred*max_emergence)/1000)) +
  stat_lineribbon(alpha = 0.25) +
  stat_lineribbon(data = . %>% filter(outside_inside == "inside")) +
  geom_point(data = mod_dat, aes(y = (emerge_1*max_emergence)/1000),
             size = 0.5) +
  scale_fill_brewer(palette = "Greens") +
  guides(fill = "none") +
  labs(y = expression("Annual Emergence Production (g m"^-2*" yr"^-1*" dry mass)"),
       x = "Mean Annual Temperature (\u00b0C)",
       fill = "Uncertainty\nInterval",
       subtitle = "b)") 

library(patchwork)

emergence_two_plots = precip_emergence_plot/temp_emergence_plot + plot_layout(axis_titles = "collect")

ggsave(emergence_two_plots, file = "plots/emergence_two_plots.jpg", width = 6, height = 6)


emergence_prediction = emergence_two_plots | readRDS(file = "plots/raw_vs_modeled_emergence_per_hybas.rds") + 
  plot_layout(ncol = 2, widths = c(0.8, 0.2))

ggsave(emergence_prediction, file = "plots/emergence_prediction.jpg", width = 9, height = 5, dpi = 400)

