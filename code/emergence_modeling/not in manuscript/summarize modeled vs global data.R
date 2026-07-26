library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)
theme_set(theme_default())


# Get ranges of temp and precip in the training data and compared to the
# range for predictions

# 1) load raw emergence data and predictors. precip_s is the scale() transformed precipitation in mm/km2
emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds') # created in fit_emergence_predictor_models

# 2) load predictors for each hybas
data_to_predict = readRDS("data/data_to_predict.rds") %>% mutate(HYBAS_ID = as.character(HYBAS_ID))

# 3) combine and plot
test_train = emergence_production_with_vars %>% mutate(data_source = "Data in GAM models") %>% 
  select(HYBAS_ID, precip_s, pre_mm_syr, stream_temp, stream_temp_s, data_source, hft_ix_s09, hft_ix_u09) %>% 
  bind_rows(data_to_predict %>% mutate(data_source = "Global Data") %>% 
              select(HYBAS_ID, precip_s, pre_mm_syr, stream_temp, stream_temp_s, data_source, hft_ix_s09, hft_ix_u09)) 

precip_test_train = test_train %>% 
  group_by(data_source) %>% 
  ggplot(aes(x = pre_mm_syr)) +
  geom_density(aes(fill = data_source), alpha = 0.5) +
  xlim(NA, 5000) +
  ggthemes::scale_fill_colorblind() +
  ggthemes::scale_color_colorblind() +
  labs(fill = "",
       color = "",
       x = expression("Annual Precipitation (mm/m"^2*")"),
       subtitle = "A") +
  theme(legend.position = c(0.5, 0.8),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

temp_test_train = test_train %>% 
  group_by(data_source) %>% 
  ggplot(aes(x = stream_temp)) +
  geom_density(aes(fill = data_source), alpha = 0.5) +
  ggthemes::scale_fill_colorblind() +
  ggthemes::scale_color_colorblind() +
  guides(fill = "none",
         color = "none") +
  labs(fill = "",
       color = "",
       x = "Mean Annual Stream Temperature (\u00b0C)",
       subtitle = "B") +
  theme(legend.position = c(0.2, 0.8),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())


hft_ix_s09_test_train = test_train %>% 
  group_by(data_source) %>% 
  ggplot(aes(x = hft_ix_s09)) +
  geom_density(aes(fill = data_source), alpha = 0.5) +
  ggthemes::scale_fill_colorblind() +
  ggthemes::scale_color_colorblind() +
  guides(fill = "none",
         color = "none") +
  labs(fill = "",
       color = "",
       x = "Human Footprint - Sub-basin (hft_ix_s09)",
       subtitle = "C") +
  theme(legend.position = c(0.2, 0.8),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

hft_ix_u09_test_train = test_train %>% 
  group_by(data_source) %>% 
  ggplot(aes(x = hft_ix_u09)) +
  geom_density(aes(fill = data_source), alpha = 0.5) +
  ggthemes::scale_fill_colorblind() +
  ggthemes::scale_color_colorblind() +
  guides(fill = "none",
         color = "none") +
  labs(fill = "",
       color = "",
       x = "Human Footprint - Upstream (hft_ix_u09)",
       subtitle = "D") +
  theme(legend.position = c(0.2, 0.8),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())


# stream width. have to use GRWL for this so data wrangle again: https://zenodo.org/records/2582500

library(sf)

# shp_files <- list.files(
#   "data/frasson_widths",
#   pattern = "\\.shp$",
#   recursive = TRUE,
#   full.names = TRUE
# )
# 
# 
# # Read the shapefile
# frasson_widths = list()
# 
# for(i in 1:length(shp_files)){
#   temp = st_read(shp_files[[i]])
#   frasson_widths[[i]] = temp %>% ungroup %>% as_tibble() %>% select(Width, lakeFlag)
# }
# 
# saveRDS(frasson_widths, file = "data/frasson_widths.rds")
frasson_widths = readRDS(file = "data/frasson_widths.rds")

global_widths = bind_rows(frasson_widths) %>% filter(lakeFlag == 0) %>% filter(Width != -9999) %>% 
  rename(stream_width_m = Width) %>% select(-lakeFlag) %>% 
  mutate(data_source = "Global Data")

test_train_width = emergence_production_with_vars %>% mutate(data_source = "Data in GAM models") %>% 
  select(HYBAS_ID, stream_width_m, data_source) %>% 
  mutate(stream_width_m = parse_number(stream_width_m)) %>% 
  bind_rows(global_widths) %>% 
  filter(stream_width_m > 0)

width_test_train = test_train_width %>% 
  group_by(data_source) %>% 
  ggplot(aes(x = stream_width_m)) +
  geom_density(aes(fill = data_source), alpha = 0.5) +
  ggthemes::scale_fill_colorblind() +
  guides(fill = "none",
         color = "none") +
  labs(fill = "",
       color = "",
       x = "Stream Width (m)",
       subtitle = "E") +
  theme(legend.position = c(0.2, 0.8),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  coord_cartesian(xlim = c(NA, 3000)) +
  NULL


library(patchwork)
precip_temp_test_train = (precip_test_train+temp_test_train+hft_ix_s09_test_train)/(hft_ix_u09_test_train + width_test_train)

# 4) summarize
test_train_summary = test_train %>% 
  select(HYBAS_ID, pre_mm_syr, stream_temp, data_source, hft_ix_u09, hft_ix_s09) %>% 
  pivot_longer(cols = c(pre_mm_syr, stream_temp, hft_ix_u09, hft_ix_s09)) %>% 
  group_by(name, data_source) %>%
  mutate(min = min(value, na.rm = T),
         max = max(value, na.rm = T)) %>% 
  add_tally() %>% 
  group_by(name, data_source, min, max, n) %>% 
  median_qi(value, na.rm = T) 

test_train_summary_width = test_train_width %>% 
  group_by(data_source) %>%
  mutate(min = min(stream_width_m, na.rm = T),
         max = max(stream_width_m, na.rm = T)) %>% 
  add_tally() %>% 
  rename(value = stream_width_m) %>% 
  group_by(data_source, min, max, n) %>% 
  median_qi(value, na.rm = T) %>% 
  mutate(name = "stream_width_m")
  
test_train_summary_table = bind_rows(test_train_summary, test_train_summary_width) %>% 
  mutate(range = paste0(round(min, 1), " to ", round(max,1)),
         median_cri = paste0(round(value, 1), " (", round(.lower, 1), " to ", round(.upper, 1), ")")) %>% 
  select(name, data_source, median_cri, range, n)

write_csv(test_train_summary_table, file = "tables/test_train_summary_table.csv")
