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
data_to_predict = readRDS("data/data_to_predict.rds")

# 3) combine and plot
test_train = emergence_production_with_vars %>% mutate(data_source = "Data in GAM models") %>% 
  select(HYBAS_ID, precip_s, pre_mm_syr, stream_temp, stream_temp_s, data_source) %>% 
  bind_rows(data_to_predict %>% mutate(data_source = "Global Data") %>% 
              select(HYBAS_ID, precip_s, pre_mm_syr, stream_temp, stream_temp_s, data_source)) 

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
       subtitle = "a) Precipitation") +
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
       subtitle = "b) Temperature") +
  theme(legend.position = c(0.2, 0.8),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

library(patchwork)
precip_temp_test_train = precip_test_train + temp_test_train

# 4) summarize
test_train_summary = test_train %>% 
  select(HYBAS_ID, pre_mm_syr, stream_temp, data_source) %>% 
  pivot_longer(cols = c(pre_mm_syr, stream_temp)) %>% 
  group_by(name, data_source) %>%
  mutate(min = min(value, na.rm = T),
         max = max(value, na.rm = T)) %>% 
  add_tally() %>% 
  group_by(name, data_source, min, max, n) %>% 
  median_qi(value, na.rm = T) 
  
test_train_summary_table = test_train_summary %>% 
  mutate(range = paste0(round(min, 1), " to ", round(max,1)),
         median_cri = paste0(round(value, 1), " (", round(.lower, 1), " to ", round(.upper, 1), ")")) %>% 
  select(name, data_source, median_cri, range, n)

write_csv(test_train_summary_table, file = "tables/test_train_summary_table.csv")
