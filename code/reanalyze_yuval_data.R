library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)
library(readxl)

aggregated_biomes_data <- read_csv("data/yuval_etal_terrestrial_data/The global biomass and number of terrestrial arthropods/data/aggregated biomes data.csv") %>% 
  clean_names() %>% 
  mutate(aggregated_biome = aggregated_biome_1)

yuval_raw_data <- read_excel("data/yuval_etal_terrestrial_data/The global biomass and number of terrestrial arthropods/data/RawData.xlsx") %>% 
  clean_names() %>% 
  filter(units != "individuals/m^2") %>% 
  mutate(numerical_value = case_when(grepl("wet ", units) ~ numerical_value*0.3,
                                     TRUE ~ numerical_value)) %>% 
  mutate(median_sd = median(standard_deviation)) %>% 
  group_by(reference, source_in_text, date, site) %>% 
  mutate(sample_id = cur_group_id()) %>% 
  ungroup %>% 
  mutate(standard_deviation = case_when(is.na(standard_deviation) ~ median_sd,
                                        TRUE ~ standard_deviation),
         numerical_value_s = numerical_value/mean(numerical_value))


yuval_raw_data %>% 
  ggplot(aes(x = numerical_value + 1)) + 
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~aggregated_biome, scales = "free")

yuval_raw_data %>% 
  ggplot(aes(x = sample_id, y = numerical_value)) + 
  geom_point() 

yuval_raw_summed = yuval_raw_data %>% 
  group_by(sample_id, aggregated_biome, sub_phylum) %>% 
  reframe(numerical_value = sum(numerical_value))

brm_yuval = brm(numerical_value ~ 1 + (1|sample_id) + (1|aggregated_biome) + (1|sub_phylum),
                family = hurdle_gamma(),
                data = yuval_raw_data,
                prior = c(prior(normal(1, 1), class = "Intercept"),
                          prior(exponential(2), class = "sd")),
                chains = 1, iter = 1000)

saveRDS(brm_yuval, file = "models/brm_yuval.rds")


brm_yuval_posts = brm_yuval$data %>% 
  distinct(aggregated_biome, sub_phylum) %>% 
  mutate(sample_id = "new") %>% 
  add_epred_draws(brm_yuval, re_formula = NULL, ndraws = 500, allow_new_levels = T)


brm_yuval_posts %>% 
  ggplot(aes(x = aggregated_biome, y = .epred, fill = sub_phylum)) +
  stat_halfeye() +
  scale_y_log10()

brm_yuval_posts %>% 
  group_by(aggregated_biome, .draw) %>% 
  reframe(.epred = sum(.epred)) %>% 
  right_join(aggregated_biomes_data %>% filter(!is.na(aggregated_biome))) %>% 
  mutate(.epred = .epred*area) %>% 
  group_by(.draw) %>% 
  reframe(MT = sum(.epred, na.rm = T)/1e09) %>% 
  reframe(mean_mt = mean(MT),
          median_mt = median(MT),
          gm_mt = exp(mean(log(MT))),
          lower = quantile(MT, probs = 0.025),
          upper = quantile(MT, probs = 0.975))
  


