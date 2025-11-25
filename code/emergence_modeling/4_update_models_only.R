library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)

emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds')

mean_temp = attributes(emergence_production_with_vars$stream_temp_s)$`scaled:center`
sd_temp = attributes(emergence_production_with_vars$stream_temp_s)$`scaled:scale`

# data_to_predict = readRDS("data/data_to_predict.rds") %>% 
#   mutate(stream_temp = tmp_dc_syr/10,
#          stream_temp_s = (stream_temp - mean_temp)/sd_temp) 
# 
# saveRDS(data_to_predict, file = "data/data_to_predict.rds")

# load prefit models
updated_gams = readRDS("models/updated_gams.rds") # stores all of the individual models below

# fit models ---------------------

updated_gams_list = list()

for(i in 1:length(updated_gams)){
  updated_gams_list[[i]] = update(updated_gams[[i]], 
                                  newdata = emergence_production_with_vars)
}

updated_int_only_gam = update(updated_gams_list[[4]],
                              prior = c(prior(normal(-2, 1), class = Intercept),
                                        # prior(normal(0, 2), class = b),
                                        prior(exponential(6), class = sd),
                                        prior(exponential(0.01), class = shape)),
                              control = list(adapt_delta = 0.95))

updated_gams_list[[4]] <- updated_int_only_gam

updated_gams = updated_gams_list
saveRDS(updated_gams, file = "models/updated_gams.rds")


new = readRDS("models/updated_gams.rds")
old = readRDS("models/old/updated_gams.rds")

compare_mods = list()

for(i in 1:length(updated_gams)){
  old_posts = as_draws_df(old[[i]]) %>% 
    mutate(model = "old",
           id = i)
 
  new_posts = as_draws_df(new[[i]]) %>%
    mutate(model = "new",
           id = i)
  
  compare_mods[[i]] = bind_rows(old_posts, new_posts)
  
}

compare_posts = bind_rows(compare_mods) %>% 
  select(b_Intercept, starts_with("bs"), sd_author_year__Intercept, sd_HYBAS_ID__Intercept, sds_sprecip_s_1, id, model) %>% 
  pivot_longer(cols = c(-id, -model)) %>% 
  filter(!is.na(value)) %>% 
  group_by(id, model, name) %>% 
  median_qi(value)


compare_posts %>% 
  ggplot(aes(x = name, y = value, ymin = .lower, ymax = .upper)) + 
  geom_pointrange(aes(color = model), position = position_dodge(width = 0.2)) +
  facet_wrap(~id, scales = "free_x") +
  coord_flip()
