library(brms)
library(tidyverse)
library(janitor)
library(ggthemes)

# Convert insect secondary production to emergence production using e:p ratios
# ~2 seconds to 5 minutes (depending on need to compile models)

# 1) load data
secondary_prod_sd = readRDS(file = "data/secondary_prod_sd.rds") %>% 
  mutate(lat = parse_number(as.character(lat)),
         lon = parse_number(as.character(lon)))

# 2) fit model from Gratton data
# prior for E:P from Gratton et al. and Raitif et al.  
gratton_ep = read_csv("data/e_p_ratios.csv") %>% clean_names() %>% 
  filter(type == "Streams")

# ep_model = brm(e_p_ratio ~ 1 + (1|reference) + (1|taxa_measured),
#                family = Beta(link = "logit"),
#                data = gratton_ep,
#                prior = c(prior(exponential(2), class = "sd"),
#                          prior(normal(-1.45, 0.5), class = "Intercept")))
# 
# saveRDS(ep_model, file = "models/ep_model.rds")

ep_model = readRDS("models/ep_model.rds")

# ep_model = update(ep_model, newdata = gratton_ep)

# 3) get posteriors 
ep_posts = ep_model %>% 
  as_draws_df() %>% 
  mutate(ep_posts = inv_logit_scaled(b_Intercept)) 

# 4) estimate emergence production as a proportion of insect secondary production
emergence = secondary_prod_sd %>% 
  expand_grid(ep = ep_posts %>% slice(1:1000) %>% select(ep_posts) %>% pull) %>% 
  select(acsp, aisp_original, aisp, aisp_sd, ep, everything()) %>% 
  mutate(emergence = aisp*ep) %>% 
  group_by(id) %>% 
  mutate(emergence = case_when(is.na(emerg) ~ emergence,  # add empirical measures
                               TRUE ~ emerg),
         emergence_kg = emergence/1e6) %>% 
  reframe(mean_emergence_mgdmm2y = mean(emergence),
          sd_emergence = sd(emergence),
          mean_emergence_kgdmm2y = mean(emergence_kg),
          sd_emergence_kg = sd(emergence_kg)) 

emergence_production = secondary_prod_sd %>% 
  left_join(emergence) %>% 
  mutate(empirical_emergence = case_when(is.na(emerg) ~ "no", 
                                         TRUE ~ "yes")) 

write_csv(emergence_production, file = "data/emergence_production.csv")

# 5) plot

emergence_production = read_csv(file = "data/emergence_production.csv") %>% 
  mutate(source = case_when(empirical_emergence == "no" ~ "Converted from ACSP",
                            TRUE ~ "Directly Measured")) %>% 
  arrange(mean_emergence_mgdmm2y) 


# make three data source: all data, converted only, and emergence only. then plot to show comparison
# this is to address a reviewer comment that a single category on the x-axis is confusing (which we agree)

emer = bind_rows(emergence_production %>% mutate(grouping = "All Data"),
                 emergence_production %>% filter(source == "Directly Measured") %>% mutate(grouping = "Directly Measured"),
                 emergence_production %>% filter(source != "Directly Measured") %>% mutate(grouping = "Converted from ACSP")) %>% 
  mutate(jitter_offset = rnorm(nrow(.), 0, 0.1))

emergence_compared_raw_acsp_revised = emer %>% 
  ggplot(aes(x = grouping, y = mean_emergence_mgdmm2y)) +
  geom_jitter(width = 0.05, height = 0, aes(color = source,
                               alpha = source)) +
  scale_color_colorblind() +
  theme_default() +
  labs(y = expression("Annual Insect Emergence Production (mgDM m"^-2*" yr"^-1*")"),
       x = "") +
  scale_alpha_manual(values = c(0.2, 0.8)) +
  theme(legend.text = element_text(size = 8),
        legend.position = "top",
        legend.title = element_blank()) +
  NULL

ggsave(emergence_compared_raw_acsp_revised , file = "plots/emergence_compared_raw_acsp_revised .jpg",
       width = 6, height = 5, dpi = 400)



emergence_compared_raw_acsp = emergence_production %>% 
  ggplot(aes(y = mean_emergence_mgdmm2y,
             color = source,
             alpha = source,
             x = NA)) + 
  geom_jitter(width = 0.2) +
  # scale_y_log10() +
  theme_default() +
  labs(y = expression("Annual Emergence Production (mg m"^-2*" yr"^-1*" dry mass)"),
       x = "All Emergence Estimates",
       color = "Method",
       alpha = "Method") +
  theme(legend.text = element_text(size = 8),
        axis.text.x = element_blank()) +
  ggthemes::scale_color_colorblind() + 
  NULL

ggsave(emergence_compared_raw_acsp, file = "plots/emergence_compared_raw_acsp.jpg",
       width = 5, height = 5, dpi = 400)


