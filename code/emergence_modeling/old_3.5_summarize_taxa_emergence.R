library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)
library(ggthemes)

theme_set(theme_default())

#!NOTE: Taxa's have lots of zeros. Figure out if these are real first!

ep_model = readRDS("models/ep_model.rds")
secondary_prod_sd = readRDS("data/secondary_prod_sd.rds")

ep_taxa = ep_model$data %>% 
  distinct(taxa_measured) %>% 
  mutate(reference = "new") %>% 
  add_epred_draws(ep_model, re_formula = NULL, allow_new_levels = T)

ep_taxa %>% 
  group_by(taxa_measured) %>% 
  median_qi(.epred)

ep_mean = ep_model %>% 
  as_draws_df() %>% 
  mutate(.epred = inv_logit_scaled(b_Intercept)) %>% 
  mutate(.draw = 1:nrow(.)) %>% 
  select(.draw, .epred) %>% 
  mutate(taxa_measured = "aisp",
         reference = "new")

ep_all = bind_rows(ep_taxa, ep_mean) %>% filter(.draw <= 1000)

emergence_taxa = secondary_prod_sd %>% 
  # expand_grid(ep = ep_posts %>% slice(1:1000) %>% select(ep_posts) %>% pull) %>% 
  select(acsp, aisp_original, aisp, aisp_sd, everything()) %>% 
  filter(!is.na(aisp)) %>% 
  pivot_longer(cols = c(aisp, ends_with("_sp"))) %>%
  mutate(taxa_measured = case_when(name == "eph_sp" ~ "Ephemeroptera",   # changes names to match those in Grattons supplement and ep model
                                   name == "tri_sp" ~ "Trichoptera",
                                   name == "ple_sp" ~ "aisp",

                                                                      name == "chi_sp" ~ "Diptera",
                                   name == "aisp" ~ "aisp",
                                   TRUE ~ "aisp")) %>%
  mutate(value = case_when(name != "aisp" & is.na(value) ~ 0, TRUE ~ value)) %>% # add zeros for taxa, but not for total production, since total production NA means no measurement available
  right_join(ep_all, relationship = "many-to-many") %>% 
  mutate(emergence = value*.epred,
         emergence_kg = emergence/1e6) %>% 
  group_by(id, name, taxa_measured) %>% 
  reframe(mean_emergence_mgdmm2y = mean(emergence),
          sd_emergence = sd(emergence),
          mean_emergence_kgdmm2y = mean(emergence_kg),
          sd_emergence_kg = sd(emergence_kg))

emergence_production_taxa = secondary_prod_sd %>% 
  pivot_longer(cols = c(aisp, ends_with("_sp"))) %>% 
  left_join(emergence_taxa) %>% 
  filter(name != "aisp") %>% 
  separate(site_id, into = c("author", "author2"), extra = "merge") %>% 
  mutate(year = parse_number(author2)) %>% 
  mutate(author_year = paste(author, year, sep = "_"))

emergence_production_taxa %>% 
  ggplot(aes(x = mean_emergence_kgdmm2y + 1)) +
  geom_histogram() + 
  facet_wrap(~name) +
  scale_x_log10()


# model
get_prior(bf(mean_emergence_kgdmm2y ~ name + (1|author_year),
             hu ~ name),
          data = emergence_production_taxa,
          family = hurdle_gamma(link = "log"))

brm_emerge_taxa = brm(bf(mean_emergence_kgdmm2y ~ name + (1|author_year),
                         hu ~ name),
                      data = emergence_production_taxa,
                      family = hurdle_gamma(link = "log"),
                      prior = c(prior(normal(-2, 2), class = "Intercept"),
                                prior(normal(0, 0.1), class = "b"),
                                prior(normal(0, 2), class = "b", dpar = "hu"),
                                prior(exponential(10), class = "sd"),
                                prior(exponential(10), class = "shape")))

saveRDS(brm_emerge_taxa, file = "models/brm_emerge_taxa.rds")


pp_check(brm_emerge_taxa) + 
  scale_x_log10()


cond_plot_taxa = plot(conditional_effects(brm_emerge_taxa), points = T)

posts_taxa = brm_emerge_taxa$data %>% 
  distinct(name, author_year) %>% 
  add_epred_draws(brm_emerge_taxa, dpar = c("mu", "hu")) %>% 
  mutate(name = case_when(name == "chi_sp" ~ "Chironomid",
                          name == "tri_sp" ~ "Trichoptera",
                          name == "ple_sp" ~ "Plecoptera",
                          name == "other_sp" ~ "Other Insects",
                          name == "eph_sp" ~ "Ephemeroptera"))


posts_taxa %>% 
  ggplot(aes(x = name, y = .epred)) + 
  stat_slab() +
  geom_point(data = brm_emerge_taxa$data %>% 
               mutate(name = case_when(name == "chi_sp" ~ "Chironomid",
                                       name == "tri_sp" ~ "Trichoptera",
                                       name == "ple_sp" ~ "Plecoptera",
                                       name == "other_sp" ~ "Other Insects",
                                       name == "eph_sp" ~ "Ephemeroptera")), 
             aes(y = mean_emergence_kgdmm2y),
             position = position_jitter(width = 0.1)) +
  scale_y_log10()

saveRDS(posts_taxa, file = "taxa_kgdmy_posterior.rds")

posts_taxa %>% 
  group_by(name) %>% 
  median_qi(.epred)

# global emergence -----------------------------------
river_area_global_m2 = 7.73e+11  # From Allen, G. & Pavelsky, T. (2018) Global extent of rivers and streams. Science, (361), 6398.

river_area_global_km2 = 773000
river_area_global_sd = 79000
river_area_global_m2_sims = tibble(river_global_m2 = rnorm(1000, river_area_global_km2, river_area_global_sd)*1e6)
total_kgdmy = readRDS("data/total_kgdmy_posterior.rds")
posts_taxa = readRDS(file = "taxa_kgdmy_posterior.rds")

posts_taxa %>% 
  ungroup %>% 
  mutate(kgdmy = .epred*rnorm(nrow(.), river_area_global_km2, river_area_global_sd)*1e6) %>% 
  bind_rows(total_kgdmy) %>% 
  group_by(name) %>% 
  mutate(median = median(kgdmy)) %>% 
  ggplot(aes(x = kgdmy/1000, y = reorder(name, median), fill = median)) +
  stat_slabinterval() +
  # scale_x_log10(labels = comma) +
  scale_fill_viridis(begin = 0.5, end = 0.1) +
  guides(fill = "none") +
  labs(x = "Insect Emergence (mtDM/y)",
       y = "") +
  coord_cartesian(xlim = c(0, 1000000))

