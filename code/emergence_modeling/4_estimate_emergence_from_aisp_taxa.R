library(brms)
library(tidyverse)
library(janitor)
library(ggthemes)

# Convert insect secondary production to emergence production using e:p ratios
# ~2 seconds to 5 minutes (depending on need to compile models)

# 1) load data
secondary_prod_sd_taxa = read_csv(file = "data/secondary_prod_taxa.csv") %>% 
  select(-aisp) %>% 
  pivot_longer(cols = ends_with("_sp"), names_to = "taxon", 
               values_to = "aisp") %>% 
  # filter(aisp > 0)  %>% 
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
ep_posts_taxa = ep_model$data %>% 
  mutate(taxa_measured = case_when(taxa_measured == "Chironomidae" ~ "Chironomidae",
                           taxa_measured == "Ephemeroptera" ~ "Ephemeroptera",
                           taxa_measured == "Trichoptera" ~ "Trichoptera",
                           T ~ "Insects"
  )) %>% 
  distinct(taxa_measured) %>% 
  mutate(taxon = case_when(taxa_measured == "Chironomidae" ~ "chi_sp",
                           taxa_measured == "Ephemeroptera" ~ "eph_sp",
                           taxa_measured == "Trichoptera" ~ "tri_sp",
                           T ~ "Insects"
  )) %>% 
  filter(!is.na(taxon)) %>%
  mutate(reference = "new") %>% 
  add_epred_draws(ep_model, re_formula = NULL, allow_new_levels = T) 



# 4) estimate emergence production as a proportion of insect secondary production
emergence_taxa = secondary_prod_sd_taxa %>% 
  filter(!is.na(taxon)) %>% 
  filter(aisp > 0) %>%
  filter(!is.na(aisp)) %>% # remidner: aisp here is TAXON specific, not community-wide. 
  mutate(taxon_original = taxon) %>% 
  mutate(taxon = case_when(taxon %in% unique(ep_posts_taxa$taxon) ~ taxon,
                           T ~ "Insects")) %>% # creates a general category for taxa that  don't have a match in the ep model
  left_join(ep_posts_taxa %>% filter(.draw <= 1000) %>% ungroup %>% select(taxon, .draw, .epred),
            relationship = "many-to-many", by = "taxon") %>%
  select(aisp, .epred, taxon_original, everything()) %>% 
  mutate(emergence = aisp*.epred) %>% 
  group_by(id, taxon_original, site_id) %>% 
  mutate(emergence = case_when(is.na(emerg) ~ emergence,  # add empirical measures
                               TRUE ~ emerg),
         emergence_kg = emergence/1e6) %>% 
  reframe(mean_emergence_mgdmm2y = mean(emergence),
          sd_emergence = sd(emergence),
          mean_emergence_kgdmm2y = mean(emergence_kg),
          sd_emergence_kg = sd(emergence_kg)) 


# add predictors and save
emergence_production_taxa = emergence_taxa %>% 
  left_join(readRDS("data/attributes_by_id.rds")) %>% 
  filter(mean_emergence_mgdmm2y > 0) %>% 
  filter(!is.na(mean_emergence_mgdmm2y))

saveRDS(emergence_production_taxa, file = "data/emergence_production_taxa.rds")

# 5) plot

emergence_production_taxa = readRDS(file = "data/emergence_production_taxa.rds") %>% 
  arrange(mean_emergence_mgdmm2y) 

emergence_production_taxa %>% 
  filter(!is.na(mean_emergence_mgdmm2y)) %>% 
  group_by(taxon_original) %>% 
  tally()
