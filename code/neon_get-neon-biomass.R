library(tidyverse)

neon_size = read_csv("data/neon_biomass/invertebrate-size-data.csv") 

neon_taxa = read_csv("data/neon_biomass/invertebrate-taxonomy.csv")  %>% 
  pivot_longer(cols = c(order, suborder, infraorder, family, subfamily, tribe, scientificName),
               values_to = "taxon") %>% 
  distinct(taxon, class)


neon_biomass = neon_size %>% 
  group_by(across(c(-body_mass, -body_length, -count))) %>% 
  reframe(sum_mgdm = sum(body_mass)) %>% 
  mutate(sum_mgdm_m2 = sum_mgdm/sampling_area) %>% 
  left_join(neon_taxa) %>% 
  group_by(across(c(-taxon, -sum_mgdm, -sum_mgdm_m2, -class))) %>% 
  mutate(insect_mgdm_m2 = case_when(class == "Insecta" ~ sum_mgdm_m2,
                                    TRUE ~ 0)) %>%
  reframe(total_mgdm_m2 = sum(sum_mgdm_m2),
          insect_mgdm_m2 = sum(insect_mgdm_m2)) %>% 
  rename(site_date = site) %>% 
  mutate(site_id = str_sub(site_date, 1, 4),
         date = ymd(str_sub(site_date, 6, 20)),
         year = year(date))

write_csv(neon_biomass, file = "data/neon_biomass/neon_biomass.csv")

# plots to check data
neon_biomass %>% 
  ggplot(aes(x = insect_mgdm_m2, y = total_mgdm_m2)) + 
  geom_point() +
  scale_x_log10() + 
  scale_y_log10()

neon_biomass %>% 
  ggplot(aes(x = insect_mgdm_m2)) + 
  geom_histogram() + 
  scale_x_log10() +
  facet_wrap(~site_id)

neon_biomass %>% 
  pivot_longer(cols = c(total_mgdm_m2, insect_mgdm_m2)) %>% 
  ggplot(aes(x = value, fill = name)) + 
  geom_histogram() + 
  scale_x_log10() +
  facet_wrap(~site_id)

# check sites that have big differences in total vs insect biomass
sites_to_check = neon_biomass %>% 
  mutate(diff = total_mgdm_m2 - insect_mgdm_m2) %>% 
  arrange(-diff) %>% 
  slice(1:10)

neon_size %>% 
  group_by(across(c(-body_mass, -body_length, -count))) %>% 
  reframe(sum_mgdm = sum(body_mass)) %>% 
  mutate(sum_mgdm_m2 = sum_mgdm/sampling_area) %>% 
  left_join(neon_taxa) %>% 
  filter(site %in% c(sites_to_check$site)) %>% 
  group_by(class, site) %>% 
  reframe(total = sum(sum_mgdm_m2)) %>% 
  arrange(-total) %>% 
  print(n = Inf)
# Good. The large differences in total vs insect mass are explained by Malacostraca, likely large Crayfish, along with Bivalves. That seems reasonable.