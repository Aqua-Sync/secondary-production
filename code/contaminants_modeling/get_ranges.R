
range = contaminants %>% 
  pivot_longer(cols = c(water_conc_ugl,
               adult_conc_ng_mg_dm,
               sediment_conc_ugg,
               emergence_mgdm_m2_y)) %>%
  group_by(chemical, name, pub_name, pub_number) %>% 
  mutate(min = min(value, na.rm = T),
         max = max(value, na.rm = T)) %>% 
  distinct(chemical, min, max, chemical_category_broad, pub_number) %>% 
  mutate(range = log10(max) - log10(min)) %>% 
  filter(!is.na(range)) %>% 
  filter(range != "-Inf") %>% 
  filter(range != "Inf") %>% 
  arrange(-range)

View(range)

range %>% 
  filter(!is.na(chemical)) %>% 
  # filter(chemical_category_broad == "metal") %>% 
  ggplot(aes(y = reorder(chemical, range),
             x = range)) + 
  geom_point() +
  facet_wrap(~name)


check_conc = contaminants_raw %>% 
  mutate(water_conc_ugl = parse_number(water_conc_ugl),
         adult_conc_ng_mg_dm = parse_number(adult_conc_ng_mg_dm),
         emergence_mgdm_m2_y = parse_number(emergence_mgdm_m2_y),
         sediment_conc_ugg = parse_number(sediment_conc_ugg)) %>% 
  pivot_longer(cols = c(water_conc_ugl,
                        adult_conc_ng_mg_dm,
                        sediment_conc_ugg,
                        emergence_mgdm_m2_y)) %>%
  group_by(chemical, name, pub_name, pub_number) %>% 
  mutate(min = min(value, na.rm = T),
         max = max(value, na.rm = T)) %>% 
  distinct(chemical, min, max, pub_number) %>% 
  mutate(range_of_order_of_magnitude = log10(max) - log10(min)) %>% 
  filter(!is.na(range_of_order_of_magnitude)) %>% 
  filter(range_of_order_of_magnitude != "-Inf") %>% 
  filter(range_of_order_of_magnitude != "Inf") %>% 
  arrange(-range_of_order_of_magnitude) %>% 
  rename(min_ng_mg = min,
         max_ng_mg = max) %>% 
  head()


write_csv(check_conc, file = "data/check_conc.csv")


unique(check_conc$pub_name)
