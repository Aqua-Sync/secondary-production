library(tidyverse)
library(taxize)

# This code allows a quick way of assigning the order or class to an invertebrate taxa.
# The first step is to get list of insect taxonomy the literature table.
# The next step is to use the taxizer package to look up taxonomic info for each name. 
# Then append the taxonomic info to the data table so we can summarize by order or family, etc.

#1) load table with production data
dat = read_csv("data/papers/Junker_Cross_2014_WestBlacktailDeerCreek.csv")

#2) Look up taxonomic info (NOTE: THIS IS INTERACTIVE...CHECK THE CONSOLE)
get_taxa <- taxize::classification(dat$Taxon, return_id = T, db = "gbif")

#3) Wrangle taxonomic info
got_taxa <- get_taxa %>% rbind() %>% as_tibble() %>% 
  select(-id) %>% 
  pivot_wider(names_from = rank, values_from = name) %>% 
  rename(Taxon = query)  # This is specific to the data table. I this case, we need to match by "Taxon", but replace "Taxon" with whatever the correct column is in your data

#4) Join taxa info to original table
dat_taxa = dat %>% left_join(got_taxa)


# Now we can wrangle and summarize by the class/order/etc.For example...

#summarize by order
dat_taxa %>%
  mutate(production = parse_number(gsub("\\(.*?\\)", "", P))) %>% # specialized to remove parentheses and keep only the number before them.
  group_by(order) %>% 
  reframe(production = sum(production))

# Get "OtherSP
dat_taxa %>%
  mutate(production = parse_number(gsub("\\(.*?\\)", "", P))) %>% # specialized to remove parentheses and keep only the number before them.
  # group_by(order) %>% 
  filter(family != "Chironomidae") %>% 
  filter(!order %in% c("Ephemeroptera", "Plecoptera", "Trichoptera")) %>% 
  reframe(production = sum(production))  
