# Data for aqua sync project

# This file organizes the full data set to be in the template
# format for the aquasync project on assessing changes to size 
# spectra across landuse gradients, led by Dr. Dan Perkins. 

# written by Justin Pomeranz, October 2023

library(tidyverse)

#stream sites
streamsites=c("HOPB", "LEWI", "POSE", "CUPE",
              "GUIL", "KING", "MCDI", "LECO",
              "WALK", "MAYF", "ARIK", "BLUE",
              "PRIN", "BLDE", "COMO", "WLOU", 
              "SYCA", "REDB", "MART", "MCRA",
              "BIGC", "TECR", "OKSR", "CARI")

# macroinvertebrate raw format
dat <- readRDS("data/derived_data/MN.no.damage.taxa.rds")

# filter out just stream data
dat <- dat %>%
  filter(siteID %in% streamsites)

names(dat)

# make a table of taxonomic information
dat %>%
  select(scientificName, taxonRank:specificEpithet) %>%
  unique() %>%
  write_csv("data/derived_data/aqua-sync_data/invertebrate-taxonomy.csv")


# From Wesner ####
macro <- readRDS("data/raw_data/macro.rds") # to get collection info

# get sampler type for each sampleID

sampler = macro$inv_fieldData %>%
  distinct(sampleID, samplerType, habitatType, substratumSizeClass) %>%
  rename(method = samplerType)


# join sample type to dat

sampler_dat <- dat %>%
  left_join(sampler) %>%
  filter(!is.na(method)) %>%
  mutate(date = ymd(as.Date(collectDate)))

# copied from 00_make_macros_data.R script
size_dat <- sampler_dat %>% 
  group_by(dw, siteID, sampleID, collectDate,
           scientificName, sizeClass, benthicArea,
           method, habitatType, substratumSizeClass) %>% 
  summarize(
    # total number of individuals from sample
    # this is based on the % subsample in the original processing
    n = sum(estimatedTotalCount, na.rm = TRUE))%>%
  filter(!is.na(dw)) %>%
  mutate(organism_group = "macroinvertebrate") 

# select columns necessary for formatting
size_dat <- size_dat %>%
  ungroup() %>%
  select(siteID, sampleID, method,
         collectDate, benthicArea,
         organism_group, scientificName, dw, sizeClass, n)
names(size_dat)


# rename existing columns and add new ones to match aquaSync format
short_dat <- size_dat %>%
  mutate(date = as_date(collectDate),
         body_weight_units = "mg",
         body_length_units = "mm") %>%
  tidyr::unite("site", c(siteID, date), remove = FALSE) %>%
  rename(sampling_area = benthicArea,
         taxon = scientificName,
         body_mass = dw,
         body_length = sizeClass) %>%
  select(-collectDate)

# extract info on site-date combo
# use this to construct site metadate
site_date <- short_dat %>%
  select(site, siteID, date, method) %>%
  unique() %>%
  mutate(sampling_year = year(date),
         sampling_month = month(date)) %>%
  rename(sampling_methodology = method)

# site lat long info
site_meta <- read.csv("data/field_data.csv")
site_date <- left_join(site_date, site_meta, by = join_by(siteID == Site.ID)) %>%
  rename(geographical_position_specification = State,
         geographical_latitude = Latitude,
         geographical_longitude = Longitude,
         geographical_altitude = Elevation) %>%
  mutate(geographical_position = "USA",
         organism_groups = "Invertebrates + Fish",
         trophic_levels = ">2",
         data_owner = "NEON: NAtional Ecological Observatory Network",
         data_contributors = "Pomeranz, Wesner, Junker, Gjoni",
         multiple_sampling = "3 per year") %>%
  select(data_owner, data_contributors, site, geographical_position,
         geographical_position_specification, geographical_latitude,
         geographical_longitude, geographical_altitude, 
         sampling_year, sampling_month, multiple_sampling, organism_groups, 
         trophic_levels, sampling_methodology)

# save as csv
write_csv(site_date, "data/derived_data/aqua-sync_data/site_data.csv")


# template column names:
# site = site name, code
# sampling_method = Surber, kick, etc
# sample = unique sample for that sampling method
# sampling_area = area of each sample in m2
# organism_group = invertebrates or fish
# taxon = taxonomic ID, lowest rank possible
# body_mass = 
# body_length = 
# body_weight_units = 
# body_length_units = 
# count = number of individuals
# multiplier = 1/sample area, multiplier needed for count per m2 for that sample

# extend data set based on count of body size classes in "n" column
# this makes each row of data be for 1 individual
# chnage names and organize to be in AquaSync format
size_dat_formatted <- short_dat %>%
  uncount(n, ) %>%
  mutate(count = 1,
         multiplier = 1 / sampling_area) %>%
  select(-siteID) %>%
  rename(sample = sampleID,
         sampling_method = method) %>%
  select(site, sampling_method, sample, sampling_area,
         organism_group, taxon, body_mass, body_length, body_weight_units,
         body_length_units, count,  multiplier)

names(size_dat_formatted)

View(size_dat_formatted)

write_csv(size_dat_formatted, "data/derived_data/aqua-sync_data/invertebrate-size-data.csv")
