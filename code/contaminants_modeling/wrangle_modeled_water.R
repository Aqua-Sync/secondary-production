library(tidyverse)
library(cccapi)
library(data.table)

modeled_water_files = list.files("data/modeled_water_concentrations", pattern = "*.rds")

# empty objects to put data into - one for ,csv + one for .xlsx
data_list = list()

# add files to empty objects
for(name in seq_along(modeled_water_files)){
  data_list[[name]] <- readRDS(paste0("data/modeled_water_concentrations/",modeled_water_files[name]))
}

modeled_water_temp = data.table::rbindlist(data_list, fill = T) %>% 
  mutate(HYBAS_ID = as.numeric(HYBAS_L12)) %>% 
  select(-HYBAS_L12) %>% 
  filter(mean.det.year > 0) # reduces size to only the small number of waters that have detection. Will treat
# them as zeros otherwise later


# add minimum values to hybas with essential elements ---------------------

# filter to only essential elements
essential_water = modeled_water_temp %>% 
  filter(cas %in% c("7782-49-2", "7440-50-8", "7440-66-6")) %>% 
  ungroup

# get essential cas
essential_cas = unique(essential_water$cas)

# get minimum concentrations for essential cas
min_water_concentrations = essential_water %>% 
  group_by(cas) %>% 
  filter(mean.conc.year == min(mean.conc.year)) %>%
  select(-HYBAS_ID) # will add these concentrations to all HYBAS that have reported 0 aqueous concentration but also have flux. That way we can get an estimate of contaminant flux for those hybas based on low assumed concentrations

all_hybas = readRDS("data/hybas_regions.rds") %>% distinct(HYBAS_ID)

hybas_without1 = all_hybas %>% anti_join(essential_water %>% filter(cas == essential_cas[1])) %>% mutate(cas = essential_cas[1])
hybas_without2 = all_hybas %>% anti_join(essential_water %>% filter(cas == essential_cas[2])) %>% mutate(cas = essential_cas[2])
hybas_without3 = all_hybas %>% anti_join(essential_water %>% filter(cas == essential_cas[3])) %>% mutate(cas = essential_cas[3])

# hybas/cas combinations that do not have water concentrations reported.
hybas_without = bind_rows(hybas_without1, hybas_without2, hybas_without3)

# get all hybas for which there is >0 emergence
flux_predictions_all = readRDS("posteriors/hybas_predictions_mass_nutrients.rds")

hybas_with_flux = hybas_without %>% left_join(flux_predictions_all %>% filter(units == "kgdm_peryear") %>% select(HYBAS_ID, mean) %>% 
                                                filter(mean > 0)) 

# add minimum concentrations to the hybas
hybas_to_add = hybas_with_flux %>%
  select(-mean) %>% 
  left_join(min_water_concentrations)

# add estimates to the full dataset
modeled_water_fixed = modeled_water_temp %>% bind_rows(hybas_to_add) %>% distinct()

saveRDS(modeled_water_fixed, file = "data/modeled_water.rds")

unique_cas_split = modeled_water_temp %>% distinct(cas) %>% 
  mutate(cas = as.character(cas)) %>% 
  group_by(cas) %>% group_split()

cas_list = list()

for(i in 1:length(unique_cas_split)){
  cas_name = get_search(unique_cas_split[[i]]$cas)
  
  cas_list[[i]] = tibble(cas = unique_cas_split[[i]]$cas,
                         chemical = cas_name$results$name)
}

cas_names = bind_rows(cas_list)
saveRDS(cas_names, file = "data/cas_names.rds")


