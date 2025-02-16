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

modeled_water = data.table::rbindlist(data_list, fill = T) %>% 
  filter(mean.det.year > 0) # reduces size to only the small number of waters that have detection. Will treat
# them as zeros otherwise later

saveRDS(modeled_water, file = "data/modeled_water.rds")

unique_cas_split = modeled_water %>% distinct(cas) %>% 
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


