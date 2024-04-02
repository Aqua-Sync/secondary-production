library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(viridis)
library(scales)
library(readxl)

# read bioclim variables
ACSP_Locations_bioclims = read_excel("data/ACSP_Locations_bioclims.xlsx") %>% 
  rename(index = Index) %>% 
  select(-SiteID, Xgeo_dd,  Ygeo_dd)

emergence_production = read_csv(file = "data/emergence_production.csv") %>% 
  separate(site_id, into = c("author", "author2"), extra = "merge") %>% 
  mutate(year = parse_number(author2)) %>% 
  mutate(author_year = paste(author, year, sep = "_")) %>% 
  left_join(ACSP_Locations_bioclims, by = "index") %>% 
  mutate(temp_s = scale(MeanAnnAir),
         elev_s = scale(Elevation_),
         precip_s = scale(AnnPrecip_))

saveRDS(emergence_production, file = "data/emergence_production_bioclim.rds")

emergence_production %>% 
  ggplot(aes(x = AnnPrecip_, y = mean_emergence_kgdmm2y)) +
  geom_pointrange(aes(ymin = mean_emergence_kgdmm2y - sd_emergence_kg,
                      ymax = mean_emergence_kgdmm2y + sd_emergence_kg)) +
  scale_x_log10() +
  # scale_y_log10() +
  NULL


# brm_emerge = brm(mean_emergence_kgdmm2y|mi(sd_emergence_kg) ~ 1 + (1|author_year),
#                  data = emergence_production,
#                  family = Gamma(link = "log"),
#                  prior = c(prior(normal(-2, 2), class = "Intercept"),
#                            prior(exponential(10), class = "sd"),
#                            prior(exponential(10), class = "shape")))
# # 

brm_emerge =readRDS(file = "models/brm_emerge.rds")

# brm_emerge_temp = update(brm_emerge, formula = . ~ temp_s + (1|author_year),
#                          newdata = emergence_production, 
#                          iter = 100, chains = 1)
# brm_emerge_temp = update(brm_emerge_temp, iter = 2000, chains = 4)
# saveRDS(brm_emerge_temp, file = "models/brm_emerge_temp.rds")

brm_emerge_temp = readRDS("models/brm_emerge_temp.rds")


# brm_emerge_elev = update(brm_emerge_temp, formula = . ~ elev_s + (1|author_year), 
#                            newdata = emergence_production)
# 
# brm_emerge_precip = update(brm_emerge_temp, formula = . ~ precip_s + (1|author_year), 
#                            newdata = emergence_production)
# 
# brm_emerge_additive = update(brm_emerge_temp, formula = . ~ precip_s + temp_s + elev_s + (1|author_year), 
#                              newdata = emergence_production)
# 
# brm_emerge_interaction = update(brm_emerge_temp, formula = . ~ precip_s*temp_s*elev_s + (1|author_year), 
#                                 newdata = emergence_production)
# 
# saveRDS(brm_emerge_elev, "models/brm_emerge_elev.rds")
# saveRDS(brm_emerge_precip, "models/brm_emerge_precip.rds")
# saveRDS(brm_emerge_interaction, "models/brm_emerge_interaction.rds")
# saveRDS(brm_emerge_additive, "models/brm_emerge_additive.rds")

brm_emerge_elev = readRDS("models/brm_emerge_elev.rds")
brm_emerge_precip = readRDS("models/brm_emerge_precip.rds")
brm_emerge_interaction = readRDS("models/brm_emerge_interaction.rds")
brm_emerge_additive = readRDS("models/brm_emerge_additive.rds")


plot_temp = plot(conditional_effects(brm_emerge_temp), points = T)
plot_elev = plot(conditional_effects(brm_emerge_elev), points = T)
plot_precip = plot(conditional_effects(brm_emerge_precip), points = T)

plot_temp$temp_s + scale_y_log10()
plot_elev$elev_s + scale_y_log10()
plot_precip$precip_s + scale_y_log10()

waic_int = waic(brm_emerge, newdata = emergence_production %>% filter(!is.na(mean_emergence_kgdmm2y)))
waic_temp = waic(brm_emerge_temp, newdata = emergence_production %>% filter(!is.na(mean_emergence_kgdmm2y)))
waic_elev = waic(brm_emerge_elev, newdata = emergence_production %>% filter(!is.na(mean_emergence_kgdmm2y)))
waic_precip = waic(brm_emerge_precip, newdata = emergence_production %>% filter(!is.na(mean_emergence_kgdmm2y)))
waic_additive = waic(brm_emerge_additive, newdata = emergence_production %>% filter(!is.na(mean_emergence_kgdmm2y)))
waic_interaction = waic(brm_emerge_interaction, newdata = emergence_production %>% filter(!is.na(mean_emergence_kgdmm2y)))

waic_int$estimates %>% 
  as_tibble() %>% 
  mutate(model = "intercept only")





