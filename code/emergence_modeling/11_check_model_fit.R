library(tidyverse)
library(brms)
library(janitor)
library(tidybayes)
library(scales)

theme_set(theme_default())



# final_model -------------------------------------------------------------

#plot pp_checks
final_mod = readRDS(file = 'models/final_mod.rds')
emergence_production_with_vars = readRDS(file = 'data/emergence_production_with_vars.rds') %>% # use this for pp_checks b/c the final_mod.rds is a brm_multiple(). If we just run pp_check(final_mod), it will only check the fit for the first sample of data
  rename(emerge_mean_centered = emerge_1)

emergence_final_model_checks = pp_check(final_mod, newdata = emergence_production_with_vars, ndraws = 100) + scale_x_log10()

final_pp_check_emergence = emergence_final_model_checks$data %>% 
  ggplot(aes(x = value, group = rep_id, color = is_y_label)) + 
  geom_density() +
  scale_x_log10(labels = scales::comma) +
  scale_color_manual(values = c("black", "#1E90FF20"),
                     labels = c(expression(italic(y)), 
                                expression(italic(y)[rep]))) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = expression("Insect Emergence (mgDM m"^-2*") [scaled to mean]")) +
  guides(alpha = "none")

ggsave(final_pp_check_emergence, file = "plots/final_pp_check_emergence.jpg",
       width = 4, height = 4, dpi = 500)

# get rhats for each imputation (got this from claude, hence the arrows!)

library(posterior)

# draws <- as_draws_array(final_mod)
# 
# chains_per_imp <- 4
# m <- length(draws_list)
# 
# draws_per_dat <- lapply(1:m, function(i) {
#   chain_ids <- ((i - 1) * chains_per_imp + 1):(i * chains_per_imp)
#   subset_draws(draws, chain = chain_ids)
# })
# 
# final_mod_rhats = lapply(draws_per_dat, summarise_draws, default_convergence_measures())
# saveRDS(final_mod_rhats, file = "models/final_mod_rhats.rds")

final_mod_rhats = readRDS("models/final_mod_rhats.rds")

bind_rows(final_mod_rhats) %>% 
  filter(variable %in% c("Intercept", "bs_sprecip_sstream_temp_s_1",
                         "bs_sprecip_sstream_temp_s_1", "sd_HYBAS_ID__Intercept",
                         "sds_sprecip_sstream_temp_s_1",
                         "shape")) %>% 
  ggplot(aes(x = rhat, y = variable)) + 
  geom_jitter(width = 0, height = 0.05,
              size = 0.1) +
  xlim(NA, 1.1)




# 15 models ---------------------------------------------------------------

# get mod files
gam_files <- list.files(path = "models/",
                        pattern = "^updated_gams_")

gam_files <- gam_files[order(as.numeric(sub("updated_gams_([0-9]+)\\.rds", "\\1", gam_files)))]

# read models
updated_gams <- lapply(paste0("models/",gam_files), readRDS)

pp_data_list = list()

for(i in 1:length(updated_gams)){
pp_data_list[[i]] = pp_check(updated_gams[[i]])$data %>% 
  mutate(formula = as.character(updated_gams[[i]]$formula)[1],
         model_number = i)
}

pp_data = bind_rows(pp_data_list)

emergence_model_checks = pp_data %>% 
  ggplot(aes(x = value, group = rep_id, color = is_y_label)) + 
  geom_density() +
  facet_wrap(~model_number) +
  scale_x_log10(labels = scales::comma) +
  scale_color_manual(values = c("black", "#1E90FF20"),
                     labels = c(expression(italic(y)), 
                                expression(italic(y)[rep]))) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  labs(x = expression("Insect Emergence (mgDM m"^-2*") [scaled to mean]")) +
  guides(alpha = "none")

ggsave(emergence_model_checks, file = "plots/emergence_model_checks.jpg",
       width = 6.5, height = 6, dpi = 500)



# model list --------------------------------------------------------------

model_list = list()

for(i in 1:length(updated_gams)){
  model_list[[i]] = tibble(model_number = i,
                           formula = as.character(updated_gams[[i]]$formula)[1])
}

model_table = bind_rows(model_list)

write_csv(model_table, file = "tables/model_table.csv")

# plot models -------------------------------------------------------------
theme_set(theme_default())

raw_dat = readRDS(file = 'data/emergence_production_with_vars.rds')
mu_precip = mean(raw_dat$pre_mm_syr)
sd_precip = sd(raw_dat$pre_mm_syr)

mean_emergence <- mean(raw_dat$mean_emergence_mgdmm2y, na.rm = T)

mod_dat = updated_gams[[1]]$data %>% as_tibble()
mod = updated_gams[[1]]

mod1_posts = tibble(precip_s = seq(min(mod_dat$precip_s),
                      max(mod_dat$precip_s),
                      length.out = 30)) %>% 
  mutate(HYBAS_ID = "new",
         pre_mm_syr = (precip_s*sd_precip) + mu_precip,
         sd_emergence_1 = 1000) %>% 
  add_epred_draws(mod, allow_new_levels = T, re_formula = NULL) %>% 
  mutate(.epred = .epred*mean_emergence)

plot_emergence_precip = mod1_posts %>% 
  ggplot(aes(x = pre_mm_syr, y = .epred/1000)) +
  stat_lineribbon(alpha = 0.3, color = "white") +
  labs(fill = "Credible Interval",
       y = expression("Emergence (gDM m"^-2*" yr"^-1*")"),
       x = expression("Precipitation (mm basin"^-1*" yr"^-1*")")) +
  geom_point(data = raw_dat, aes(y = mean_emergence_mgdmm2y/1000),
             shape = 1) +
  theme(legend.position = c(0.8, 0.8),
        text = element_text(size = 16))

ggsave(plot_emergence_precip, file = "plots/plot_emergence_precip.jpg", width = 6.5, height = 6.5,
       dpi = 400)

plot_emergence_precip_nodots = mod1_posts %>% 
  ggplot(aes(x = pre_mm_syr, y = .epred/1000)) +
  stat_lineribbon(alpha = 0.3, color = "white") +
  labs(fill = "Credible Interval",
       y = expression("Emergence (gDM m"^-2*" yr"^-1*")"),
       x = expression("Precipitation (mm basin"^-1*" yr"^-1*")")) +
  # geom_point(data = raw_dat, aes(y = mean_emergence_mgdmm2y/1000),
             # shape = 1) +
  # ylim(NA, max(raw_dat$mean_emergence_mgdmm2y/1000)) +
  theme(legend.position = c(0.8, 0.8),
        text = element_text(size = 16))

ggsave(plot_emergence_precip_nodots, file = "plots/plot_emergence_precip_nodots.jpg", width = 6.5, height = 6.5,
       dpi = 400)


# taxa model --------------------------------------------------------------


emergence_production_with_vars_taxa = readRDS("data/emergence_production_with_vars_taxa.rds")
mean_temp = attributes(emergence_production_with_vars_taxa$stream_temp_s)$`scaled:center`
sd_temp = attributes(emergence_production_with_vars_taxa$stream_temp_s)$`scaled:scale`
mean_emergence = mean(emergence_production_with_vars_taxa$mean_emergence_mgdmm2y, na.rm = T)
mod_taxa_emerge = readRDS(file = "models/mod_taxa_emerge.rds")
mod_taxa_data = mod_taxa_emerge$data %>% 
  mutate(.epred = emerge_1*mean_emergence,
         stream_temp = stream_temp_s*sd_temp + mean_temp)
taxon_names = emergence_production_with_vars_taxa %>% glimpse() %>% 
  distinct(taxon_original) %>% 
  mutate(taxon = case_when(taxon_original == "chi_sp" ~ "A (Diptera)",
                           taxon_original == "eph_sp" ~ "B (Ephemeroptera)",
                           taxon_original == "ple_sp" ~ "C (Plecoptera)",
                           taxon_original == "tri_sp" ~ "D (Trichoptera)",
                           T ~ "E (Other)"))


post_taxa_emerge = mod_taxa_emerge$data %>% 
  distinct(taxon_original) %>% 
  expand_grid(precip_s = quantile(mod_taxa_emerge$data$precip_s, probs = c(0.25, 0.5, 0.75))) %>% 
  expand_grid(stream_temp_s = seq(min(mod_taxa_emerge$data$stream_temp_s),
                             max(mod_taxa_emerge$data$stream_temp_s),
                             length.out = 30)) %>% 
  add_epred_draws(mod_taxa_emerge, re_formula = "~ (1|taxon_original)") %>% 
  mutate(.epred = .epred*mean_emergence,
         stream_temp = stream_temp_s*sd_temp + mean_temp) %>% 
  left_join(taxon_names)

plot_taxa_emerge = post_taxa_emerge %>% 
  ggplot(aes(x = stream_temp, y = .epred, fill = taxon)) +
  stat_lineribbon(alpha = 0.25) +
  facet_wrap(~taxon) +
  scale_y_log10() +
  geom_point(data = mod_taxa_data %>% left_join(taxon_names), aes(y = .epred,
                                                                         color = taxon),
             size = 0.2) +
  guides(fill = "none", color = "none") +
  labs(y = expression("Annual Emergence Production (mg m"^-2*" yr"^-1*" dry mass)"),
       x = "Water Temperature °C") +
  theme(strip.text = element_text(hjust = 0))

ggsave(plot_taxa_emerge, file = "plots/plot_taxa_emerge.jpg", width = 6, height = 5)
saveRDS(plot_taxa_emerge, file = "plots/plot_taxa_emerge.rds")


# taxon proportions
post_taxa_pivot = post_taxa_emerge %>% 
  ungroup %>% 
  group_by(taxon_original, .draw, stream_temp_s) %>% 
  reframe(.epred = mean(.epred)) %>% # average over precip
  pivot_wider(names_from = taxon_original, values_from = .epred)


post_taxa_proportions = post_taxa_pivot %>% mutate(total = chi_sp + eph_sp + other_sp + ple_sp + tri_sp) %>% 
  pivot_longer(cols = ends_with("_sp")) %>% 
  mutate(proportion = value/total)

post_taxa_proportions %>% 
  filter(stream_temp_s == min(stream_temp_s) | stream_temp_s == max(stream_temp_s) | stream_temp_s == median(stream_temp_s)) %>% 
  group_by(stream_temp_s, name) %>% 
  median_qi(proportion) %>% 
  arrange(name, stream_temp_s)
  
post_taxa_proportions %>% 
  group_by(stream_temp_s, name) %>% 
  ggplot(aes(x = stream_temp_s, y = proportion, fill = name, color = name)) +
  stat_lineribbon(alpha = 0.25, .width = c(0.5, 0.75)) 
