library(tidyverse)
library(tidybayes)
library(brms)
library(ggridges)

# 1) load data and model -------------------------------
dat_wrangled = readRDS("data/secondary_production_data_wrangled.rds")

mod_posterior = readRDS(file = "models/mod_posterior.rds")

# get means and sds from the scaling
mean_mat = attributes(dat_wrangled$mat_deg_c_s)$`scaled:center`
sd_mat = attributes(dat_wrangled$mat_deg_c_s)$`scaled:scale`

mean_precip = attributes(dat_wrangled$precip_s)$`scaled:center`
sd_precip = attributes(dat_wrangled$precip_s)$`scaled:scale`

mean_aisp = unique(dat_wrangled$AISP_mean)


# 2) predict from posterior -------------------------------
# create new temp and precip grid (this would eventually be the Global pixel grid, perhaps). Currently, the numbers are just made up.

set.seed = 232323
new_dat = tibble(bio12_precip = runif(10, min(dat_wrangled$bio12_precip), max(dat_wrangled$bio12_precip)),
                 bio1_mat = runif(10, min(dat_wrangled$bio1_mat), max(dat_wrangled$bio1_mat))) %>% 
  mutate(year = "2024",
         site = "new", 
         author = "new") %>% 
  mutate(mat_deg_c = bio1_mat*0.1,  # recapture units in variables by adjusting for the scale: https://chelsa-climate.org/bioclim/
         precip_kg_m2 = bio12_precip*0.1,
         mat_deg_c_s = (mat_deg_c - mean_mat)/sd_mat, # convert to scaled units so model can use them
         precip_s = (precip_kg_m2 - mean_precip)/sd_precip) # conver to scaled units so model can use them

# predict new values of secondary production
post_preds = new_dat %>% 
  add_epred_draws(mod_posterior, re_formula = NULL, allow_new_levels = T) %>% 
  mutate(.epred = .epred*mean_aisp) %>%  # convert response back to mgAFDM/m2/year
  group_by(mat_deg_c_s, precip_kg_m2) %>% 
  mutate(site = cur_group_id()) # make a site id for plotting

# 3) Plot predictions ---------------------------------------------------------------

plot_preds = post_preds %>% 
  group_by(site) %>% 
  mutate(median = median(.epred)) %>% # median for adding color
  ggplot(aes(y = as.factor(site), x = .epred, fill = median)) + 
  stat_halfeye() +
  scale_x_log10() +
  theme_default() +
  labs(x = "Predicted Secondary Production\n(mgAFDM/m2/y)",
       y = "Hypothetical new sites",
       subtitle = "c) Posterior predictions of 2\u00b0 production at 10 new sites",
       fill = "Median 2\u00b0 production\n(mgAFDM/m2/y)")
  
# combine model and predictions in one plot
cond_plot = readRDS(file = "plots/cond_plot.rds")

# cond plot of mat
plot_dat_mat = cond_plot$mat_deg_c_s$data %>% 
  mutate(mat = (mat_deg_c_s*sd_mat) + mean_mat)

plot_mat = plot_dat_mat %>% 
  ggplot(aes(x = mat, y = estimate__*mean_aisp)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.3, aes(ymin = lower__*mean_aisp, 
                               ymax = upper__*mean_aisp)) +
  theme_default() +
  geom_point(data = dat, aes(x = mat_deg_c, y = AISP_mgAFDM_m2_y)) + 
  labs(y = "mgAFDM/m2/y",
       x = "Mean Annual Air Temp \u00b0C",
       subtitle = "a) Marginal relationship with temperature",
       caption = "(Data: Patrick et al. 2019)")


# cond plot of precip
plot_dat_precip = cond_plot$precip_s$data %>% 
  mutate(precip = precip_s*sd_precip + mean_precip)


plot_precip = plot_dat_precip %>% 
  ggplot(aes(x = precip, y = estimate__*mean_aisp)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.3, aes(ymin = lower__*mean_aisp, 
                               ymax = upper__*mean_aisp)) +
  theme_default() +
  geom_point(data = dat, aes(x = precip_kg_m2, y = AISP_mgAFDM_m2_y)) + 
  labs(y = "mgAFDM/m2/y",
       x = "Mean Annual Precipitation (kg/m2)",
       subtitle = "b) Marginal relationship with precipitation",
       caption = "(Data: Patrick et al. 2019)")


# combine
library(patchwork)

plot_model_predictions = plot_mat/plot_precip | plot_preds + guides(fill = "none")
ggview::ggview(plot_model_predictions, width = 9, height = 6)
ggsave(plot_model_predictions, width = 9, height = 6, file = "plots/plot_model_predictions.jpg")
