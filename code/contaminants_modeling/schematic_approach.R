library(tidyverse)
library(ggthemes)
library(brms)
library(paletteer)
library(scales)


a = brm_emerge_temp %>% 
  as_draws_df() %>% 
  mutate(b_Intercept = exp(b_Intercept)*773000*1e6) %>% 
  mutate(f_global_emergence = b_Intercept,
         f_emerge_with_contaminants = f_global_emergence*0.11,
         f_contaminant_effect_on_emergence = f_emerge_with_contaminants*0.5) %>%
  pivot_longer(cols = starts_with("f_")) %>%
  group_by(name) %>% 
  mutate(median = log(median(value))) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 100, aes(fill = name, 
                                 group = name,
                                 y = ..count../sum(..count..)), 
                 position = "identity",
                 color = "white") +
  scale_fill_manual(values = c("grey100", "grey100", "grey10")) +
  theme_void() + 
  theme(axis.line.x = element_line(),
        axis.ticks.x = element_line()) +
  scale_x_log10(labels = comma) +
  labs(y = "",
       x = "Global Emergence (mgDM/y)") +
  guides(fill = "none") 



b = brm_emerge_temp %>% 
  as_draws_df() %>% 
  mutate(b_Intercept = exp(b_Intercept)*773000*1e6) %>% 
  mutate(f_global_emergence = b_Intercept,
         f_emerge_with_contaminants = f_global_emergence*0.11,
         f_contaminant_effect_on_emergence = f_emerge_with_contaminants*0.5) %>%
  pivot_longer(cols = starts_with("f_")) %>%
  group_by(name) %>% 
  mutate(median = log(median(value))) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 100, aes(fill = name, 
                                 group = name,
                                 y = ..count../sum(..count..)), 
                 position = "identity",
                 color = "white") +
  scale_fill_manual(values = c("grey100", "grey10", "grey90")) +
  theme_void() + 
  theme(axis.line.x = element_line(),
        axis.ticks.x = element_line()) +
  scale_x_log10(labels = comma) +
  labs(y = "",
       x = "Global Emergence (mgDM/y)") +
  guides(fill = "none") 


c = brm_emerge_temp %>% 
  as_draws_df() %>% 
  mutate(b_Intercept = exp(b_Intercept)*773000*1e6) %>% 
  mutate(f_global_emergence = b_Intercept,
         f_emerge_with_contaminants = f_global_emergence*0.11,
         f_contaminant_effect_on_emergence = f_emerge_with_contaminants*0.5) %>%
  pivot_longer(cols = starts_with("f_")) %>%
  group_by(name) %>% 
  mutate(median = log(median(value))) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 100, aes(fill = name, 
                                 group = name,
                                 y = ..count../sum(..count..)), 
                 position = "identity",
                 color = "white") +
  scale_fill_manual(values = c("grey10", "grey90", "grey90")) +
  theme_void() + 
  theme(axis.line.x = element_line(),
        axis.ticks.x = element_line()) +
  scale_x_log10(labels = comma) +
  labs(y = "",
       x = "Global Emergence (mgDM/y)") +
  guides(fill = "none") 




library(patchwork)
library(ggview)

abc = a/b/c

ggview(abc, width = 4.5, height = 5)
ggsave(abc, width = 4.5, height = 5, file = "plots/conceptual.jpg", dpi = )



d = brm_emerge_temp %>% 
  as_draws_df() %>% 
  mutate(b_Intercept = exp(b_Intercept)*773000*1e6) %>% 
  mutate(f_global_emergence = b_Intercept,
         d_emerge_with_contaminants = f_global_emergence*0.11,
         f_contaminant_effect_on_emergence = d_emerge_with_contaminants*0.5) %>%
  pivot_longer(cols = starts_with("f_")) %>%
  group_by(name) %>% 
  mutate(median = log(median(value))) %>% 
  ggplot(aes(x = value)) + 
  geom_histogram(bins = 100, aes(fill = name, 
                                 group = name,
                                 alpha = name,
                                 # color = name,
                                 y = ..count../sum(..count..)), 
                 color = "white",
                 position = "identity") +
  scale_fill_manual(values = c("grey10", "grey100")) +
  # scale_color_manual(values = c("grey100", NA, NA)) +
  scale_alpha_manual(values = c(1, 0, 0)) +
  theme_void() + 
  theme(axis.line.x = element_line(),
        axis.ticks.x = element_line()) +
  scale_x_log10(labels = comma) +
  labs(y = "",
       x = "Global Emergence (mgDM/y)") +
  guides(fill = "none",
         alpha = "none") 

ggsave(d, file = "plots/conceptual_d.jpg", width = 4.5, height = 1.5)
