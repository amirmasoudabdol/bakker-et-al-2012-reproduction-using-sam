# Title     : Comparison of the results from the Original Study with SAM's Replication
# Objective : In this file, we are creating the Figure 7 of the paper
# Created by: Amir Masoud Abdol
# Created on: Aug 3, 2021

library(tidyverse)
library(ggthemes)
library(showtext)
library(data.table)
library(latex2exp)
library(hrbrthemes)
library(patchwork)
library(sfthemes)

# Reading the Summarized Output
dataset <- fread("../sam-project/sam-results/bakker-et-al-2012_Summarized.csv")

dataset <- dataset %>%
  filter(alpha == 0.05,
         n_reps == 5)

dataset <- dataset %>%
  mutate(tnobs = factor(tnobs, levels = c(5, 10, 20, 25, 50, 100)),
         is_hacked = factor(is_hacked, levels = c(0, 1), labels = c("No", "Yes")))

dataset$sizeclass <- factor(dataset$sizeclass, levels = c("S", "M", "L"), labels = c("S; N = 10", "M; N = 20", "L; N = 40"))
dataset$size <- factor(dataset$size, levels = c("Small", "Large"))

# Filter for alpha = 0.05, and n_reps = c(1, 5) to capture the original setting
# Note that the Summarized file contains enough information to produce Figure 7, 8, 9
# as well; however, here, we are only producing the figure 7.




# COMPARE TO THE FIXED ONE

bakker_fixed_ef <- read.csv("../r-project/bug-fixed-code-and-results/Figure 2/fixed_effect_size_bias_dataset.csv", stringsAsFactors = T)
bakker_fixed_ef$sizeclass <- factor(bakker_fixed_ef$sizeclass, levels = c("S", "M", "L"), labels = c("S; N = 10", "M; N = 20", "L; N = 40"))
bakker_fixed_pfs <- read.csv("../r-project/bug-fixed-code-and-results/Figure 2/fixed_proportion_of_sig_results_dataset.csv", stringsAsFactors = T)
bakker_fixed_pfs$sizeclass <- factor(bakker_fixed_pfs$sizeclass, levels = c("S", "M", "L"), labels = c("S; N = 10", "M; N = 20", "L; N = 40"))

fixed_df <- bakker_fixed_ef %>%
  mutate(tmean = es) %>%
  mutate(mean_eff_diff = value) %>%
  mutate(sigmean = bakker_fixed_pfs$value) %>%
  mutate(source = "Original") %>%
  mutate(is_hacked = factor(is_hacked, levels = c(FALSE, TRUE), labels = c("No", "Yes"))) %>%
  select(-value, -X, -col_code, -es)

combined_df <- dataset %>%
  mutate(source = 'Replication') %>%
  select(sizeclass, size, is_hacked, tmean, mean_eff_diff, sigmean, source)

combined_df <- rbind(combined_df, fixed_df)


ef_bias_plot <- combined_df %>%
  ggplot(., aes(x = tmean, y = mean_eff_diff,
                color = is_hacked,
                linetype = size,
                shape = source)) +
  geom_line(show.legend = TRUE) +
  geom_point(size = 1.25, alpha = .75) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_shape_manual(values=c(19, 29)) +
  scale_x_continuous(n.breaks = 3) +
  facet_grid(sizeclass ~ .) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0., 1.)) +
  labs(x = "True Effect Size", y = "Bias", title="Effect Size Bias",
       subtitle = TeX("$\\alpha = 0.05$"),
       color = "w/ QRP",
       shape = "Source",
       linetype="Study Size") +
  theme_sf_light(font_family = "Nitti Grotesk") +
  scale_colour_ios_light(accessible = T) +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16)) +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.spacing = unit(0.0, "pt"),
        legend.justification = c(0.85, 0))

ef_bias_plot

ggsave("Figures/Figure-7-Right-Panel.png", dpi = 600, bg = "transparent")
ggsave("Figures/Figure-7-Right-Panel.tiff", dpi = 600, bg = "transparent")

# -------------------------------------------------------

# bakker_fixed_pfs <- read.csv("marjan_2012_bakker_fixed_pfs.csv")
# bakker_fixed_pfs$sizeclass <- factor(bakker_fixed_pfs$sizeclass, levels = c("S", "M", "L"), labels = c("S; N = 10", "M; N = 20", "L; N = 40"))

fixed_df <- bakker_fixed_pfs %>%
  mutate(tmean = es) %>%
  mutate(sigmean = value) %>%
  mutate(mean_eff_diff = bakker_fixed_ef$value) %>%
  mutate(source = "Original") %>%
  mutate(is_hacked = factor(is_hacked, levels = c(FALSE, TRUE), labels = c("No", "Yes"))) %>%
  select(-value, -X, -es, -row)

combined_df <- dataset %>%
  mutate(source = 'Replication') %>%
  select(sizeclass, size, is_hacked, tmean, mean_eff_diff, sigmean, source)

combined_df <- rbind(combined_df, fixed_df)
combined_df$size <- factor(combined_df$size, levels = c("Small", "Large"))
# combined_df$is_hacked <- factor(combined_df$is_hacked, levels = c(FALSE, TRUE), labels = c("No", "Yes"))


pfs_bias_plot <- combined_df %>%
  ggplot(., aes(x = tmean, y = sigmean,
                color = is_hacked,
                linetype = size,
                shape = source)) +
  geom_line(show.legend = F) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  geom_point(size = 1.25, alpha = .75) +
  scale_shape_manual(values=c(19, 29)) +
  facet_grid(sizeclass ~ .) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0., 1.)) +
  scale_x_continuous(n.breaks = 3) +
  labs(x = "True Effect Size", y = "Proportion", title="Chance of Finding Sig.",
       subtitle = TeX("$\\alpha = 0.05$"),
       color = "w/ QRP",
       shape = "Source",
       linetype="Study Size") +
  theme_sf_light(font_family = "Nitti Grotesk") +
  scale_colour_ios_light(accessible = T) +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16)) +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.spacing = unit(0.0, "pt"),
        legend.box.just = "left",
        legend.justification = c(0.85, 0))

pfs_bias_plot

ggsave("Figures/Figure-7-Left-Panel.png", bg = "transparent", dpi = 600)  
ggsave("Figures/Figure-7-Left-Panel.tiff", bg = "transparent", dpi = 600)

pfs_bias_plot + ef_bias_plot + plot_layout(guides = 'collect')

ggsave("Figures/Figure-7.png", bg = "transparent", dpi = 600)
ggsave("Figures/Figure-7.tiff", bg = "transparent", dpi = 600)


# Comparison with the Original Results
# As we mentioned in the article, we discovered a bug in Bakker et al., 2012 paper
# and the following figure demonstrate the discrepancy between the replication and
# the published results, which contains a minor bug.

bakker_original_ef <- read.csv("../r-project/original-code-and-results/Figure 2/original_effect_size_bias_dataset.csv", stringsAsFactors = T)
bakker_original_ef$sizeclass <- factor(bakker_original_ef$sizeclass, levels = c("S", "M", "L"), labels = c("S; N = 10", "M; N = 20", "L; N = 40"))
bakker_original_pfs <- read.csv("../r-project/original-code-and-results/Figure 2/original_proportion_of_sig_results_dataset.csv", stringsAsFactors = T)
bakker_original_pfs$sizeclass <- factor(bakker_original_pfs$sizeclass, levels = c("S", "M", "L"), labels = c("S; N = 10", "M; N = 20", "L; N = 40"))

fixed_df <- bakker_original_ef %>%
  mutate(tmean = es) %>%
  mutate(mean_eff_diff = value) %>%
  mutate(sigmean = bakker_original_pfs$value) %>%
  mutate(source = "Original") %>%
  mutate(is_hacked = factor(is_hacked, levels = c(FALSE, TRUE), labels = c("No", "Yes"))) %>%
  select(-value, -X, -col_code, -es)

combined_df <- dataset %>%
  mutate(source = 'Replication') %>%
  select(sizeclass, size, is_hacked, tmean, mean_eff_diff, sigmean, source)

combined_df <- rbind(combined_df, fixed_df)


ef_bias_plot <- combined_df %>%
  ggplot(., aes(x = tmean, y = mean_eff_diff,
                color = is_hacked,
                linetype = size,
                shape = source)) +
  geom_line(show.legend = TRUE) +
  geom_point(size = 1.25, alpha = .75) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_shape_manual(values=c(19, 29)) +
  scale_x_continuous(n.breaks = 3) +
  facet_grid(sizeclass ~ .) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0., 1.)) +
  labs(x = "True Effect Size", y = "Bias", title="Effect Size Bias",
       subtitle = TeX("$\\alpha = 0.05$"),
       color = "w/ QRP",
       shape = "Source",
       linetype="Study Size") +
  theme_sf_light(font_family = "Nitti Grotesk") +
  scale_colour_ios_light(accessible = T) +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16)) +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.spacing = unit(0.0, "pt"),
        legend.justification = c(0.85, 0))

ef_bias_plot

ggsave("Figures/S2-Original-vs-Reproduction-Right-Panel.png", dpi = 600, bg = "transparent")
ggsave("Figures/S2-Original-vs-Reproduction-Right-Panel.tiff", dpi = 600, bg = "transparent")

# -------------------------------------------------------

# bakker_original_pfs <- read.csv("marjan_2012_bakker_original_pfs.csv")
# bakker_original_pfs$sizeclass <- factor(bakker_original_pfs$sizeclass, levels = c("S", "M", "L"), labels = c("S; N = 10", "M; N = 20", "L; N = 40"))

fixed_df <- bakker_original_pfs %>%
  mutate(tmean = es) %>%
  mutate(sigmean = value) %>%
  mutate(mean_eff_diff = bakker_original_ef$value) %>%
  mutate(source = "Original") %>%
  mutate(is_hacked = factor(is_hacked, levels = c(FALSE, TRUE), labels = c("No", "Yes"))) %>%
  select(-value, -X, -es, -row)

combined_df <- dataset %>%
  mutate(source = 'Replication') %>%
  select(sizeclass, size, is_hacked, tmean, mean_eff_diff, sigmean, source)

combined_df <- rbind(combined_df, fixed_df)
combined_df$size <- factor(combined_df$size, levels = c("Small", "Large"))
# combined_df$is_hacked <- factor(combined_df$is_hacked, levels = c(FALSE, TRUE), labels = c("No", "Yes"))


pfs_bias_plot <- combined_df %>%
  ggplot(., aes(x = tmean, y = sigmean,
                color = is_hacked,
                linetype = size,
                shape = source)) +
  geom_line(show.legend = F) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  geom_point(size = 1.25, alpha = .75) +
  scale_shape_manual(values=c(19, 29)) +
  facet_grid(sizeclass ~ .) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0., 1.)) +
  scale_x_continuous(n.breaks = 3) +
  labs(x = "True Effect Size", y = "Proportion", title="Chance of Finding Sig.",
       subtitle = TeX("$\\alpha = 0.05$"),
       color = "w/ QRP",
       shape = "Source",
       linetype="Study Size") +
  theme_sf_light(font_family = "Nitti Grotesk") +
  scale_colour_ios_light(accessible = T) +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16)) +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.spacing = unit(0.0, "pt"),
        legend.box.just = "left",
        legend.justification = c(0.85, 0))

pfs_bias_plot

ggsave("Figures/S2-Original-vs-Reproduction-Left-Panel.png", bg = "transparent", dpi = 600)  
ggsave("Figures/S2-Original-vs-Reproduction-Left-Panel.tiff", bg = "transparent", dpi = 600)

pfs_bias_plot + ef_bias_plot + plot_layout(guides = 'collect')

ggsave("Figures/S2-Original-vs-Reproduction-Joined.png", bg = "transparent", dpi = 600)
ggsave("Figures/S2-Original-vs-Reproduction-Joined.tiff", bg = "transparent", dpi = 600)