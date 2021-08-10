# Title     : Report Figures
# Objective : Generate publication ready figures showcasing the difference between SAM's and Bakker's Oringal Simulation (before the patch)
# Created by: amabdol
# Created on: Sep 25, 2020

library(tidyverse)
library(hrbrthemes)
library(sfthemes)
library(gganimate)
library(data.table)
library(latex2exp)
library(patchwork)


project_path <- "../sam-project/"

qrp_df <- fread(paste0(project_path, "bakker-et-al-2012-qrp/outputs/bakker-et-al-2012-qrp_Summarized.csv"))
no_qrp_df <- fread(paste0(project_path, "bakker-et-al-2012-no-qrp/outputs/bakker-et-al-2012-no-qrp_Summarized.csv"))

qrp_df <- data.frame(qrp_df)
no_qrp_df <- data.frame(no_qrp_df)
pubs_summ <- bind_rows(qrp_df, no_qrp_df)

pubs_summ <- pubs_summ %>%
  mutate(tnobs = factor(tnobs, levels = c(5, 10, 20, 25, 50, 100)),
         is_hacked = factor(is_hacked, levels = c(0, 1), labels = c("No", "Yes")))

pubs_summ$sizeclass <- factor(pubs_summ$sizeclass, levels = c("S", "M", "L"), , labels = c("S; N = 10", "M; N = 20", "L; N = 40"))
pubs_summ$size <- factor(pubs_summ$size, levels = c("Small", "Large"))


# Comparing ES Bias Between Bakker and SAM
# ========================================

bakker_fixed_ef <- read.csv("../r-project/bug-fixed-code-and-results/Figure 2/fixed_effect_size_bias_dataset.csv", stringsAsFactors = T)
bakker_fixed_ef$sizeclass <- factor(bakker_fixed_ef$sizeclass, levels = c("S", "M", "L"), labels = c("S; N = 10", "M; N = 20", "L; N = 40"))
bakker_fixed_pfs <- read.csv("../r-project/bug-fixed-code-and-results/Figure 2/fixed_proportion_of_sig_results_dataset.csv", stringsAsFactors = T)
bakker_fixed_pfs$sizeclass <- factor(bakker_fixed_pfs$sizeclass, levels = c("S", "M", "L"), labels = c("S; N = 10", "M; N = 20", "L; N = 40"))

orig_df <- bakker_fixed_ef %>%
  mutate(tmean = es) %>%
  mutate(mean_eff_diff = value) %>%
  mutate(sigmean = bakker_fixed_pfs$value) %>%
  mutate(source = "Bakker") %>%
  mutate(is_hacked = factor(is_hacked, levels = c(FALSE, TRUE), labels = c("No", "Yes"))) %>%
  select(-value, -X, -col_code, -es)


# Combining SAM and Bakker's Data
# -------------------------------
combined_df <- pubs_summ %>%
  mutate(source = 'SAM') %>%
  select(sizeclass, size, is_hacked, tmean, mean_eff_diff, sigmean, source)

combined_df <- rbind(combined_df, orig_df)

eff_plot_light <- combined_df %>%
  ggplot(., aes(x = tmean, y = mean_eff_diff,
                color = is_hacked,
                linetype = size,
                shape = source)) +
  geom_line(show.legend = TRUE) +
  geom_point(size = 3, alpha = .75) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_shape_manual(values=c(19, 30)) +
  geom_hline(yintercept=0.0, alpha = 0.25) +
  facet_grid(sizeclass ~ .) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0., 1.2)) +
  # scale_color_manual(values = c("black", "red")) +
  labs(x = "ES", y = "Bias",
       title="ES Bias",
       subtitle = "ɑ = 0.05",
       color = "Hacked?", 
       linetype="Study Size",
       parse=TRUE) +
  theme_sf_light(size_class = 'Medium') +
  scale_colour_ios_light(accessible = T)


eff_plot_dark <- combined_df %>%
  ggplot(., aes(x = tmean, y = mean_eff_diff,
                color = is_hacked,
                linetype = size,
                shape = source)) +
  geom_line(show.legend = TRUE) +
  geom_point(size = 3, alpha = .75) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_shape_manual(values=c(19, 30)) +
  geom_hline(yintercept=0.0, alpha = 0.25) +
  facet_grid(sizeclass ~ .) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0., 1.2)) +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "ES", y = "Bias",
       title="ES Bias",
       subtitle = "ɑ = 0.05",
       color = "Hacked?", 
       linetype="Study Size",
       parse=TRUE) +
  theme_sf_dark(size_class = 'Medium') +
  scale_colour_ios_dark(accessible = T)

# Comparing Chance of Finding Sig.
# ================================


orig_df <- bakker_fixed_pfs %>%
  mutate(tmean = es) %>%
  mutate(sigmean = value) %>%
  mutate(mean_eff_diff = bakker_fixed_ef$value) %>%
  mutate(source = "Bakker") %>%
  mutate(is_hacked = factor(is_hacked, levels = c(FALSE, TRUE), labels = c("No", "Yes"))) %>%
  select(-value, -X, -es, -row)

# Combining SAM and Bakker's Data
# -------------------------------
combined_df <- pubs_summ %>%
  mutate(source = 'SAM') %>%
  select(sizeclass, size, is_hacked, tmean, mean_eff_diff, sigmean, source)

combined_df <- rbind(combined_df, orig_df)
combined_df$size <- factor(combined_df$size, levels = c("Small", "Large"))

sig_plot_light <- combined_df %>%
  ggplot(., aes(x = tmean, y = sigmean,
                color = is_hacked,
                linetype = size,
                shape = source)) +
  geom_line(alpha = 0.7, show.legend = FALSE) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  geom_hline(yintercept=0.0, alpha = 0.25) +
  geom_point(size = 3, alpha = .75) +
  scale_shape_manual(values=c(19, 30)) +
  facet_grid(sizeclass ~ .) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0., 1.)) +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "ES", y = "Proportion",
       title=TeX("Chance of Finding Sig."),
       subtitle = "ɑ = 0.05",
       color = "Hacked?",
       linetype="Study Size",
       parse=TRUE) +
  theme_sf_light(size_class = 'Medium') +
  scale_colour_ios_light(accessible = T) +
  theme(legend.position = "none")

sig_plot_dark <- combined_df %>%
  ggplot(., aes(x = tmean, y = sigmean,
                color = is_hacked,
                linetype = size,
                shape = source)) +
  geom_line(alpha = 0.7, show.legend = FALSE) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  geom_hline(yintercept=0.0, alpha = 0.25) +
  geom_point(size = 3, alpha = .75) +
  scale_shape_manual(values=c(19, 30)) +
  facet_grid(sizeclass ~ .) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0., 1.)) +
  scale_color_manual(values = c("black", "red")) +
  labs(x = "ES", y = "Bias",
       title=TeX("Chance of Finding Sig."),
       subtitle = "ɑ = 0.05",
       color = "Hacked?",
       linetype="Study Size",
       parse=TRUE) +
  theme_sf_dark(size_class = 'Medium') +
  scale_colour_ios_dark(accessible = T) +
  theme(legend.position = "none")

patched_light <- sig_plot_light + eff_plot_light
ggsave("~/Desktop/Comparison_with_Original_Simulation_light.png", plot = patched_light, dpi = 320, height = 40, width = 40, unit = "cm", bg = "transparent")

patched_dark <- sig_plot_dark + eff_plot_dark
ggsave("~/Desktop/Comparison_with_Original_Simulation_dark.png", plot = patched_dark, dpi = 320, height = 40, width = 40, unit = "cm", bg = "transparent")
