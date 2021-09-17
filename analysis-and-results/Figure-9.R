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

unique(dataset$alpha)

# dataset <- dataset %>%
#   filter(n_reps == 5)

dataset <- dataset %>%
  mutate(tnobs = factor(tnobs, levels = c(5, 10, 20, 25, 50, 100)),
         is_hacked = factor(is_hacked, levels = c(0, 1), labels = c("No", "Yes")))

dataset$sizeclass <- factor(dataset$sizeclass, levels = c("S", "M", "L"), labels = c("S; N = 10", "M; N = 20", "L; N = 40"))
dataset$size <- factor(dataset$size, levels = c("Small", "Large"))
dataset$alpha <- factor(dataset$alpha, levels = c(5e-04, 5e-03, 5e-02), labels = c("0.0005", "0.005", "0.05"))

# alpha_labels <- c(`5e-04` = TeX("$\\alpha$ = 0.0005"), `0.005` = TeX("$\\alpha$ = 0.005"), `0.05` = TeX("$\\alpha$ = 0.05"))
alpha_labels <- c(`5e-04` = "0.0005", `0.005` = "0.005", `0.05` = "0.05")

dataset %>%
  ggplot(., aes(x = tmean, y = mean_eff_diff,
                color = is_hacked,
                linetype = size,
                shape = as.factor(n_reps))) +
  geom_line(show.legend = TRUE) +
  geom_point() +
  scale_linetype_manual(values = c("dashed", "solid")) +
  facet_grid(sizeclass ~ alpha, labeller = labeller(alpha = alpha_labels)) +
  coord_fixed(xlim = c(0., 1.), ylim = c(0., 1.)) +
  scale_y_continuous(n.breaks = 3) +
  labs(x = "True Effect Size", y = "Bias",
       title = "Influece of Number of Replications",
       subtitle = TeX("Under different $\\alpha$ regimes"),
       color = "QRP",
       shape = "# Replications",
       linetype = "Study Size") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(10))) +
  theme_sf_light(font_family = "Nitti Grotesk") +
  scale_colour_ios_light(accessible = T) +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12)) 

ggsave("Figures/Figure-9-Alpha-vs-Size.png", dpi = 600, bg = "transparent")

ggsave("Figures/Figure-9-Alpha-vs-Size.tiff", dpi = 600, bg = "transparent")

# ----- Experiment 
# 
# dataset %>%
#   ggplot(., aes(x = tmean, y = mean_eff_diff,
#                 color = as.factor(n_reps),
#                 linetype = size,
#                 shape = is_hacked)) +
#   geom_smooth(se = F, show.legend = TRUE) +
#   geom_point(size = 2) +
#   scale_linetype_manual(values = c("dashed", "solid")) +
#   facet_grid(sizeclass ~ alpha, labeller = labeller(alpha = alpha_labels)) +
#   coord_fixed(xlim = c(0., 1.), ylim = c(0., 1.)) +
#   scale_color_brewer(type = "seq") +
#   scale_y_continuous(n.breaks = 3) +
#   labs(x = "True Effect Size", y = "Bias",
#        title = "Influece of Number of Replications",
#        subtitle = TeX("Under different $\\alpha$ regimes"),
#        color = "QRP",
#        shape = "# Replications",
#        linetype = "Study Size") +
#   theme(axis.text.x = element_text(angle = 45, margin = margin(10))) +
#   theme_sf_light(font_family = "Nitti Grotesk") +
#   # scale_colour_ios_light(accessible = T) +
#   theme(plot.background = element_rect(fill = "white")) +
#   theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12)) 

dataset %>%
  filter(alpha == 0.05) %>%
  ggplot(., aes(x = tmean, y = mean_eff_diff,
                color = is_hacked,
                linetype = size#,
                # shape = as.factor(alpha)
        )) +
  geom_line(show.legend = TRUE, lwd = 0.75) +
  # geom_point(size = 1.75, alpha = 0.9) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  facet_grid(sizeclass ~ n_reps, labeller = labeller(alpha = alpha_labels)) +
  coord_fixed(xlim = c(0., 1.), ylim = c(0., 1.1)) +
  scale_y_continuous(n.breaks = 3) +
  scale_x_continuous(n.breaks = 3) +
  # scale_shape_manual(values = c(0, 1, 2)) +
  labs(x = "True Effect Size", y = "Bias",
       title = "Effect Size Bias",
       subtitle = TeX("Under different number of replications., $\\alpha$ = 0.05"),
       color = "QRP",
       shape = "Alpha",
       linetype = "Study Size") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(10))) +
  theme_sf_light(font_family = "Nitti Grotesk", font_size_class = "Large") +
  scale_colour_ios_light(accessible = T) +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12, margin = margin(-5, 0, 10, 0))) + 
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.spacing = unit(0.0, "pt"),
        legend.box.just = "top",
        legend.justification = c(1, 0))

ggsave("Figures/Figure-9-Reps-vs-Size.png", dpi = 600, bg = "transparent")
ggsave("Figures/Figure-9-Reps-vs-Size.tiff", dpi = 600, bg = "transparent")


dataset %>%
  ggplot(., aes(x = tmean, y = mean_eff_diff,
                shape = is_hacked,
                linetype = size,
                color = as.factor(n_reps))) +
  # geom_line(show.legend = TRUE) +
  geom_smooth(se = F, show.legend = T) +
  geom_point(size = 1.25, alpha = 0.75) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  facet_grid(sizeclass ~ alpha, labeller = labeller(alpha = alpha_labels)) +
  coord_fixed(xlim = c(0., 1.), ylim = c(0., 1.)) +
  scale_y_continuous(n.breaks = 3) +
  labs(x = "True Effect Size", y = "Bias",
       title = "Influece of Number of Replications",
       subtitle = TeX("Under different $\\alpha$ regimes"),
       color = "QRP",
       shape = "# Replications",
       linetype = "Study Size") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(10))) +
  theme_sf_light(font_family = "Nitti Grotesk") +
  # scale_colour_ios_light(accessible = T) +
  scale_color_brewer(type = "seq", palette = "Purples") +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12)) 

ggsave("Figures/Figure-9-Alpha-vs-Size-Seq.png", dpi = 600, bg = "transparent")

ggsave("Figures/Figure-9-Alpha-vs-Size-Seq.tiff", dpi = 600, bg = "transparent")
