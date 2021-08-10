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

# Reading the Summarized Output
dataset <- fread("../sam-project/sam-results/bakker-et-al-2012_Summarized.csv")

unique(dataset$alpha)

dataset <- dataset %>%
  filter(n_reps == 5)

dataset <- dataset %>%
  mutate(tnobs = factor(tnobs, levels = c(5, 10, 20, 25, 50, 100)),
         is_hacked = factor(is_hacked, levels = c(0, 1), labels = c("No", "Yes")))

dataset$sizeclass <- factor(dataset$sizeclass, levels = c("S", "M", "L"), labels = c("S; N = 10", "M; N = 20", "L; N = 40"))
dataset$size <- factor(dataset$size, levels = c("Small", "Large"))
dataset$alpha <- factor(dataset$alpha, levels = c(5e-04, 5e-03, 5e-02), labels = c("0.0005", "0.005", "0.05"))

dataset %>%
  ggplot(., aes(x = tmean, y = mean_eff_diff,
                color = is_hacked,
                linetype = size)) +
  geom_line(show.legend = TRUE) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  facet_grid(sizeclass ~ alpha) +
  coord_fixed(xlim = c(0., 1.), ylim = c(0., 1.05)) +
  scale_x_continuous(n.breaks = 3) +
  scale_y_continuous(n.breaks = 3) +
  labs(x = "True Effect Size", y = "Bias",
       title = "Effect Size Bias",
       subtitle = TeX("Under different $\\alpha$ levels."),
       color = "QRP",
       linetype = "Study Size") +
  theme(axis.text.x = element_text(angle = 45, margin = margin(10))) +
  theme_sf_light(font_family = "Nitti Grotesk") +
  scale_colour_ios_light(accessible = T) +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12, margin = margin(-5, 0, 10, 0))) + 
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.spacing = unit(0.0, "pt"),
        legend.box.just = "left",
        legend.justification = c(1, 0))

ggsave("Figures/Figure-8.png", dpi = 600, bg = "transparent")
# ggsave("Figures/Figure-8.tiff", dpi = 600, bg = "transparent")


