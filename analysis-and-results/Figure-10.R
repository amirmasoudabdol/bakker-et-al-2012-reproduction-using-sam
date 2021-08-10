# Title     : Sep 29 Results for Generating SAM Paper Figures
# Objective : To generate and customize the figures for the paper
# Created by: amabdol
# Created on: 25/03/2021

library(tidyverse)
library(hrbrthemes)
library(sfthemes)
library(patchwork)

determine_size_class <- function(x) {
  if (x %in% c(5, 25)) return("S")
  if (x %in% c(10, 50)) return("M")
  if (x %in% c(20, 100)) return("L")
}

determine_size <- function(x) {
  if (x %in% c(5, 10, 20)) return("Small")
  if (x %in% c(25, 50, 100)) return("Large")
}

pub_labels <- c(`0` = "P. Bias = 0", `0.25` = "0.25", `0.5` = "0.5", `0.75` = "0.75", `0.9` = "0.9", `0.95` = "0.95", `0.99` = "0.99")
alpha_labels <- c(`5e-04` = "0.0005", `0.005` = "0.005", `0.05` = "0.05")

NO_QRP_Publications_Summaries <- read_csv("../sam-project/sam-results/bakker-et-al-2012-extended-no-qrp_Publications_Summaries_Stacked.csv")
NO_QRP_Publications_Summaries <- NO_QRP_Publications_Summaries %>%
  rowwise() %>%
  mutate(size = determine_size(as.numeric(experiment_parameters_n_obs))) %>%
  mutate(sizeclass = determine_size_class(experiment_parameters_n_obs)) %>%
  mutate(expr_name = "No QRP")
QRP_Publications_Summaries <- read_csv("../sam-project/sam-results/bakker-et-al-2012-extended-qrp_Publications_Summaries_Stacked.csv")
QRP_Publications_Summaries <- QRP_Publications_Summaries %>%
  rowwise() %>%
  mutate(size = determine_size(as.numeric(experiment_parameters_n_obs))) %>%
  mutate(sizeclass = determine_size_class(experiment_parameters_n_obs)) %>%
  mutate(expr_name = "Bakker's")
AGG_QRP_Publications_Summaries <- read_csv("../sam-project/sam-results/bakker-et-al-2012-extended-agg-qrp_Publications_Summaries_Stacked.csv")
AGG_QRP_Publications_Summaries <- AGG_QRP_Publications_Summaries %>%
  rowwise() %>%
  mutate(size = determine_size(as.numeric(experiment_parameters_n_obs))) %>%
  mutate(sizeclass = determine_size_class(as.numeric(experiment_parameters_n_obs))) %>%
  mutate(expr_name = "Aggressive")

Publications_Summaries <- merge(QRP_Publications_Summaries, NO_QRP_Publications_Summaries, all = T)
Publications_Summaries <- merge(Publications_Summaries, AGG_QRP_Publications_Summaries, all = T)
Publications_Summaries$sizeclass <- factor(Publications_Summaries$sizeclass, levels = c("S", "M", "L"), labels = c("S; N = 10", "M; N = 20", "L; N = 40"))
Publications_Summaries$size <- factor(Publications_Summaries$size, levels = c("Small", "Large"))
Publications_Summaries$expr_name <- factor(Publications_Summaries$expr_name, levels = c("No QRP", "Bakker's", "Aggressive"))

# -------------------------------------
# Figure 9 - Aggressive QRPs vs Alpha


Publications_Summaries %>%
  filter(journal_parameters_review_strategy_pub_bias_rate == 0) %>%
  filter(journal_parameters_max_pubs == 24) %>%
  ggplot(., aes(x = experiment_parameters_data_strategy_measurements_means_2,
                y = mean_effect - experiment_parameters_data_strategy_measurements_means_2,
                color = factor(expr_name),
                linetype = factor(size))) +
  geom_smooth(show.legend = T, se = F) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_x_continuous(n.breaks = 3) +
  scale_y_continuous(n.breaks = 3) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0., 1.5), ratio = 0.75) +
  labs(title = "Effect Size Bias", 
       subtitle = "Under the influence of more aggressive set of QRPs.",
       y = "Bias", x = "True Effect Size",
       linetype = 'Study Size',
       color = "QRP") +
  facet_grid(sizeclass ~ experiment_parameters_test_strategy_alpha, labeller = labeller(experiment_parameters_test_strategy_alpha = alpha_labels)) +
  theme_sf_light(font_family = "Nitti Grotesk") + 
  scale_colour_ios_light(accessible = T) +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12, margin = margin(-5, 0, 10, 0))) + 
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.spacing = unit(0.0, "pt"),
        legend.box.just = "top",
        legend.justification = c(1, 0))

ggsave("Figures/Figure-10-Bias-vs-Alpha.png", dpi = 600, bg = "transparent")
ggsave("Figures/Figure-10-Bias-vs-Alpha.tiff", dpi = 600, bg = "transparent")

