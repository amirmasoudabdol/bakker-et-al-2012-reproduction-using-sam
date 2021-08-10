# Title     : Sep 29 Results for Generating SAM Paper Figures
# Objective : To generate and customize the figures for the paper
# Created by: amabdol
# Created on: 25/03/2021

library(tidyverse)
library(hrbrthemes)
library(sfthemes)
library(gganimate)
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
Publications_Summaries$sizeclass <- factor(Publications_Summaries$sizeclass, levels = c("S", "M", "L"), , labels = c("S; N = 10", "M; N = 20", "L; N = 40"))
Publications_Summaries$size <- factor(Publications_Summaries$size, levels = c("Small", "Large"))

NO_QRP_EggersTestEstimator <- read_csv("../sam-project/sam-results/bakker-et-al-2012-extended-no-qrp_EggersTestEstimator_Summaries_Stacked.csv")
NO_QRP_EggersTestEstimator <- NO_QRP_EggersTestEstimator %>%
  rowwise() %>%
  mutate(size = determine_size(as.numeric(experiment_parameters_n_obs))) %>%
  mutate(sizeclass = determine_size_class(experiment_parameters_n_obs)) %>%
  mutate(expr_name = "No QRP")
QRP_EggersTestEstimator <- read_csv("../sam-project/sam-results/bakker-et-al-2012-extended-qrp_EggersTestEstimator_Summaries_Stacked.csv")
QRP_EggersTestEstimator <- QRP_EggersTestEstimator %>%
  rowwise() %>%
  mutate(size = determine_size(as.numeric(experiment_parameters_n_obs))) %>%
  mutate(sizeclass = determine_size_class(experiment_parameters_n_obs)) %>%
  mutate(expr_name = "Bakker's")
AGG_QRP_EggersTestEstimator <- read_csv("../sam-project/sam-results/bakker-et-al-2012-extended-agg-qrp_EggersTestEstimator_Summaries_Stacked.csv")
AGG_QRP_EggersTestEstimator <- AGG_QRP_EggersTestEstimator %>%
  rowwise() %>%
  mutate(size = determine_size(as.numeric(experiment_parameters_n_obs))) %>%
  mutate(sizeclass = determine_size_class(as.numeric(experiment_parameters_n_obs))) %>%
  mutate(expr_name = "Aggressive")

EggersTestEstimator <- merge(QRP_EggersTestEstimator, NO_QRP_EggersTestEstimator, all = T)
EggersTestEstimator <- merge(EggersTestEstimator, AGG_QRP_EggersTestEstimator, all = T)
EggersTestEstimator$sizeclass <- factor(EggersTestEstimator$sizeclass, levels = c("S", "M", "L"), , labels = c("S; N = 10", "M; N = 20", "L; N = 40"))
EggersTestEstimator$size <- factor(EggersTestEstimator$size, levels = c("Small", "Large"))


# Figure 10

# pfs_plot <- Publications_Summaries %>%
#   filter(journal_parameters_max_pubs == 24) %>%
#   filter(sizeclass == "M; N = 20") %>%
#   # filter(experiment_parameters_test_strategy_alpha == 0.005) %>%
#   ggplot(., aes(x = experiment_parameters_data_strategy_measurements_means_2,
#                 y = mean_sig,
#                 color = factor(expr_name),
#                 linetype = factor(size))) +
#   geom_smooth(se = F, show.legend = F) +
#   coord_fixed(xlim=c(0., 1.), ylim = c(0., 1)) +
#   labs(y = "Proportion", x = "ES",
#        color = "QRP Procedure",
#        # subtitle = "Chance of Finding Sig.",
#        linetype = 'Size') +
#   scale_linetype_manual(values=c("dashed", "solid")) +
#   # scale_x_continuous(n.breaks = 2) +
#   scale_y_continuous(n.breaks = 3) +
#   facet_grid(experiment_parameters_test_strategy_alpha ~ journal_parameters_review_strategy_pub_bias_rate, labeller = labeller(experiment_parameters_test_strategy_alpha = alpha_labels, journal_parameters_review_strategy_pub_bias_rate = pub_labels)) +
#   theme_sf_light(font_family = "Nitti Grotesk") + scale_colour_ios_light(accessible = TRUE) +
#   theme(legend.position = "bottom") +
#   theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12))
# 
# pfs_plot
# ggsave("Figures/Figure-11-A.png", height = 20, width = 60, dpi = 320, unit = "cm", bg = "transparent")
# 
# ef_plot <- Publications_Summaries %>%
#   filter(journal_parameters_max_pubs == 24) %>%
#   # filter(sizeclass == "M; N = 20") %>%
#   # filter(experiment_parameters_test_strategy_alpha == 0.005) %>%
#   ggplot(., aes(x = experiment_parameters_data_strategy_measurements_means_2,
#                 y = mean_effect - experiment_parameters_data_strategy_measurements_means_2,
#                 color = factor(expr_name),
#                 # shape = factor(experiment_parameters_test_strategy_alpha),
#                 linetype = factor(size))) +
#   geom_smooth(se = F, show.legend = F) +
#   # coord_fixed(xlim=c(0., 1.), ylim = c(0., 2.5)) +
#   labs(y = "Bias", x = "ES",
#        color = "QRP Procedure",
#        linetype = 'Size') +
#   scale_linetype_manual(values=c("dashed", "solid")) +
#   # scale_x_continuous(n.breaks = 2) +
#   scale_y_continuous(n.breaks = 3) +
#   facet_grid(sizeclass ~ journal_parameters_review_strategy_pub_bias_rate, labeller = labeller(experiment_parameters_test_strategy_alpha = alpha_labels, journal_parameters_review_strategy_pub_bias_rate = pub_labels)) +
#   theme_sf_light(font_family = "Nitti Grotesk") + 
#   scale_colour_ios_light(accessible = TRUE) +
#   # scale_color_brewer(type = "seq", ) +
#   theme(legend.position = "bottom") +
#   theme(plot.background = element_rect(fill = "white")) +
#   theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12)) +
#   theme(legend.position = "none", 
#         legend.box = "horizontal",
#         legend.direction = "vertical",
#         legend.spacing = unit(0.0, "pt"),
#         legend.box.just = "top",
#         legend.justification = c(0, 1))
# 
# ef_plot
# ggsave("Figures/Figure-11-B.png", height = 20, width = 60, dpi = 320, unit = "cm", bg = "transparent")
# 
# egger_plot <- EggersTestEstimator %>%
#   filter(journal_parameters_max_pubs == 24) %>%
#   filter(sizeclass == "M; N = 20") %>%
#   # filter(experiment_parameters_test_strategy_alpha == 0.005) %>%
#   ggplot(., aes(x = experiment_parameters_data_strategy_measurements_means_2,
#                 y = mean_sig,
#                 color = factor(expr_name),
#                 linetype = factor(size))) +
#   geom_smooth(se = F, show.legend = T) +
#   coord_fixed(xlim=c(0., 1.), ylim = c(0., 1)) +
#   labs(y = "Egger's Test Power", x = "ES",
#        color = "QRP Procedure", linetype = 'Size') +
#   scale_linetype_manual(values=c("dashed", "solid")) +
#   # scale_x_continuous(n.breaks = 2) +
#   scale_y_continuous(n.breaks = 3) +
#   facet_grid(experiment_parameters_test_strategy_alpha ~ journal_parameters_review_strategy_pub_bias_rate, labeller = labeller(. = c(), journal_parameters_review_strategy_pub_bias_rate = pub_labels)) +
#   theme_sf_light(font_family = "Nitti Grotesk") + scale_colour_ios_light(accessible = TRUE) +
#   theme(legend.position = "bottom") +
#   theme(plot.background = element_rect(fill = "white")) +
#   theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12)) +
#   theme(legend.position = "none", 
#         legend.box = "horizontal",
#         legend.direction = "vertical",
#         legend.spacing = unit(0.0, "pt"),
#         legend.box.just = "top",
#         legend.justification = c(0, 1))
# 
# egger_plot
# ggsave("Figures/Figure-11-C.png", height = 20, width = 60, dpi = 320, unit = "cm", bg = "transparent")
# 
# 
# d <- pfs_plot / ef_plot / egger_plot
# d
# # 
# ggsave("Figures/Figure-11.png", height = 90, width = 180, dpi = 320, limitsize = FALSE, unit = "cm", bg = "transparent")
# 
# 
# pfs_plot <- Publications_Summaries %>%
#   filter(journal_parameters_max_pubs == 24) %>%
#   # filter(sizeclass == "M; N = 20") %>%
#   filter(expr_name == "Bakker's") %>%
#   # filter(experiment_parameters_test_strategy_alpha == 0.005) %>%
#   ggplot(., aes(x = experiment_parameters_data_strategy_measurements_means_2,
#                 y = mean_sig,
#                 color = factor(journal_parameters_review_strategy_pub_bias_rate),
#                 linetype = factor(size))
#          ) +
#   geom_smooth(se = F, show.legend = T) +
#   coord_fixed(xlim=c(0., 1.), ylim = c(0., 1)) +
#   labs(y = "Proportion", x = "ES",
#        color = "QRP Procedure",
#        # subtitle = "Chance of Finding Sig.",
#        linetype = 'Size') +
#   scale_color_brewer(type = "seq") +
#   # scale_linetype_manual(values=c("dashed", "solid")) +
#   scale_x_continuous(n.breaks = 3) +
#   scale_y_continuous(n.breaks = 3) +
#   facet_grid(sizeclass ~ experiment_parameters_test_strategy_alpha, 
#              labeller = labeller(experiment_parameters_test_strategy_alpha = alpha_labels, journal_parameters_review_strategy_pub_bias_rate = pub_labels)) +
#   theme_sf_light(font_family = "Nitti Grotesk") + # scale_colour_ios_light(accessible = TRUE) +
#   theme(legend.position = "bottom") +
#   theme(plot.background = element_rect(fill = "white")) +
#   theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12)) +
#   theme(legend.position = "none", 
#         legend.box = "horizontal",
#         legend.direction = "vertical",
#         legend.spacing = unit(0.0, "pt"),
#         legend.box.just = "top",
#         legend.justification = c(0, 1))
# 
# pfs_plot
# ggsave("Figures/Figure-11-A-Small.png", dpi = 320, unit = "cm", bg = "transparent")

ef_plot_no_qrp <- Publications_Summaries %>%
  filter(journal_parameters_max_pubs == 24) %>%
  # filter(sizeclass == "M; N = 20") %>%
  filter(expr_name == "No QRP") %>%
  ggplot(., aes(x = experiment_parameters_data_strategy_measurements_means_2,
                y = mean_effect - experiment_parameters_data_strategy_measurements_means_2,
                color = factor(journal_parameters_review_strategy_pub_bias_rate),
                # shape = as.factor(researcher_parameters_probability_of_being_a_hacker),
                linetype = factor(size))
         ) +
  geom_smooth(se = F, show.legend = T) +
  # geom_point(size = 4) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0, 2.75), ratio = .5) +
  labs(y = "Bias", x = "True Effect Size",
       color = "Pub. Bias Rate",
       linetype = 'Study Size',
       title = "Effect Size Bias",
       subtitle = "With No QRP"
       # subtitle = TeX("Under different $\\alpha$ regims.")
       ) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_color_brewer(type = "seq") +
  scale_x_continuous(n.breaks = 3) +
  scale_y_continuous(n.breaks = 6) +
  facet_grid(sizeclass ~ experiment_parameters_test_strategy_alpha, 
             labeller = labeller(experiment_parameters_test_strategy_alpha = alpha_labels, journal_parameters_review_strategy_pub_bias_rate = pub_labels)) +
  theme_sf_light(font_family = "Nitti Grotesk") + #scale_colour_ios_light(accessible = TRUE) +
  theme(legend.position = "bottom") +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12, margin = margin(-5, 0, 10, 0))) + 
  # theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.spacing = unit(0.0, "pt"),
        legend.box.just = "top",
        legend.justification = c(1, 0))

ef_plot_no_qrp
ggsave("Figures/Figure-11-B-Small_No_QRP.png", dpi = 320, unit = "cm", bg = "transparent")

ef_plot_bakker <- Publications_Summaries %>%
  filter(journal_parameters_max_pubs == 24) %>%
  # filter(sizeclass == "M; N = 20") %>%
  filter(expr_name == "Bakker's") %>%
  ggplot(., aes(x = experiment_parameters_data_strategy_measurements_means_2,
                y = mean_effect - experiment_parameters_data_strategy_measurements_means_2,
                color = factor(journal_parameters_review_strategy_pub_bias_rate),
                # shape = as.factor(researcher_parameters_probability_of_being_a_hacker),
                linetype = factor(size))
  ) +
  geom_smooth(se = F, show.legend = T) +
  # geom_point(size = 4) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0, 2.75), ratio = 0.5) +
  labs(y = "Bias", x = "True Effect Size",
       color = "Pub. Bias Rate",
       linetype = 'Study Size',
       title = "Effect Size Bias",
       subtitle = "With Bakker's QRP"
  ) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_color_brewer(type = "seq", palette = "Greens") +
  scale_x_continuous(n.breaks = 3) +
  scale_y_continuous(n.breaks = 6) +
  facet_grid(sizeclass ~ experiment_parameters_test_strategy_alpha, 
             labeller = labeller(experiment_parameters_test_strategy_alpha = alpha_labels, journal_parameters_review_strategy_pub_bias_rate = pub_labels)) +
  theme_sf_light(font_family = "Nitti Grotesk") + #scale_colour_ios_light(accessible = TRUE) +
  theme(legend.position = "bottom") +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12, margin = margin(-5, 0, 10, 0))) + 
  # theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.spacing = unit(0.0, "pt"),
        legend.box.just = "top",
        legend.justification = c(1, 0))

ef_plot_bakker
ggsave("Figures/Figure-11-B-Small_Bakker.png", dpi = 320, unit = "cm", bg = "transparent")

egger_plot_no_qrp <- EggersTestEstimator %>%
  filter(journal_parameters_max_pubs == 24) %>%
  filter(expr_name == "No QRP") %>%
  # filter(sizeclass == "M; N = 20") %>%
  # filter(experiment_parameters_test_strategy_alpha == 0.005) %>%
  ggplot(., aes(x = experiment_parameters_data_strategy_measurements_means_2,
                y = mean_sig,
                color = factor(journal_parameters_review_strategy_pub_bias_rate),
                linetype = factor(size))
  ) +
  geom_smooth(se = F, show.legend = T) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0., 1)) +
  labs(y = "Power", x = "True Effect Size",
       color = "Pub. Bias Rate",
       linetype = 'Study Size',
       title = TeX("Egger's Test, $\\alpha$ = 0.1"),
       subtitle = "With No QRP")+
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_color_brewer(type = "seq") +
  scale_x_continuous(n.breaks = 3) +
  scale_y_continuous(n.breaks = 3) +
  facet_grid(sizeclass ~ experiment_parameters_test_strategy_alpha + researcher_parameters_probability_of_being_a_hacker, 
             labeller = labeller(experiment_parameters_test_strategy_alpha = alpha_labels, journal_parameters_review_strategy_pub_bias_rate = pub_labels)) +
  theme_sf_light(font_family = "Nitti Grotesk") + #scale_colour_ios_light(accessible = TRUE) +
  theme(legend.position = "bottom") +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12, margin = margin(-5, 0, 10, 0))) + 
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.spacing = unit(0.0, "pt"),
        legend.box.just = "top",
        legend.justification = c(1, 0))

egger_plot_no_qrp
ggsave("Figures/Figure-11-C-Small_No_QRP.png", dpi = 320, unit = "cm", bg = "transparent")

egger_plot_bakker <- EggersTestEstimator %>%
  filter(journal_parameters_max_pubs == 24) %>%
  filter(expr_name == "Bakker's") %>%
  # filter(sizeclass == "M; N = 20") %>%
  # filter(experiment_parameters_test_strategy_alpha == 0.005) %>%
  ggplot(., aes(x = experiment_parameters_data_strategy_measurements_means_2,
                y = mean_sig,
                color = factor(journal_parameters_review_strategy_pub_bias_rate),
                linetype = factor(size))
  ) +
  geom_smooth(se = F, show.legend = T) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0., 1)) +
  labs(y = "Power", x = "True Effect Size",
       color = "Pub. Bias Rate",
       linetype = 'Study Size',
       title = TeX("Egger's Test, $\\alpha$ = 0.1"),
       subtitle = "With Bakker's QRP")+
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_color_brewer(type = "seq", palette = "Greens") +
  scale_x_continuous(n.breaks = 3) +
  scale_y_continuous(n.breaks = 3) +
  facet_grid(sizeclass ~ experiment_parameters_test_strategy_alpha + researcher_parameters_probability_of_being_a_hacker, 
             labeller = labeller(experiment_parameters_test_strategy_alpha = alpha_labels, journal_parameters_review_strategy_pub_bias_rate = pub_labels)) +
  theme_sf_light(font_family = "Nitti Grotesk") + #scale_colour_ios_light(accessible = TRUE) +
  theme(legend.position = "bottom") +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12, margin = margin(-5, 0, 10, 0))) + 
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.spacing = unit(0.0, "pt"),
        legend.box.just = "top",
        legend.justification = c(1, 0))

egger_plot_bakker
ggsave("Figures/Figure-11-C-Small_Bakker.png", dpi = 320, unit = "cm", bg = "transparent")

all <- (ef_plot_no_qrp + ef_plot_bakker) / (egger_plot_no_qrp + egger_plot_bakker) + plot_layout(guides = "collect")
all
ggsave("~/Desktop/out.png")
(ef_plot_no_qrp + ef_plot_bakker) + plot_layout(guides = "collect")


d <- (pfs_plot / egger_plot) | ef_plot / guide_area() + plot_layout(guides = 'collect')
d

d <- (pfs_plot / egger_plot) | ef_plot / guide_area() + plot_layout(guides = 'collect')
d

d <- ef_plot + egger_plot + plot_layout(guides = 'keep')
d

ggsave("Figures/Figure-11-Small.png", bg = "transparent")
ggsave("Figures/Figure-11-Small.tiff", bg = "transparent")







Publications_Summaries %>%
  filter(journal_parameters_max_pubs == 24) %>%
  # filter(sizeclass == "M; N = 20") %>%
  filter(researcher_parameters_probability_of_being_a_hacker == 1,
         expr_name == "Bakker's") %>%
  ggplot(., aes(x = experiment_parameters_data_strategy_measurements_means_2,
                y = mean_effect - experiment_parameters_data_strategy_measurements_means_2,
                color = factor(journal_parameters_review_strategy_pub_bias_rate),
                # shape = as.factor(researcher_parameters_probability_of_being_a_hacker),
                linetype = factor(size))
  ) +
  geom_smooth(se = F, show.legend = T) +
  # geom_point(size = 4) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0, 2.25)) +
  labs(y = "Bias", x = "True Effect Size",
       color = "Pub. Bias",
       linetype = 'Study Size',
       title = "Effect Size Bias"
       # subtitle = TeX("Under different $\\alpha$ regims.")
  ) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_color_brewer(type = "seq", palette = "Greens") +
  scale_x_continuous(n.breaks = 3) +
  scale_y_continuous(n.breaks = 5) +
  facet_grid(sizeclass ~ experiment_parameters_test_strategy_alpha, 
             labeller = labeller(experiment_parameters_test_strategy_alpha = alpha_labels, journal_parameters_review_strategy_pub_bias_rate = pub_labels)) +
  theme_sf_light(font_family = "Nitti Grotesk") + #scale_colour_ios_light(accessible = TRUE) +
  theme(legend.position = "bottom") +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12)) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.spacing = unit(0.0, "pt"),
        legend.box.just = "top",
        legend.justification = c(1, 0))


Publications_Summaries %>%
  filter(journal_parameters_max_pubs == 24) %>%
  filter(sizeclass == "M; N = 20") %>%
  # filter(experiment_parameters_test_strategy_alpha == 0.005) %>%
  ggplot(., aes(x = experiment_parameters_data_strategy_measurements_means_2,
                y = mean_effect - experiment_parameters_data_strategy_measurements_means_2,
                color = factor(researcher_parameters_probability_of_being_a_hacker),
                # shape = as.factor(researcher_parameters_probability_of_being_a_hacker),
                linetype = factor(size))
  ) +
  geom_smooth(se = F, show.legend = T) +
  # geom_point(size = 4) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0, 2.25)) +
  labs(y = "Bias", x = "True Effect Size",
       color = "Pub. Bias",
       linetype = 'Study Size',
       title = "Effect Size Bias"
       # subtitle = TeX("Under different $\\alpha$ regims.")
  ) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  # scale_color_brewer(type = "seq") +
  scale_x_continuous(n.breaks = 3) +
  scale_y_continuous(n.breaks = 5) +
  facet_grid(experiment_parameters_test_strategy_alpha ~ journal_parameters_review_strategy_pub_bias_rate, 
             labeller = labeller(experiment_parameters_test_strategy_alpha = alpha_labels, journal_parameters_review_strategy_pub_bias_rate = pub_labels)) +
  theme_sf_light(font_family = "Nitti Grotesk") + #scale_colour_ios_light(accessible = TRUE) +
  theme(legend.position = "bottom") +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16), plot.subtitle = element_text(size = 12)) +
  theme(axis.text.y = element_text(face = "bold")) +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.spacing = unit(0.0, "pt"),
        legend.box.just = "top",
        legend.justification = c(1, 0))



EggersTestEstimator %>%
  filter(journal_parameters_max_pubs == 24) %>%
  filter(sizeclass == "M; N = 20") %>%
  # filter(experiment_parameters_test_strategy_alpha == 0.005) %>%
  ggplot(., aes(x = experiment_parameters_data_strategy_measurements_means_2,
                y = mean_sig,
                color = factor(researcher_parameters_probability_of_being_a_hacker),
                # shape = as.factor(researcher_parameters_probability_of_being_a_hacker),
                linetype = factor(size))
  ) +
  geom_smooth(se = F, show.legend = T) +
  coord_fixed(xlim=c(0., 1.), ylim = c(0., 1)) +
  labs(y = "Power", x = "True Effect Size",
       color = "QRP Procedure", linetype = 'Study Size',
       title = TeX("Egger's Test, $\\alpha$ = 0.1"))+
  scale_linetype_manual(values=c("dashed", "solid")) +
  # scale_color_brewer(type = "seq") +
  scale_x_continuous(n.breaks = 3) +
  scale_y_continuous(n.breaks = 3) +
  facet_grid(experiment_parameters_test_strategy_alpha ~ journal_parameters_review_strategy_pub_bias_rate,  
             labeller = labeller(experiment_parameters_test_strategy_alpha = alpha_labels, journal_parameters_review_strategy_pub_bias_rate = pub_labels)) +
  theme_sf_light(font_family = "Nitti Grotesk") + #scale_colour_ios_light(accessible = TRUE) +
  theme(legend.position = "bottom") +
  theme(plot.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(size = 16, face = "bold"), plot.subtitle = element_text(size = 12)) +
  theme(legend.position = "bottom", 
        legend.box = "horizontal",
        legend.direction = "vertical",
        legend.spacing = unit(0.0, "pt"),
        legend.box.just = "top",
        legend.justification = c(1, 0))
