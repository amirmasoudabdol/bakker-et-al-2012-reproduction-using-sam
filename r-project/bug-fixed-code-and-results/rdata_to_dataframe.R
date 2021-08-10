# Title     : Marjan's Data → Dataframe
# Objective : Converting Marjan's CSV files to Dataframes
# Created by: amabdol
# Created on: 03/04/2020

library(tidyverse)
library(reshape2)
library(ggthemes)
library(showtext)

pProp <- read.csv("Figure 2/pProp.csv", sep  = ";")

colnames(pProp) <- c("x",
                     "S_Large_noQRP",
                     "S_Small_noQRP",
                     "S_Large_QRP",
                     "S_Small_QRP",
                     "M_Large_noQRP",
                     "M_Small_noQRP",
                     "M_Large_QRP",
                     "M_Small_QRP",
                     "L_Large_noQRP",
                     "L_Small_noQRP",
                     "L_Large_QRP",
                     "L_Small_QRP")

colname <- function(x) {
  if (x == 2) return("S_Large_noQRP")
  if (x == 3) return("S_Small_noQRP")
  if (x == 4) return("S_Large_QRP")
  if (x == 5) return("S_Small_QRP")
  if (x == 6) return("M_Large_noQRP")
  if (x == 7) return("M_Small_noQRP")
  if (x == 8) return("M_Large_QRP")
  if (x == 9) return("M_Small_QRP")
  if (x == 10) return("L_Large_noQRP")
  if (x == 11) return("L_Small_noQRP")
  if (x == 12) return("L_Large_QRP")
  if (x == 13) return("L_Small_QRP")
}

sizeclass <- function(x) {
  if (x == 2) return("S")
  if (x == 3) return("S")
  if (x == 4) return("S")
  if (x == 5) return("S")
  if (x == 6) return("M")
  if (x == 7) return("M")
  if (x == 8) return("M")
  if (x == 9) return("M")
  if (x == 10) return("L")
  if (x == 11) return("L")
  if (x == 12) return("L")
  if (x == 13) return("L")
}

size <- function(x) {
  if (x == 2) return("Large")
  if (x == 3) return("Large")
  if (x == 4) return("Small")
  if (x == 5) return("Small")
  if (x == 6) return("Large")
  if (x == 7) return("Large")
  if (x == 8) return("Small")
  if (x == 9) return("Small")
  if (x == 10) return("Large")
  if (x == 11) return("Large")
  if (x == 12) return("Small")
  if (x == 13) return("Small")
}

ishacked <- function(x) {
  if (x == 2) return(FALSE)
  if (x == 3) return(TRUE)
  if (x == 4) return(FALSE)
  if (x == 5) return(TRUE)
  if (x == 6) return(FALSE)
  if (x == 7) return(TRUE)
  if (x == 8) return(FALSE)
  if (x == 9) return(TRUE)
  if (x == 10) return(FALSE)
  if (x == 11) return(TRUE)
  if (x == 12) return(FALSE)
  if (x == 13) return(TRUE)
}

es <- function(x) {
  return(seq(0, 1., 0.05)[x - 1])
}

meltedProp <- melt_delim("Figure 2/pProp.csv", ';')
meltedProp <- tail(meltedProp, -14)
cleanedProp <- meltedProp %>%
  select(-data_type) %>%
  mutate(col = as.integer(col)) %>%
  mutate(row = as.integer(row)) %>%
  filter(col != 1) %>%
  mutate(value = str_replace(value, ",", ".")) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(col_code = lapply(col, colname)) %>%
  mutate(size = lapply(col, size)) %>%
  mutate(sizeclass = lapply(col, sizeclass)) %>%
  mutate(is_hacked = lapply(col, ishacked)) %>%
  mutate(es = lapply(row, es)) %>%
  mutate(es = unlist(es), is_hacked = unlist(is_hacked), size = unlist(size), sizeclass = unlist(sizeclass)) %>%
  select(-col_code, -col)

# cleanedProp$value <- as.numeric(cleanedProp$value)

head(cleanedProp)

cleanedProp$sizeclass <- factor(cleanedProp$sizeclass, levels = c("S", "M", "L"))
cleanedProp$size <- factor(cleanedProp$size, levels = c("Small", "Large"))
cleanedProp$is_hacked <- factor(cleanedProp$is_hacked, levels = c(FALSE, TRUE))

ggplot(cleanedProp,
       aes(x = es, y = value,
           linetype = size,
           color = is_hacked)) +
  geom_line() +
  facet_grid(sizeclass ~ .) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_color_manual(values = c("black", "red")) +
  coord_fixed(ylim = c(0, 1), xlim=c(0., 1.)) +
  labs(x = "True Effect", y = "% of Significant Outcomes", title="Proportaion of Significance", color = "Hacked?", linetype="Study Size", parse=TRUE)

ggsave("Figure 2/fixed_dataset_proportion_of_sig_results.png")

write.csv(cleanedProp, "Figure 2/fixed_proportion_of_sig_results_dataset.csv")

# -------

load("Figure 2/esAr.dat")

size <- function(x) {
  if (x == 2) return("Large")
  if (x == 3) return("Large")
  if (x == 4) return("Small")
  if (x == 5) return("Small")
  if (x == 6) return("Large")
  if (x == 7) return("Large")
  if (x == 8) return("Small")
  if (x == 9) return("Small")
  if (x == 10) return("Large")
  if (x == 11) return("Large")
  if (x == 12) return("Small")
  if (x == 13) return("Small")
}

ishacked <- function(x) {
  if (x == 2) return(FALSE)
  if (x == 3) return(TRUE)
  if (x == 4) return(FALSE)
  if (x == 5) return(TRUE)
  if (x == 6) return(FALSE)
  if (x == 7) return(TRUE)
  if (x == 8) return(FALSE)
  if (x == 9) return(TRUE)
  if (x == 10) return(FALSE)
  if (x == 11) return(TRUE)
  if (x == 12) return(FALSE)
  if (x == 13) return(TRUE)
}

sizeclass <- function(x) {
  if (x == 2) return("S")
  if (x == 3) return("S")
  if (x == 4) return("S")
  if (x == 5) return("S")
  if (x == 6) return("M")
  if (x == 7) return("M")
  if (x == 8) return("M")
  if (x == 9) return("M")
  if (x == 10) return("L")
  if (x == 11) return("L")
  if (x == 12) return("L")
  if (x == 13) return("L")
}

es=seq(0,1,by=.05)
es_df <- data.frame("value" = NA,"col_code" = NA,"sizeclass" = NA,"size" = NA,"is_hacked" = NA,"es" = NA)
for (i in c(0, 1, 2)) {
  for (j in c(0, 1, 2, 3)) {
    
    df <- apply(esAr[i+1,,,j+1],1,mean) - es
    df <- data.frame(value = df)
    df <- df %>%
      mutate(col_code = j + i * 4 + 2) %>%
      mutate(sizeclass = lapply(col_code, sizeclass),
             size = lapply(col_code, size),
             is_hacked = lapply(col_code, ishacked)) %>%
      mutate(is_hacked = unlist(is_hacked), size = unlist(size), sizeclass = unlist(sizeclass)) %>%
      mutate(es = seq(0, 1, 0.05))
    
    write.csv(df, paste(i, "_", j, ".csv", sep = ""))
    
    es_df <- rbind(es_df, df)
    
  }
}

es_df <- tail(es_df, -1)

es_df$sizeclass <- factor(es_df$sizeclass, levels = c("S", "M", "L"))
es_df$size <- factor(es_df$size, levels = c("Small", "Large"))

ggplot(es_df,
       aes(x = es, y = value,
           color = is_hacked,
           linetype = size)) +
  geom_line() +
  facet_grid(sizeclass ~ .) +
  scale_linetype_manual(values=c("dashed", "solid")) +
  scale_color_manual(values = c("black", "red")) +
  coord_fixed(ylim = c(0, 1), xlim=c(0., 1.)) +
  labs(x = "True Effect", y = "Mean Effect Difference", title="Mean Effect Diff", color = "Hacked?", linetype="Study Size", parse=TRUE)


ggsave("Figure 2/fixed_dataset_effect_size_bias.png")

write.csv(es_df, "Figure 2/fixed_effect_size_bias_dataset.csv")

