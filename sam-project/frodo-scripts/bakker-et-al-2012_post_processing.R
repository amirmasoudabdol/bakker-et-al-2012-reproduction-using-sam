# Customize this file!
# The only reaason that this file exists is that RStudio has an
# issue with multiprocess plan and apparently it's not stable,
# so, I'm leaving it out to run it in parallel via the Makefile

suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(furrr)))
suppressWarnings(suppressMessages(library(data.table)))

plan(multicore)

project_path <- "outputs/"
project_name <- "bakker-et-al-2012"

# Currently I'm mainly processing prepared publications datasets as
# they contain most of the data that I need, but you can apply this
# script on other outputs as well, e.g., `stats`.
filenames <- list.files(project_path, pattern = "*_Publications_prepared.csv", full.names = TRUE)

determine_size_class <- function(x) {
  if (x %in% c(5, 25)) return("S")
  if (x %in% c(10, 50)) return("M")
  if (x %in% c(20, 100)) return("L")
}

determine_size <- function(x) {
  if (x %in% c(5, 10, 20)) return("Small")
  if (x %in% c(25, 50, 100)) return("Large")
}

# Reading and summarizing each file
summarize_each_file <- function(fname) {

  df <- fread(fname)
  
  # You can use this snippet as an example of how to summarize your data for
  # later post-processing and plotting.
  df %>% 
    mutate(sizeclass = determine_size_class(as.numeric(tnobs))) %>%
    mutate(size = determine_size(as.numeric(tnobs))) %>%
    mutate(nobs = nobs,
           tnobs = factor(tnobs),
           research_strategy = factor(researcher_parameters_research_strategy_name),
           is_hacked = factor(researcher_parameters_probability_of_being_a_hacker),
           tmean = experiment_parameters_data_strategy_measurements_means_2,
           alpha = experiment_parameters_test_strategy_alpha,
           n_reps = experiment_parameters_n_reps,
           test_strategy = experiment_parameters_test_strategy_name,
           n_pos_sig = if_else(effect > 0 & sig, 1, 0)) %>%
    group_by(tmean, tnobs, n_reps, alpha, sizeclass, research_strategy, test_strategy, is_hacked) %>%
    summarize(sigmean = sum(n_pos_sig) / n(),
              mean_nobs = mean(nobs),
              mean_eff = mean(effect),
              mean_eff_diff = mean(effect) - head(tmean, 1),
              mean_pvalue = mean(pvalue),
              size = head(size, 1),
              .groups = 'drop') -> agg_df
  
  return(agg_df)
}


# Reading and summarizing all files in parallel
read_all_files <- function(fnames) {

  options(warn =  -1)
  
  tbl <-
    filenames %>%
    future_map_dfr(~summarize_each_file(.), .progress = TRUE)

  return(tbl)

}

df <- read_all_files(filenames)

# Removig the grouping columns
df <- data.frame(df)


write.csv(df, paste(project_path, project_name, "_Summarized.csv", sep=""), row.names = FALSE)
