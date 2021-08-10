# Customize this file!
# The only reaason that this file exists is that RStudio has an
# issue with multiprocess plan and apparently it's not stable,
# so, I'm leaving it out to run it in parallel via the Makefile

suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(furrr)))
suppressWarnings(suppressMessages(library(data.table)))

plan(multiprocess)

project_path <- "outputs/"
project_name <- "bakker-et-al-2012-extended"

# Currently I'm mainly processing prepared publications datasets as
# they contain most of the data that I need, but you can apply this
# script on other outputs as well, e.g., `stats`.
filenames <- list.files(project_path, pattern = "*_Publications_prepared.csv", full.names = TRUE)

# Reading and summarizing each file
summarize_each_file <- function(fname) {

  df <- fread(fname)
  
  # You can use this snippet as an example of how to summarize your data for
  # later post-processing and plotting.
  agg_df <- df %>% 
    mutate(tnobs = factor(tnobs),
           n_items = factor(experiment_parameters_data_strategy_n_items),
           n_categories = factor(experiment_parameters_data_strategy_n_categories),
           k = factor(researcher_parameters_pre_processing_methods_0_multipliers_0),
           is_pre_processed = factor(researcher_parameters_is_pre_processing),
           abilities = experiment_parameters_data_strategy_abilities_1,
           difficulties = experiment_parameters_data_strategy_difficulties_0) %>%
    group_by(abilities, k, n_categories, n_items, difficulties, tnobs, is_pre_processed) %>%
    summarize(sig_mean = mean(sig),
              pval_mean = mean(pvalue),
              nobs_mean = mean(nobs)
    )
  
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
