## load any libraries needed across website pages
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(knitr)
})

## brewed vars
study_area      <- "West Hawaiian Islands"
key             <- "whi"
dir_scenario_gh <- "https://raw.githubusercontent.com/OHI-Science/whi/master/region2017"

## knitr options for all webpages
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

## read in variables if they exist (i.e. don't try for prep repos)
scores_csv <- file.path(dir_scenario_gh, 'scores.csv')
layers_csv <- file.path(dir_scenario_gh, 'layers.csv')
conf_csv   <- file.path(dir_scenario_gh, 'conf/goals.csv')

if (RCurl::url.exists(scores_csv)) scores <- readr::read_csv(scores_csv)
if (RCurl::url.exists(layers_csv)) layers <- readr::read_csv(layers_csv)
if (RCurl::url.exists(conf_csv))   weight <- readr::read_csv(conf_csv) %>%
  select(goal, weight)

## save local copies of Rmds to knit-child ----

dir_raw_draft <- 'https://raw.githubusercontent.com/OHI-Science/whi/master'

to_copy <- c(# 'region2017/conf/web/goals.Rmd',
             'region2017/conf/web/layers_all.Rmd',
             'region2017/conf/web/layers_table.Rmd',
             'region2017/conf/web/OHI_Hawaii.bib')

for (f in to_copy) { # f <-  'region2017/conf/web/layers_all.Rmd'

  fp <- file.path(dir_raw_draft, f)

  ## if the url exists, save a copy.
  if (RCurl::url.exists(fp)) {
    f_web   <- readr::read_lines(fp)
    if ( tools::file_ext(fp) == 'Rmd' ) {
      f_local <- paste0('local_', basename(fp))
    } else {
      f_local <- basename(fp)
    }
    readr::write_lines(f_web, path = f_local, append = FALSE)
    message(sprintf('saving %s', f_local))
  } else {
    message(sprintf('%s does not exist', fp))
  }
}
