## calculate_scores.R

## This script calculates OHI scores with the `ohicore` package.
## - configure_toolbox.r ensures your files are properly configured for `ohicore`.
## - The `ohicore` function CalculateAll() calculates OHI scores.

## set working directory for all OHI calculations
#setwd("~/github/whi/region2017")

## run the configure_toolbox.r script to check configuration
source("configure_toolbox.r")

## calculate scenario scores
scores <- ohicore::CalculateAll(conf, layers)

## save scores as scores.csv
readr::write_csv(scores, 'scores.csv', na='')


## visualize scores ----

## Flower plots for each region ----
source('https://raw.githubusercontent.com/OHI-Science/arc/master/circle2016/plot_flower_local.R')

## to create flower plots with equal weighting for FIS/MAR: since this relies on the `fp_wildcaught_weigth*` csv that is also used for calculating it, we'll rename it temporarily
fp_real <- 'layers/fp_wildcaught_weight_mhi2017.csv'
fp_temp <- 'layers/fp_TEMP_COPY_wildcaught.csv'

## temporarily rename fp file and delete original
readr::read_csv(fp_real) %>%
  write_csv(fp_temp)
unlink(fp_real)

## temporarily change CON subgoal scores to NA (to reset, rerun CalculateAll and rewrite scores.csv)
scores$score[scores$goal == "CON"] <- NA
write.csv(scores, 'scores.csv', na='', row.names=FALSE)

## now plot
PlotFlower(assessment_name = "West Hawaiʻi Island")

## now reinstate original file
readr::read_csv(fp_temp) %>%
  write_csv(fp_real)
unlink(fp_temp)
## check to make sure neither fp_real nor fp_tmp is in the Git tab!!

