## To update the layers information displayed on the website:
# Note: this is slightly simplified from the MHI version.

## setwd if not already there
#setwd('region2017')

## 1. Update prep/data_layers_table_wh.csv

## 2. Recreate any layer .Rmd files if necessary (to be saved in conf/web/layers_all/)
source("conf/web/create_layers_rmds.R")

## 3. Combine layer .Rmd files into layers_all.Rmd. Creates an object called "layers_whi"
source("conf/web/combine_layers_rmds.R")
# This does not rely on join_layers_csvs.R anymore!

##3.5 need to push to get layers to github!!! Otherwise the files are just local and we are loading them from github

## 4. Inspect layers_all.Rmd and knit

## 5. Commit and push

## 6. Switch to gh-pages branch, pull, reknit layers page,
#if made changed to layers_table.Rmd - need to update local_layers_table.Rmd with same changes to see changes displayed on website


## 7. conf/web/layers_table.Rmd will also use the individual Rmds assembled above, so don't forget to rerun that too!

