#####################################################
## This file takes all the layer Rmd files and
## writes them into a single Rmd file: layers_all.Rmd.
## By M. Frazier, adapted by J. Lowndes
## https://github.com/OHI-Science/ohi-global/blob/draft/eez/conf/web/CombineLayers.R
#####################################################

## setwd if not already there
# setwd('region2017')

## load relevant libraries
library(tidyverse)

## delete layers_all.Rmd to start fresh
unlink('conf/web/layers_all.Rmd')

######################################################
### Rmd file header information
#######################################################
tmp <- capture.output(cat("---",
                          "\ntitle: Layers descriptions",
                          "\noutput:",
                          "\n  html_document:",
                          "\n    toc: true",
                          "\n    toc_depth: 1",
                          "\n    number_sections: false",
                          "\n    toc_float: yes",
                          "\n---"))

write(tmp, "conf/web/layers_all.Rmd")

######################################################
### Load libraries in Rmd
### and get master list of layers
#######################################################

tmp <- capture.output( cat(paste0("\n```{r, message=FALSE, echo=FALSE, warning=FALSE, error=FALSE}"),
                           "\n",
                           "library(tidyverse)",
                           "\n",
                           "library(knitr)",
                           "\n",
                           "\n",
                           "layer_meta <- readr::read_csv('https://raw.githubusercontent.com/OHI-Science/whi/master/region2017/data_layers_table_wh.csv')",
                           "\n",
                           "layer_path <- 'https://github.com/OHI-Science/whi/tree/master/region2017/layers_whi'",
                           "\n",
                           "\n",
                           "\n```"))

write(tmp, "conf/web/layers_all.Rmd", append=TRUE)


######################################################
### Cycle through each layer and add to file
#######################################################

layer_path <- 'https://github.com/OHI-Science/whi/tree/master/region2017/layers_whi'
dir_scenario_gh <- "https://raw.githubusercontent.com/OHI-Science/whi/master/region2017/"

## make sure all the Rmd files are in there and no typos!
layers_Rmd <- list.files("conf/web/layers_all")
layers_Rmd <- layers_Rmd[grep(".Rmd", layers_Rmd)]
layers_Rmd <- gsub(".Rmd", "", layers_Rmd)

# ## join region2017/layers.csv with prep/data_layers.csv
# source(
#   "https://raw.githubusercontent.com/OHI-Science/whi/master/region2017/conf/web/join_layers_csvs.R")



### Grab each layer description and add to master Rmd file!

## read in information from layers.csv
layers_whi_csv  <- readr::read_csv(file.path(dir_scenario_gh, 'data_layers_table_wh.csv')) %>%
  dplyr::select(name = `Data Layer`, # previously renamed to `header_layer`
                layer   = Name,      # previously renamed to `layer_name`
                description  = `Brief Description`,
                reference    = Reference,
                targets    = Goal, # previously renamed to `Dimension`
                targets_sub= Subgoal,
                filename_prep  = File, # previously renamed to `filename`
                bib,
                bib2,
                url)

## now select/filter a bit
layers_whi <- layers_whi_csv %>%
  filter(!is.na(name)) %>% ## when above setdiffs are fixed, won't need this anymore
  select(name, layer, filename_prep, description) %>%
  arrange(name)

for (h in layers_whi$name){ # h="aquarium fishing"

  layer      <-  layers_whi$layer[layers_whi$name == h]
  filename   <-  layers_whi$filename_prep[layers_whi$name == h]
  layer_path <- 'https://github.com/OHI-Science/whi/tree/master/region2017/layers_whi'

  tmp <- capture.output( cat("\n",
                             paste0("\n# ", h),

                             paste0("\n####[", layer, "]", "(", file.path(layer_path, filename), ") {-}"),

                             paste0("\n```{r, echo=FALSE, results='hide'}\n
                                    x <- tempfile(fileext = 'Rmd')\n
                                    on.exit(unlink(x))\n
                                    download.file(", "\"",
                                    sprintf('https://raw.githubusercontent.com/OHI-Science/whi/master/region2017/conf/web/layers_all/%s.Rmd',

                                            layer), "\", x)\n```\n"),

                             paste0("\n```{r, child = x, echo=FALSE, results='asis'}"),
                             "\n",
                             "\n```",
                             "\n"
                             ))

  write(tmp, "conf/web/layers_all.Rmd", append=TRUE)
}
