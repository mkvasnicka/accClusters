# -------------------------------------
# Script:
# Author:
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Corporation Name
# -------------------------------------

library(readr)
library(tmap)
library(sf)

# non-continuus old/newer
no <- read_rds("guts_data/output/shiny/shiny_CZ0642_default_2018-01-01_2020-12-31.rds")
nn <- read_rds("guts_data/output/shiny/shiny_CZ0642_default_2019-01-01_2021-12-31.rds")

# continuus old/newer
co <- read_rds("guts_data/output/shiny/shiny_CZ0642_other_2018-01-01_2020-12-31.rds")
cn <- read_rds("guts_data/output/shiny/shiny_CZ0642_other_2019-01-01_2021-12-31.rds")

tmap_mode("view")
lwd = 5
tm_shape(no$cluster_statistics) + tm_lines(col = "cost", lwd = lwd) +
    tm_shape(nn$cluster_statistics) + tm_lines(col = "cost", lwd = lwd) +
    tm_shape(co$cluster_statistics) + tm_lines(col = "cost", lwd = lwd) +
    tm_shape(cn$cluster_statistics) + tm_lines(col = "cost", lwd = lwd)

