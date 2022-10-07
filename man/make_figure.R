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

library(tidyverse)
library(sf)
library(sfnetworks)
library(tmap)


# paths
districts_path <- "data/output/districts/districts.rds"
roads_path <- "data/tmp/maps/district_CZ0642.rds"
brno_clusters_path <- "data/output_brno/clusters/clusters_CZ0642_default_2019-01-01_2021-12-31.rds"


# read district of Brno
dbrno <- read_rds(districts_path) |>
    filter(district_name == "Brno-město")


# read roads in Brno
rbrno <- read_rds(roads_path) |>
    st_as_sf() |>
    st_intersection(st_geometry(dbrno))


# read Brno clusters
br <- readr::read_rds(brno_clusters_path)
cbrno <- br |>
    filter(severity == 5, additional_lixels == 5) |>
    pull(clusters) |>
    pluck(1)


# plot and save the figure
p <- tm_shape(dbrno) + tm_polygons(col = "blanchedalmond") +
    tm_shape(rbrno) + tm_lines(col = "azure4") +
    tm_shape(cbrno) + tm_lines(lwd = 4, col = "red")
tmap_save(p, filename = "title_fig.png",
          width = 160, height = 99, units = "mm", dpi = 600)
tmap_save(p, filename = "title_fig.pdf",
          width = 160, height = 99, units = "mm")
