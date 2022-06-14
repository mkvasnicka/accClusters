# -------------------------------------
# Script:   prepare_clusters.R
# Author:
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Corporation Name
# -------------------------------------

# load necessary packages
library(readr)
library(sf)
library(sfnetworks)
library(tmap)


# source necessary scripts
RSCRIPTDIR <- "guts"
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_cluster_preparation.R"))


# read in districts
districts <- readr::read_rds(PATH_TO_DISTRICTS)


tmap_mode("view")


# TEMP
districts <- districts |>
    dplyr::filter(stringr::str_detect(district_name, "Beroun"))


full_map <- read_rds(sf_file_name(districts, SF_MAPS_DIR))
lixels <- read_rds(densities_file_name(districts, DENSITIES_DIR))
nb <- read_rds(lixel_nb_file_name(districts, LIXEL_MAPS_DIR))
accidents <- read_rds(accidents_file_name(districts, ACCIDENTS_DIR))


threshold <- quantile(lixels$density, 0.995)
no_of_steps <- 0


system.time(
    cls <- compute_cluster_tibble(lixels, nb, threshold, no_of_steps)
)


system.time(
    clstrs <- graphic_clusters(lixels, accidents, cls, unit_costs = UNIT_COSTS)
)


cluster_pai(clstrs, accidents, lixels)
# treshold = 0.995 quantile, no_of_steps = 0 => PAI =
# treshold = 0.995 quantile, no_of_steps = 1 => PAI = 17.60684
# treshold = 0.995 quantile, no_of_steps = 5 => PAI = 14.50918
# treshold = 0.995 quantile, no_of_steps = 15 => PAI = 10.41518
# treshold = 0.995 quantile, no_of_steps = 30 => PAI = 7.20486
# treshold = 0.995 quantile, no_of_steps = 60 => PAI = 4.564866


system.time(
    acc <- add_clusters_to_accidents(accidents, cls)
)


tm_shape(full_map |> activate("edges") |> st_as_sf()) + tm_lines() +
    tm_shape(clstrs |> mutate(cluster = as.character(cluster))) +
    tm_lines(col = "cluster", lwd = 2) +
    tm_shape(acc |> filter(!is.na(cluster)) |>
                 mutate(cluster = as.character(cluster))) +
    tm_dots(col = "cluster")


tm_shape(clstrs) + tm_lines(col = "cost", lwd = 3)
tm_shape(clstrs) + tm_lines(col = "cost_per_meter", lwd = 3)
