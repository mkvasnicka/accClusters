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

library(readr)
library(sf)
library(sfnetworks)
library(tmap)


# source necessary scripts
RSCRIPTDIR <- "guts"
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_cluster_preparation.R"))


tmap_mode("view")


brno <- read_rds("guts/data/maps/district_40711.rds")
nbs <- read_rds("guts/data/lixels/lixel_nb_40711.rds")
lixels <- read_rds("guts/data/densities/densities_40711.rds")
accidents <- read_rds("guts/data/accidents/accidents_40711.rds")


threshold <- quantile(lixels$density, 0.995)
no_of_steps <- 30


system.time(
    cls <- compute_cluster_tibble(lixels, nbs, threshold, no_of_steps)
)


system.time(
    # clstrs <- add_clusters_to_lixels(lixels, cls) |>
    #     filter(!is.na(cluster)) |>
    #     group_by(cluster) |>
    #     summarise(total_length = sum(len),
    #               total_density = sum(density),
    #               geometry = st_union(geometry),
    #               .groups = "drop")
    clstrs <- graphic_clusters(lixels, accidents, cls, unit_costs = UNIT_COSTS)
)

system.time(
    acc <- add_clusters_to_accidents(accidents, cls)
)


tm_shape(brno |> activate("edges") |> st_as_sf()) + tm_lines() +
    tm_shape(clstrs |> mutate(cluster = as.character(cluster))) +
    tm_lines(col = "cluster", lwd = 2) +
    tm_shape(acc |> filter(!is.na(cluster)) |>
                 mutate(cluster = as.character(cluster))) +
    tm_dots(col = "cluster")


tm_shape(clstrs) + tm_lines(col = "cost", lwd = 3)
tm_shape(clstrs) + tm_lines(col = "cost_per_meter", lwd = 3)
