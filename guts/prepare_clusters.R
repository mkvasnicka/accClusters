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

tmap_mode("view")


brno <- read_rds("guts/data/maps/district_40711.rds")
nbs <- read_rds("guts/data/lixels/lixel_nb_40711.osm")
lixels <- read_rds("guts/data/densities/densities_40711.osm")


treshold <- quantile(lixels$density, 0.99)


start <- which(lixels$density == max(lixels$density))[1]
no_of_steps <- 60

system.time(
    # cl <- make_cluster(lixels, nbs, start, treshold)
    # cl <- make_cluster(lixels, nbs, start, cluster_condition_density, treshold)
    # cl <- make_one_cluster(lixels, nbs, start, treshold)
    # cl <- make_one_cluster(lixels, nbs, start, treshold, no_of_steps)
    cls <- make_clusters(lixels, nbs, treshold, no_of_steps)
)

system.time(
    cls2 <- join_clusters(cls)
)

clstrs <- cls2 |>
    purrr::map(~tibble(lineID = unlist(.))) |>
    dplyr::bind_rows(.id = "cluster")
clstrs <- left_join(lixels, clstrs, by = "lineID") |>
    filter(!is.na(cluster))


tm_shape(brno |> activate("edges") |> st_as_sf()) + tm_lines() +
    # tm_shape(lixels[cl, ]) + tm_lines(col = "red", lwd = 2)
    tm_shape(clstrs) + tm_lines(col = "cluster", lwd = 2)
