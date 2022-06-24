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

# load necessary packages
library(dplyr)
library(readr)
library(sf)
library(tmap)


simplify_clusters <- function(cluster_list) {
    cluster_list$lixels |>
        filter(!is.na(cluster)) |>
        left_join(cluster_list$cluster_statistics, by = "cluster") |>
        group_by(cluster) |>
        summarise(geometry = st_union(geometry)) |>
        left_join(cluster_list$cluster_statistics)
}


brno1921 <- read_rds("guts/shiny/shiny_40711_2019-01-01_2021-12-31.rds")
clusters1921 <- simplify_clusters(brno1921)
brno1820 <- read_rds("guts/shiny/shiny_40711_2018-01-01_2020-12-31.rds")
clusters1820 <- simplify_clusters(brno1820)


tmap_mode("view")


# metric comparison within one period
tm_shape(clusters1921, name = "cost") +
    tm_lines(lwd = 3, col = "cost", palette = "Oranges") +
    tm_shape(clusters1921, name = "cost/m") +
    tm_lines(lwd = 3, col = "cost_per_meter", palette = "Blues") +
    tm_shape(clusters1921, name = "density") +
    tm_lines(lwd = 3, col = "total_density", palette = "Greens")

cor(brno1921$cluster_statistics$cost,
    brno1921$cluster_statistics$total_density,
    method = "spearman")
cor(brno1921$cluster_statistics$cost,
    brno1921$cluster_statistics$cost_per_meter,
    method = "spearman")
cor(brno1921$cluster_statistics$total_density,
    brno1921$cluster_statistics$cost_per_meter,
    method = "spearman")


# comparison of two periods
tm_shape(clusters1921, name = "2019--2021") +
    tm_lines(lwd = 3, col = "cost", palette = "Oranges") +
    tm_shape(clusters1820, name = "2018--2020") +
    tm_lines(lwd = 3, col = "cost", palette = "Blues")



# tmap_mode("view")
#
#
# # TEMP
# # districts <- districts |> dplyr::filter(district_name == "Brno-město")
#
#
# full_map <- read_rds(sf_file_name(districts, SF_MAPS_DIR))
# lixels <- read_rds(densities_file_name(districts, DENSITIES_DIR,
#                                        from_date = "2019-01-01",
#                                        to_date = "2021-12-31"))
# nb <- read_rds(lixel_nb_file_name(districts, LIXEL_MAPS_DIR))
# accidents <- read_rds(accidents_file_name(districts, ACCIDENTS_DIR))
#
#
# threshold <- quantile(lixels$density, 0.995)
# no_of_steps <- 1
#
#
# system.time({
#     cls <- compute_cluster_tibble(lixels, nb, threshold, no_of_steps)
#     clss <- cluster_statistics(lixels, accidents, cls)
#     # clstrs <- graphic_clusters(lixels, accidents, cls, unit_costs = UNIT_COSTS)
#     # clstrs <- gr_clusters(lixels, accidents, cls)
#     clstrs <- graphic_clusters(lixels, cls, clss)
# })
#
#
# cluster_pai(clstrs, accidents, lixels)
# # treshold = 0.99 quantile, no_of_steps = 5 => PAI = NA
# # treshold = 0.99 quantile, no_of_steps = 20 => PAI = 7.985548
# # treshold = 0.995 quantile, no_of_steps = 0 => PAI = NA
# # treshold = 0.995 quantile, no_of_steps = 1 => PAI = 17.60684
# # treshold = 0.995 quantile, no_of_steps = 5 => PAI = 14.50918
# # treshold = 0.995 quantile, no_of_steps = 15 => PAI = 10.41518
# # treshold = 0.995 quantile, no_of_steps = 30 => PAI = 7.20486
# # treshold = 0.995 quantile, no_of_steps = 60 => PAI = 4.564866
#
#
# system.time(
#     opt_par <- optimize_cluster_parameters(lixels, nb, accidents)
# )
#
#
# system.time(
#     acc <- add_clusters_to_accidents(accidents, cls)
# )
#
#
# tm_shape(full_map |> activate("edges") |> st_as_sf()) + tm_lines() +
#     tm_shape(clstrs |> mutate(cluster = as.character(cluster))) +
#     tm_lines(col = "cluster", lwd = 2) +
#     tm_shape(acc |> filter(!is.na(cluster)) |>
#                  mutate(cluster = as.character(cluster))) +
#     tm_dots(col = "cluster")
#
#
# tm_shape(clstrs2) + tm_lines(col = "blue", lwd =5 ) + tm_shape(clstrs) + tm_lines(col = "red", lwd = 3)
# tm_shape(clstrs) + tm_lines(col = "cost", lwd = 3)
# tm_shape(clstrs) + tm_lines(col = "cost_per_meter", lwd = 3)
#
#
# densities_file = densities_file_name(districts, DENSITIES_DIR,
#                                      from_date = "2019-01-01",
#                                      to_date = "2021-12-31")
# lixel_nb_file = lixel_nb_file_name(districts, LIXEL_MAPS_DIR)
# accident_file = accidents_file_name(districts, ACCIDENTS_DIR)
# shiny_file = shiny_file_name(districts, SHINY_DIR,
#                              from_date = "2019-01-01",
#                              to_date = "2021-12-31")
#
#
# cluster_min_quantile <- CLUSTER_MIN_QUANTILE
# cluster_steps <- CLUSTER_ADDITIONAL_STEPS
# visual_min_quantile <- VISUAL_MIN_QUANTILE
# one_district(densities_file, lixel_nb_file, accident_file,
#              shiny_file,
#              cluster_min_quantile, cluster_steps,
#              visual_min_quantile)
#
