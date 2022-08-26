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
library(readr)


# supply path to RSCRIPTDIR if it was not supplied outside
if (!exists("RSCRIPTDIR")) RSCRIPTDIR <- "guts"


# source necessary scripts
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_cluster_preparation.R"))


# process command-line parameters
process_command_line_arguments(RSCRIPTDIR)


# start logging
start_logging(log_dir())


# read in districts
districts <- readr::read_rds(path_to_districts())


districts <- districts |>
    filter(district_name == "Brno-město")
from_date <- "2018-01-01"
to_date <- "2020-12-31"

densities_file <- densities_file_name(districts,
                                      from_date = from_date, to_date = to_date,
                                      folder = path_to_densities_dir())
lixel_nb_file <- lixel_nb_file_name(districts,
                                    folder = path_to_lixels_maps_dir())
accident_file <- accidents_file_name(districts,
                                     folder = path_to_accidents_dir())
cluster_min_quantile <- CLUSTER_MIN_QUANTILE
visual_min_quantile <- VISUAL_MIN_QUANTILE
cluster_steps <- CLUSTER_ADDITIONAL_STEPS

lixels <- readr::read_rds(densities_file)
nb <- readr::read_rds(lixel_nb_file)
accidents <- readr::read_rds(accident_file) |>
    filter(p2a >= from_date, p2a <= to_date)

threshold <- quantile(lixels$density, cluster_min_quantile)
visual_threshold <- quantile(lixels$density, visual_min_quantile)





cluster_pai <- function(cluster, accidents, lixels) {
    cluster_cost <- sum(cluster$total_density)
    cluster_length <- as.numeric(sum(cluster$total_length))
    total_cost <- sum(lixels$density)
    total_length <- as.numeric(sum(lixels$len))
    (cluster_cost / total_cost) / (cluster_length / total_length)

}



system.time(
    grid <- optimize_cluster_parameters(lixels, nb, accidents,
                                        threshold_range = seq(from = 0.9985,
                                                              to = 0.9999,
                                                              by = 0.0001),
                                        step_range = 1:10
    )
)

grid[1,]

library(ggplot2)
ggplot(grid, aes(no_of_steps, threshold_quantile, z= pai)) +
    geom_contour_filled() +
    scale_fill_grey(start = 1, end = 0) +
    theme_linedraw()


# library(tmap)
# tmap_mode("view")
#
# tm_shape(lixels) +
#     tm_lines(col = "density") +
#     tm_shape(cs) +
#     tm_lines(col = "tier", lwd = 3, breaks = seq(from = 0.5, to = 5.5, by = 1))
