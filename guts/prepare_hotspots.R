# -------------------------------------
# Script:   prepare_hotspots.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# load necessary packages
library(readr)


# source necessary scripts
RSCRIPTDIR <- "guts"
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_hotspots_preparation.R"))


# read in districts
districts <- readr::read_rds(PATH_TO_DISTRICTS)


# accidents <- read_rds(file.path(ACCIDENTS_DIR, "accidents_40711.rds"))
# map <- read_rds(file.path(SF_MAPS_DIR, "district_40711.rds")) |>
#     sfnetworks::activate("edges") |>
#     sf::st_as_sf()
# lixels <- read_rds(file.path(LIXEL_MAPS_DIR, "lixel_40711.rds"))
# samples <- read_rds(file.path(LIXEL_MAPS_DIR, "lixel_sample_40711.rds"))
#
# library(spNetwork)
#
# # setting the multisession plan
# oplan <- future::plan()
# future::plan("multisession", workers = NO_OF_WORKERS)
#
# system.time(
#     densities <- spNetwork::nkde.mc(map,
#                          events = accidents,
#                          w = rep(1,nrow(accidents)),
#                          samples = samples,
#                          kernel_name = "quartic",
#                          bw = 300, div = "bw",
#                          # adaptive = TRUE,
#                          # trim_bw = 600,
#                          method = "discontinuous", digits = 1, tol = 1,
#                          grid_shape = c(10,10), max_depth = 10,
#                          agg = 5, #we aggregate events within a 5m radius (faster calculation)
#                          sparse = TRUE,
#                          verbose = TRUE)
# )
#
# future::plan(oplan)
#
# lixels$density <- densities
#
# library(tmap)
# tmap_mode("view")
#
# tm_shape(map) + tm_lines() +
#     tm_shape(lixels |> filter(density >= quantile(densities, 0.9))) +
#     tm_lines(col = "density")


if(is_behind(densities_file_name(districts, DENSITIES_DIR),
             c(sf_file_name(districts, SF_MAPS_DIR),
               lixel_file_name(districts, LIXEL_MAPS_DIR),
               lixel_sample_file_name(districts, LIXEL_MAPS_DIR),
               accidents_file_name(districts, ACCIDENTS_DIR),
               PATH_TO_DISTRICTS))) {
compute_densities(districts,
                  maps_dir = SF_MAPS_DIR,
                  lixel_dir = LIXEL_MAPS_DIR,
                  sample_dir = LIXEL_MAPS_DIR,
                  accidents_dir = ACCIDENTS_DIR,
                  density_dir = DENSITIES_DIR,
                  weights = NULL, bw = 300,
                  adaptive = FALSE, trim_bw = 600,
                  method = "discontinuous", agg = 1,
                  workers = NO_OF_WORKERS)
}
