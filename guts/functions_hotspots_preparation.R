# -------------------------------------
# Script:   functions_hotspots_preparation.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

require(spNetwork)
require(readr)
require(tibble)


compute_densities <- function(districts,
                              maps_dir, lixel_dir, sample_dir, accidents_dir,
                              density_dir,
                              weights = NULL, bw = 300,
                              adaptive = FALSE, trim_bw = 600,
                              method = "discontinuous", agg = 1,
                              workers = NULL,
                              other_files = NULL) {
  one_district <- function(map_path, lixel_path, sample_path, accidents_path,
                           density_path,
                           weights, bw, adaptive, trim_bw, method, agg) {
    map <- readr::read_rds(map_path) |>
        sfnetworks::activate("edges") |>
        sf::st_as_sf()
    lixels <- readr::read_rds(lixel_path)
    samples <- readr::read_rds(sample_path)
    accidents <- readr::read_rds(accidents_path)
    if (is.null(weights))
        weights <- rep(1,nrow(accidents))
    densities <- spNetwork::nkde(map,
                                 events = accidents,
                                 w = weights,
                                 samples = samples,
                                 kernel_name = "quartic",
                                 bw = bw, div = "bw",
                                 adaptive = adaptive,
                                 trim_bw = trim_bw,
                                 method = method, digits = 1, tol = 1,
                                 grid_shape = c(10,10), max_depth = 10,
                                 agg = agg,
                                 sparse = TRUE,
                                 verbose = TRUE)
    lixels$density <- densities
    write_dir_rds(lixels, density_path)
  }

  workers <- get_number_of_workers(workers)
  districts <- districts_behind(districts,
                                target_fun = densities_file_name,
                                source_fun = list(sf_file_name, lixel_file_name,
                                                  lixel_sample_file_name,
                                                  accidents_file_name),
                                target_folder = density_dir,
                                source_folder = list(maps_dir, lixel_dir,
                                                     sample_dir, accidents_dir),
                                other_files = other_files)
  tab <- tibble::tibble(
      map_path = sf_file_name(districts, maps_dir),
      lixel_path = lixel_file_name(districts, lixel_dir),
      sample_path = lixel_sample_file_name(districts, sample_dir),
      accidents_path = accidents_file_name(districts, accidents_dir),
      density_path = densities_file_name(districts, density_dir)
  )
  PWALK(tab, one_district, workers = get_number_of_workers(workers),
        weights = weights, bw = bw, adaptive = adaptive, trim_bw = trim_bw,
        method = method, agg = agg)
}
