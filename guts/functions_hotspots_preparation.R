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



# density computation ----------------------------------------------------------

# compute_one_time_densities() computes densities for one time window given by
# parameters from_date and to_date
#
# inputs:
# - districts ... (sf tibble) districts table
# - maps_dir ... (character scalar) path to folder where sfnetworks maps of
#   roads in individual districts are stored
# - lixel_dir ... (character scalar) path to folder where where lixelized roads
#   in individual districts are stored
# - sample_dir ... (character scalar) path to folder where samples of lixelized
#   roads in individual districts are stored
# - accidents_dir ... (character scalar) path to folder where accidents snapped
#   to roads in individual districts are stored
# - density_dir ... (character scalar) path to folder where computed densities
#   will be stored
# - from_date, to_date ... (Date scalars) describe date range of accidents that
#   are used
# - weights, bw, adaptive, trim_bw, method, agg ... parameters sent to
#   spNetwork::nkde()
# - workers ... (NULL or integer scalar) number of cores used in parallel
# - other_files ... (character vector) pathes to other files that can determine
#   whether existing files are up-to-date
#
# value:
#   none; data are written to disk
compute_one_time_densities <- function(districts,
                                       maps_dir, lixel_dir, sample_dir,
                                       accidents_dir,
                                       density_dir,
                                       from_date, to_date,
                                       weights = NULL, bw = 300,
                                       adaptive = FALSE, trim_bw = 600,
                                       method = "discontinuous", agg = 1,
                                       workers = NULL,
                                       other_files = NULL) {
  one_district <- function(map_path, lixel_path, sample_path, accidents_path,
                           density_path,
                           from_date, to_date,
                           weights, bw, adaptive, trim_bw, method, agg) {
    map <- readr::read_rds(map_path) |>
        sfnetworks::activate("edges") |>
        sf::st_as_sf()
    lixels <- readr::read_rds(lixel_path)
    samples <- readr::read_rds(sample_path)
    accidents <- readr::read_rds(accidents_path) |>
        dplyr::filter(p2a >= from_date, p2a <= to_date)
    if (!(is.null(weights) ||
          (is.character(weights) && length(weights) == 1 &&
           weights %in% names(accidents) && is.numeric(accidents[[weights]]))))
        stop("Weights must be eiter NULL or name of one numeric vector ",
             "in accidents.")
    if (is.null(weights))
        weights <- rep(1,nrow(accidents))
    if (is.character(weights))
        weights <- accidents[[weights]]
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

  from_date <- as.Date(from_date)
  to_date <- as.Date(to_date)
  stopifnot(from_date <= to_date)

  profile_name <- NULL
  if (exists("PROFILE_NAME"))
      profile_name <- PROFILE_NAME

  workers <- get_number_of_workers(workers)
  districts <- districts_behind(districts,
                                target_fun = densities_file_name,
                                source_fun = list(sf_file_name, lixel_file_name,
                                                  lixel_sample_file_name,
                                                  accidents_file_name),
                                target_folder = density_dir,
                                source_folder = list(maps_dir, lixel_dir,
                                                     sample_dir, accidents_dir),
                                other_files = other_files,
                                from_date = from_date, to_date = to_date,
                                profile_name = profile_name)
  tab <- tibble::tibble(
      map_path = sf_file_name(districts, maps_dir),
      lixel_path = lixel_file_name(districts, lixel_dir),
      sample_path = lixel_sample_file_name(districts, sample_dir),
      accidents_path = accidents_file_name(districts, accidents_dir),
      density_path = densities_file_name(districts, density_dir,
                                         from_date = from_date,
                                         to_date = to_date,
                                         profile_name = profile_name)
  )
  PWALK(tab, one_district, workers = workers,
        from_date, to_date,  # TODO: je toto nutné???
        weights = weights, bw = bw, adaptive = adaptive, trim_bw = trim_bw,
        method = method, agg = agg)
}


# compute_densities() computes densities for all time periods given by time
# window
#
# inputs:
# - districts ... (sf tibble) districts table
# - maps_dir ... (character scalar) path to folder where sfnetworks maps of
#   roads in individual districts are stored
# - lixel_dir ... (character scalar) path to folder where where lixelized roads
#   in individual districts are stored
# - sample_dir ... (character scalar) path to folder where samples of lixelized
#   roads in individual districts are stored
# - accidents_dir ... (character scalar) path to folder where accidents snapped
#   to roads in individual districts are stored
# - density_dir ... (character scalar) path to folder where computed densities
#   will be stored
# - path_to_time_window_file ... (character scalar) path to TSV file where time
#   windows are stored; see help for read_time_window_file()
# - weights, bw, adaptive, trim_bw, method, agg ... parameters sent to
#   spNetwork::nkde()
# - workers ... (NULL or integer scalar) number of cores used in parallel
# - other_files ... (character vector) pathes to other files that can determine
#   whether existing files are up-to-date
#
# value:
#   none; data are written to disk
compute_densities <- function(districts,
                              maps_dir, lixel_dir, sample_dir, accidents_dir,
                              density_dir,
                              path_to_time_window_file,
                              weights = NULL, bw = 300,
                              adaptive = FALSE, trim_bw = 600,
                              method = "discontinuous", agg = 1,
                              workers = NULL,
                              other_files = NULL) {
    time_window <- read_time_window_file(path_to_time_window_file)
    purrr::walk(seq_len(nrow(time_window)),
                ~compute_one_time_densities(districts = districts,
                                            maps_dir = maps_dir,
                                            lixel_dir = lixel_dir,
                                            sample_dir = sample_dir,
                                            accidents_dir = accidents_dir,
                                            density_dir = density_dir,
                                            from_date = time_window$from_date[.],
                                            to_date = time_window$to_date[.],
                                            weights = weights, bw = bw,
                                            adaptive = adaptive,
                                            trim_bw = trim_bw,
                                            method = method, agg = agg,
                                            workers = workers,
                                            other_files = other_files)
    )
}
