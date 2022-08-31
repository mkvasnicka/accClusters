# -------------------------------------
# Script:   prepare_densities.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# TEMP: remove
# supply path to RSCRIPTDIR if it was not supplied outside
if (!exists("RSCRIPTDIR")) RSCRIPTDIR <- "guts"
# supply path to folder where user stores her config and profile
if (!exists("DIR_ORIGIN")) DIR_ORIGIN <- "guts_data"


# source necessary scripts
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_densities_preparation.R"))


# read user config/profiles
profiles <- read_profiles()


# read in districts
districts <- read_districts()


# compute hotspots, i.e., NKDE
compute_densities(districts,
                  maps_dir = path_to_map_dir(),  # SF_MAPS_DIR,
                  lixel_dir = path_to_lixels_maps_dir(),  # LIXEL_MAPS_DIR,
                  sample_dir = path_to_lixels_maps_dir(),  # LIXEL_MAPS_DIR,
                  accidents_dir = path_to_accidents_dir(),  # ACCIDENTS_DIR,
                  density_dir = path_to_densities_dir(),  # DENSITIES_DIR,
                  profiles = profiles
                  # time_window = TIME_WINDOW,
                  # weights = NKDE_WEIGHTS,
                  # bw = NKDE_BW,
                  # adaptive = NKDE_ADAPTIVE,
                  # trim_bw = NKDE_TRIM_BW,
                  # method = NKDE_METHOD,
                  # agg = NKDE_AGG,
                  # workers = NO_OF_WORKERS
                  )
