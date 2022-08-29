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

# load necessary packages
library(readr)


# supply path to RSCRIPTDIR if it was not supplied outside
if (!exists("RSCRIPTDIR")) RSCRIPTDIR <- "guts"


# source necessary scripts
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_densities_preparation.R"))


# process command-line parameters
process_command_line_arguments(RSCRIPTDIR)


# start logging
start_logging(log_dir())


# read in districts
districts <- readr::read_rds(path_to_districts())


# compute hotspots, i.e., NKDE
compute_densities(districts,
                  maps_dir = path_to_map_dir(),  # SF_MAPS_DIR,
                  lixel_dir = path_to_lixels_maps_dir(),  # LIXEL_MAPS_DIR,
                  sample_dir = path_to_lixels_maps_dir(),  # LIXEL_MAPS_DIR,
                  accidents_dir = path_to_accidents_dir(),  # ACCIDENTS_DIR,
                  density_dir = path_to_densities_dir(),  # DENSITIES_DIR,
                  time_window = TIME_WINDOW,
                  weights = NKDE_WEIGHTS,
                  bw = NKDE_BW,
                  adaptive = NKDE_ADAPTIVE,
                  trim_bw = NKDE_TRIM_BW,
                  method = NKDE_METHOD,
                  agg = NKDE_AGG,
                  workers = NO_OF_WORKERS,
                  other_files = path_to_districts())
