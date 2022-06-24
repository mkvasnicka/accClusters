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


# supply path to RSCRIPTDIR if it was not supplied outside
if (!exists("RSCRIPTDIR")) RSCRIPTDIR <- "guts"


# source necessary scripts
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_hotspots_preparation.R"))


# process command-line parameters
process_command_line_arguments(RSCRIPTDIR)


# read in districts
districts <- readr::read_rds(PATH_TO_DISTRICTS)


# compute hotspots, i.e., NKDE
compute_densities(districts,
                  maps_dir = SF_MAPS_DIR,
                  lixel_dir = LIXEL_MAPS_DIR,
                  sample_dir = LIXEL_MAPS_DIR,
                  accidents_dir = ACCIDENTS_DIR,
                  density_dir = DENSITIES_DIR,
                  # path_to_time_window_file = PATH_TO_TIME_WINDOW,
                  time_window = PATH_TO_TIME_WINDOW,
                  weights = NKDE_WEIGHTS,
                  bw = NKDE_BW,
                  adaptive = NKDE_ADAPTIVE,
                  trim_bw = NKDE_TRIM_BW,
                  method = NKDE_METHOD,
                  agg = NKDE_AGG,
                  workers = NO_OF_WORKERS,
                  other_files = PATH_TO_DISTRICTS)
