# -------------------------------------
# Script:   prepare_clusters.R
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
source(file.path(RSCRIPTDIR, "functions_cluster_preparation.R"))


# process command-line parameters
process_command_line_arguments(RSCRIPTDIR)


# read in districts
districts <- readr::read_rds(path_to_districts())


# compute and save clusters for all periods
compute_clusters(districts,
                 densities_dir = path_to_densities_dir(),  # DENSITIES_DIR,
                 lixel_maps_dir = path_to_lixels_maps_dir(),  # LIXEL_MAPS_DIR,
                 accidents_dir = path_to_accidents_dir(),  # ACCIDENTS_DIR,
                 cluster_dir = shiny_dir(),  # SHINY_DIR,
                 time_window = TIME_WINDOW,
                 cluster_min_quantile = CLUSTER_MIN_QUANTILE,
                 cluster_steps = CLUSTER_ADDITIONAL_STEPS,
                 visual_min_quantile = VISUAL_MIN_QUANTILE,
                 workers = NO_OF_WORKERS,
                 other_files = path_to_districts())
