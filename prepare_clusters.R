# -------------------------------------
# Script:   prepare_clusters.R
# Author:   Michal Kvasnička
# Purpose:  This script creates/updates the accident clusters/hotspots.
# Inputs:   road map, densities, profiles, and log file
# Outputs:  files describing the clusters
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# TEMP: remove
# supply path to RSCRIPTDIR if it was not supplied outside
if (!exists("RSCRIPTDIR")) RSCRIPTDIR <- "."
# supply path to folder where user stores her config and profile
if (!exists("DIR_ORIGIN")) DIR_ORIGIN <- "data"


# source necessary scripts
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_cluster_preparation.R"))


# read user config/profiles
profiles <- read_profiles()


# read in districts
districts <- read_districts()


# compute and save clusters for all periods
compute_clusters(districts,
                 densities_dir = path_to_densities_dir(),
                 lixel_maps_dir = path_to_lixels_maps_dir(),
                 accidents_dir = path_to_accidents_dir(),
                 cluster_dir = shiny_dir(),
                 profiles)
