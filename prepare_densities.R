# -------------------------------------
# Script:   prepare_densities.R
# Author:   Michal Kvasnička
# Purpose:  This script creates/updates the NKDE densities for each district.
# Inputs:   road map, accidents, profiles, and log file
# Outputs:  files describing the NKDE densities for each lixel
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
source(file.path(RSCRIPTDIR, "functions_densities_preparation.R"))


# read user config/profiles
profiles <- read_profiles()


# read in districts
districts <- read_districts()


# compute hotspots, i.e., NKDE
compute_densities(districts,
                  maps_dir = path_to_map_dir(),
                  lixel_dir = path_to_lixels_maps_dir(),
                  sample_dir = path_to_lixels_maps_dir(),
                  accidents_dir = path_to_accidents_dir(),
                  density_dir = path_to_densities_dir(),
                  profiles = profiles)
time_to_recover_memory()
