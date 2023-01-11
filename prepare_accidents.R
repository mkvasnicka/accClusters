# -------------------------------------
# Script:   prepare_accidents.R
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
if (!exists("RSCRIPTDIR")) RSCRIPTDIR <- "."
# supply path to folder where user stores her config and profile
if (!exists("DIR_ORIGIN")) DIR_ORIGIN <- "data"


# source necessary scripts
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_accidents_preparation.R"))


# read user config/profiles
profiles <- read_profiles()


# read in districts
districts <- read_districts()


# read in all accidents data
create_accidents(path_to_all_accidents = path_to_raw_accidents(),
                 raw_accidents_dir = path_to_raw_accidents_dir(),
                 profiles = profiles)
time_to_recover_memory()


# crop the accidents to buffered districts and snap them to selected roads---for
# density computation
create_districts_accidents(districts,
                           path_to_accidents = path_to_raw_accidents(),
                           lixel_dir = path_to_lixels_maps_dir(),
                           accident_dir = path_to_accidents_dir(),
                           profiles = profiles)
time_to_recover_memory()


# crop the accidents to buffered districts and snap them to selected roads---for
# shiny
create_districts_accidents(districts,
                           path_to_accidents = path_to_raw_accidents(),
                           lixel_dir = path_to_lixels_maps_dir(),
                           accident_dir = path_to_shiny_accidents_dir(),
                           profiles = profiles,
                           shiny = TRUE)
time_to_recover_memory()
