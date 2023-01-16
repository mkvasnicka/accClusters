# -------------------------------------
# Script:   prepare_maps_lixel_samples.R
# Author:   Michal Kvasnička
# Purpose:  This script creates/updates road maps in form of lixels and their
#           centers. It filters the maps, simplifies them, and does a lot of
#           other stuff. This part finds a center of each lixel.
# Inputs:   OSM map, districts, profiles, and log file
# Outputs:  files with various represetnations of OSM maps restricted to roads
#           and to individual districts
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
source(file.path(RSCRIPTDIR, "functions_map_preparation.R"))


# read user config/profiles
profiles <- read_profiles()


# read in districts
districts <- read_districts()


# for each district, create lixel centers (samples)
create_lixel_samples_for_roads(districts,
                               input_folder = path_to_lixels_maps_dir(),
                               output_folder = path_to_lixels_maps_dir(),
                               profiles = profiles)
