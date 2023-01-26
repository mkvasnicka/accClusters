# -------------------------------------
# Script:   prepare_accidents_raw.R
# Author:   Michal Kvasnička
# Purpose:  This script creates/updates the information about the accident for
#           each district. It prepares the raw accidents for the fugure
#           filtering.
# Inputs:   road map, accidents provided by the police, profiles, and log file
# Outputs:  files describing the accidents in each district
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
                 path_to_state_polygon = path_to_state_polygon(),
                 profiles = profiles)
