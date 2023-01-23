# -------------------------------------
# Script:   prepare_accidents_shiny.R
# Author:   Michal Kvasnička
# Purpose:  This script creates/updates the information about the accident for
#           each district. This version creates the accidents for the shiny app.
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


# crop the accidents to buffered districts and snap them to selected roads---for
# shiny
create_districts_accidents(districts,
                           path_to_accidents = path_to_raw_accidents(),
                           lixel_dir = path_to_lixels_maps_dir(),
                           accident_dir = path_to_shiny_accidents_dir(),
                           profiles = profiles,
                           shiny = TRUE)
