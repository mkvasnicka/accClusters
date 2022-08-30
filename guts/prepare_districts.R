# -------------------------------------
# Script:   prepare_maps.R
# Author:   Michal Kvasnička
# Purpose:  This script creates/updates districts (okresy now). It reads them
#           from ARCCR maps and tranforms to Křovák CRS. These polygons are used
#           to crop roads to districts.
# Inputs:   ARCCR maps of political districts in the Czech Republic.
# Outputs:  sf tibble districts (saved on disk)
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
source(file.path(RSCRIPTDIR, "functions_districts_preparation.R"))


# read user config/profiles
profiles <- read_profiles()


# create major districts table
create_districts(path_to_districts = path_to_districts(),
                 path_to_raw_districts = path_to_raw_districts(),
                 profiles = profiles)
