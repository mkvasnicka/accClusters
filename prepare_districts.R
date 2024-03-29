# -------------------------------------
# Script:   prepare_districts.R
# Author:   Michal Kvasnička
# Purpose:  This script creates/updates districts (okresy now). It reads them
#           from a shape file(s) and transforms them to Křovák CRS.
# Inputs:   a shape file of political districts in the Czech Republic, profiles,
#           and log file
# Outputs:  sf tibble districts (saved on disk)
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
source(file.path(RSCRIPTDIR, "functions_districts_preparation.R"))


# read user config/profiles
profiles <- read_profiles()


# create major districts table
create_districts(path_to_districts = path_to_districts(),
                 path_to_raw_districts = path_to_raw_districts(),
                 profiles = profiles)


# create a modified version of districts for shiny
create_districts(path_to_districts = path_to_shiny_districts(),
                 path_to_raw_districts = path_to_raw_districts(),
                 profiles = profiles,
                 shiny = TRUE)


# create ORPs for shiny
create_orps(path_to_orps = path_to_shiny_orps(),
            path_to_raw_districts = path_to_raw_districts(),
            profiles = profiles)


# create the whole state polygon
create_state_polygon(path_to_state_polygon = path_to_state_polygon(),
                     path_to_raw_districts = path_to_raw_districts(),
                     profiles = profiles)
