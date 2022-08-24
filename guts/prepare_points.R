# -------------------------------------
# Script:   prepare_points.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# load necessary packages
require(readr)


# supply path to RSCRIPTDIR if it was not supplied outside
if (!exists("RSCRIPTDIR")) RSCRIPTDIR <- "guts"


# source necessary scripts
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_point_preparation.R"))


# process command-line parameters
process_command_line_arguments(RSCRIPTDIR)


# start logging
start_logging(log_dir())


# read in districts
districts <- readr::read_rds(path_to_districts())


# read in all accidents data
create_accidents(path_to_all_accidents = path_to_raw_accidents(),  # PATH_TO_RAW_ACCIDENTS,
                 raw_accidents_dir = path_to_raw_accidents_dir())


# crop the accidents to buffered districts and snap them to selected roads
create_districts_accidents(districts,
                           path_to_accidents = path_to_raw_accidents(),
                           max_distance = ACCIDENT_TO_ROAD_MAX_DISTANCE,
                           lixel_dir = path_to_lixels_maps_dir(),  # LIXEL_MAPS_DIR,
                           accident_dir = path_to_accidents_dir(),  # ACCIDENTS_DIR,
                           unit_costs = UNIT_COSTS,
                           workers = NO_OF_WORKERS_ACCIDENTS,
                           other_dependencies = path_to_districts())
