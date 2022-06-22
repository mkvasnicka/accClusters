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


# read in districts
districts <- readr::read_rds(PATH_TO_DISTRICTS)


# read in all accidents data
if (is_behind(PATH_TO_RAW_ACCIDENTS,
              list.files(RAW_ACCIDENTS_DIR, pattern = "csv",
                         recursive = TRUE, full.names = TRUE))) {
    accidents <- read_raw_accidents(RAW_ACCIDENTS_DIR)
    write_dir_rds(accidents, PATH_TO_RAW_ACCIDENTS)
} else {
    accidents <- read_rds(PATH_TO_RAW_ACCIDENTS)
}


# crop the accidents to buffered districts and snap them to selected roads
create_districts_accidents(districts, accidents,
                           max_distance = ACCIDENT_TO_ROAD_MAX_DISTANCE,
                           lixel_dir = LIXEL_MAPS_DIR,
                           accident_dir = ACCIDENTS_DIR,
                           unit_costs = UNIT_COSTS,
                           workers = NO_OF_WORKERS_ACCIDENTS,
                           other_dependencies = c(PATH_TO_DISTRICTS,
                                                  PATH_TO_RAW_ACCIDENTS))
