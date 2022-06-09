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


# source necessary scripts
RSCRIPTDIR <- "guts"
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_point_preparation.R"))


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
if (is_behind(file.path(ACCIDENTS_DIR, districts$accidents_file_name),
              c(file.path(SF_MAPS_DIR, districts$sf_file_name),
                PATH_TO_DISTRICTS, PATH_TO_RAW_ACCIDENTS))) {
    create_districts_accidents(districts, accidents,
                               max_distance = ACCIDENT_TO_ROAD_MAX_DISTANCE,
                               map_dir = SF_MAPS_DIR,
                               accident_dir = ACCIDENTS_DIR,
                               workers = NO_OF_WORKERS)
}
