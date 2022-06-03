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
# TODO: načíst skutečná data o nehodách
pcrdata <- new.env()
load(PATH_TO_RAW_ACCIDENTS, envir = pcrdata)
accidents <- pcrdata$accidents
accidents <- st_transform(accidents, planary_projection)
rm(pcrdata)


# lines <- readr::read_rds(file.path(SF_MAPS_DIR, "district_40711.rds"))
# snapped_points <- snap_points_to_lines(accidents, lines,
#                                        dist = ACCIDENT_TO_ROAD_MAX_DISTANCE)
# write_dir_rds(snapped_points, file.path(ACCIDENTS_DIR, "accidents_40711.rds"))

if (is_behind(file.path(ACCIDENTS_DIR, districts$accidents_file_name),
              c(file.path(SF_MAPS_DIR, districts$sf_file_name),
                PATH_TO_DISTRICTS, PATH_TO_RAW_ACCIDENTS))) {
    create_districts_accidents(districts, accidents,
                               max_distance = ACCIDENT_TO_ROAD_MAX_DISTANCE,
                               map_dir = SF_MAPS_DIR,
                               accident_dir = ACCIDENTS_DIR)
}
