# -------------------------------------
# Script:   prepare_maps.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:   OSM maps
# Outputs:  tibble of districts
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

require(readr)


# source necessary scripts
RSCRIPTDIR <- "guts"
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_map_preparation.R"))


# read in districts
districts <- readr::read_rds(PATH_TO_DISTRICTS)


# for each district, create .osm files with filered roads
if (is_behind(file.path(OSM_MAPS_DIR, districts$osm_file_name),
              c(PATH_TO_RAW_ROADS_OSM, PATH_TO_DISTRICTS))) {
    create_osm_district_roads(districts, PATH_TO_RAW_ROADS_OSM,
                              road_types = SUPPORTED_ROAD_CLASSES,
                              buffer_size = DISTRICT_BUFFER_SIZE,
                              folder = OSM_MAPS_DIR)
}


# for each district, create SF .rds files with filered roads
if (is_behind(file.path(SF_MAPS_DIR, districts$sf_file_name),
              c(file.path(OSM_MAPS_DIR, districts$osm_file_name),
                PATH_TO_DISTRICTS))) {
    create_sf_district_roads(districts, OSM_MAPS_DIR, SF_MAPS_DIR)
}
