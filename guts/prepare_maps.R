# -------------------------------------
# Script:   prepare_maps.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:   OSM map and districts
# Outputs:  files with various represetnations of OSM maps restricted to roads
#           and to individual districts
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
source(file.path(RSCRIPTDIR, "functions_map_preparation.R"))


# read in districts
districts <- readr::read_rds(PATH_TO_DISTRICTS)


# for each district, create .osm files with filered roads
if (is_behind(osm_file_name(districts, OSM_MAPS_DIR),
              c(PATH_TO_RAW_ROADS_OSM, PATH_TO_DISTRICTS))) {
    create_osm_district_roads(districts, PATH_TO_RAW_ROADS_OSM,
                              road_types = SUPPORTED_ROAD_CLASSES,
                              buffer_size = DISTRICT_BUFFER_SIZE,
                              folder = OSM_MAPS_DIR)
}


# for each district, create simplified sfnetwork .rds files with filtered roads
if (is_behind(sf_file_name(districts, SF_MAPS_DIR),
              c(osm_file_name(districts, OSM_MAPS_DIR), PATH_TO_DISTRICTS))) {
    create_sf_district_roads(districts, OSM_MAPS_DIR, SF_MAPS_DIR,
                             crs = PLANARY_PROJECTION,
                             workers = NO_OF_WORKERS)
}


# for each district, create lixelized network
if (is_behind(lixel_file_name(districts, LIXEL_MAPS_DIR),
              c(sf_file_name(districts, SF_MAPS_DIR), PATH_TO_DISTRICTS))) {
    create_lixelized_roads(districts,
                           input_folder = SF_MAPS_DIR,
                           output_folder = LIXEL_MAPS_DIR,
                           lx_length = LIXEL_SIZE,
                           mindist = LIXEL_MIN_DIST,
                           workers = NO_OF_WORKERS)
}


# for each district, create lixel centers (samples)
if (is_behind(lixel_sample_file_name(districts, LIXEL_MAPS_DIR),
              c(lixel_file_name(districts, LIXEL_MAPS_DIR),
                PATH_TO_DISTRICTS))) {
    create_lixel_samples_for_roads(districts,
                                   input_folder = LIXEL_MAPS_DIR,
                                   output_folder = LIXEL_MAPS_DIR,
                                   workers = NO_OF_WORKERS)
}


# for each districts, create neighbor list objects (nb)
if (is_behind(lixel_nb_file_name(districts, LIXEL_MAPS_DIR),
              c(lixel_file_name(districts, LIXEL_MAPS_DIR),
                PATH_TO_DISTRICTS))) {
    create_lixel_nbs(districts,
                     input_folder = LIXEL_MAPS_DIR,
                     output_folder = LIXEL_MAPS_DIR,
                     workers = NO_OF_WORKERS)
}
