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


# supply path to RSCRIPTDIR if it was not supplied outside
if (!exists("RSCRIPTDIR")) RSCRIPTDIR <- "guts"


# source necessary scripts
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_map_preparation.R"))


# process command-line parameters
process_command_line_arguments(RSCRIPTDIR)


# start logging
start_logging(log_folder = LOG_DIR)


# read in districts
districts <- readr::read_rds(PATH_TO_DISTRICTS)


# for each district, create .osm files with filered roads
    create_osm_district_roads(districts,
                              input_path = PATH_TO_RAW_ROADS_OSM,
                              road_types = SUPPORTED_ROAD_CLASSES,
                              buffer_size = DISTRICT_BUFFER_SIZE,
                              folder = OSM_MAPS_DIR)


# for each district, create simplified sfnetwork .rds files with filtered roads
    create_sf_district_roads(districts, OSM_MAPS_DIR, SF_MAPS_DIR,
                             crs = PLANARY_PROJECTION,
                             workers = NO_OF_WORKERS,
                             other_dependencies = PATH_TO_DISTRICTS)


# for each district, create lixelized network
    create_lixelized_roads(districts,
                           input_folder = SF_MAPS_DIR,
                           output_folder = LIXEL_MAPS_DIR,
                           lx_length = LIXEL_SIZE,
                           mindist = LIXEL_MIN_DIST,
                           workers = NO_OF_WORKERS,
                           other_dependencies = PATH_TO_DISTRICTS)


# for each district, create lixel centers (samples)
    create_lixel_samples_for_roads(districts,
                                   input_folder = LIXEL_MAPS_DIR,
                                   output_folder = LIXEL_MAPS_DIR,
                                   workers = NO_OF_WORKERS,
                                   other_dependencies = PATH_TO_DISTRICTS)


# for each districts, create neighbor list objects (nb)
    create_lixel_nbs(districts,
                     input_folder = LIXEL_MAPS_DIR,
                     output_folder = LIXEL_MAPS_DIR,
                     workers = NO_OF_WORKERS,
                     other_dependencies = PATH_TO_DISTRICTS)
