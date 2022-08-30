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

# TEMP: remove
# supply path to RSCRIPTDIR if it was not supplied outside
if (!exists("RSCRIPTDIR")) RSCRIPTDIR <- "guts"
# supply path to folder where user stores her config and profile
if (!exists("DIR_ORIGIN")) DIR_ORIGIN <- "guts_data"


# source necessary scripts
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_map_preparation.R"))


# read user config/profiles
profiles <- read_profiles()


# read in districts
districts <- read_districts()


# for each district, create .osm files with filered roads
create_osm_district_roads(districts,
                          path_to_osm_maps = path_to_raw_roads_osm(),
                          path_to_geojsons = path_to_map_dir(),
                          road_types = SUPPORTED_ROAD_CLASSES,
                          buffer_size = DISTRICT_BUFFER_SIZE,
                          other_dependencies = path_to_districts())


# for each district, create simplified sfnetwork .rds files with filtered roads
create_sf_district_roads(districts,
                         path_to_osm_maps = path_to_map_dir(),
                         path_to_sf_maps = path_to_map_dir(),
                         crs = PLANARY_PROJECTION,
                         workers = NO_OF_WORKERS,
                         other_dependencies = path_to_districts())


# for each district, create lixelized network
create_lixelized_roads(districts,
                       input_folder = path_to_map_dir(),
                       output_folder = path_to_lixels_maps_dir(),
                       lx_length = LIXEL_SIZE,
                       mindist = LIXEL_MIN_DIST,
                       workers = NO_OF_WORKERS,
                       other_dependencies = path_to_districts())


# for each district, create lixel centers (samples)
create_lixel_samples_for_roads(districts,
                               input_folder = path_to_lixels_maps_dir(),
                               output_folder = path_to_lixels_maps_dir(),
                               workers = NO_OF_WORKERS,
                               other_dependencies = path_to_districts())


# for each districts, create neighbor list objects (nb)
create_lixel_nbs(districts,
                 input_folder = path_to_lixels_maps_dir(),
                 output_folder = path_to_lixels_maps_dir(),
                 workers = NO_OF_WORKERS,
                 other_dependencies = path_to_districts())
