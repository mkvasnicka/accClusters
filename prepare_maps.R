# -------------------------------------
# Script:   prepare_maps.R
# Author:   Michal Kvasnička
# Purpose:  This script creates/updates road maps in form of lixels and their
#           centers. It filters the maps, simplifies them, and does a lot of
#           other stuff. This part only filters the roads, splits them into
#           districts, read them in in sf, and simplifies them.
# Inputs:   OSM map, districts, profiles, and log file
# Outputs:  files with various represetnations of OSM maps restricted to roads
#           and to individual districts
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
source(file.path(RSCRIPTDIR, "functions_map_preparation.R"))


# read user config/profiles
profiles <- read_profiles()


# read in districts
districts <- read_districts()


# for each district, create .osm files with filered roads
create_osm_district_roads(districts,
                          path_to_osm_maps = path_to_raw_roads_osm(),
                          path_to_geojsons = path_to_map_dir(),
                          profiles = profiles)


# for each district, create simplified sfnetwork .rds files with filtered roads
create_sf_district_roads(districts,
                         path_to_osm_maps = path_to_map_dir(),
                         path_to_sf_maps = path_to_map_dir(),
                         profiles = profiles)
