# -------------------------------------
# Script:   guts_config.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:   none
# Outputs:  constatns and paths
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# constants --------------------------------------------------------------------

# projections
planary_projection <- 5514  # Křovák
wgs_projection <- 4326  # WGS84



# paths ------------------------------------------------------------------------

# start of all paths
DIR_ORIGIN <- "guts"

# path to raw data
RAW_DATA_DIR <- file.path(DIR_ORIGIN, "rawdata")

# path to original maps
RAW_MAPS_DIR <- file.path(RAW_DATA_DIR, "maps")
PATH_TO_RAW_DISTRICTS <- file.path(RAW_MAPS_DIR,
                                   "arccr", "AdministrativniCleneni_v13.gdb/")
PATH_TO_ROADS_OSM <- file.path(RAW_MAPS_DIR, "czech-republic-latest.osm.pbf")


# paths to created data
DATA_DIR <- file.path(DIR_ORIGIN, "data")

# paths to districts
DISTRICTS_DIR <- file.path(DATA_DIR, "districts")
PATH_TO_DISTRICTS <- file.path(DISTRICTS_DIR, "districts.rds")
