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



# parallel processing ----------------------------------------------------------

NO_OF_WORKERS <- 4



# paths ------------------------------------------------------------------------

# start of all paths
DIR_ORIGIN <- "guts"

# path to raw data
RAW_DATA_DIR <- file.path(DIR_ORIGIN, "rawdata")

# path to original maps
RAW_MAPS_DIR <- file.path(RAW_DATA_DIR, "maps")
PATH_TO_RAW_DISTRICTS <- file.path(RAW_MAPS_DIR,
                                   "arccr", "AdministrativniCleneni_v13.gdb/")
PATH_TO_RAW_ROADS_OSM <- file.path(RAW_MAPS_DIR, "czech-republic-latest.osm.pbf")


# paths to created data
DATA_DIR <- file.path(DIR_ORIGIN, "data")

# paths to districts
DISTRICTS_DIR <- file.path(DATA_DIR, "districts")
PATH_TO_DISTRICTS <- file.path(DISTRICTS_DIR, "districts.rds")

# paths to filtered/converted maps
OSM_MAPS_DIR <- file.path(DATA_DIR, "maps")
SF_MAPS_DIR <- file.path(DATA_DIR, "maps")
LIXEL_MAPS_DIR <- file.path(DATA_DIR, "maps")



# road types -------------------------------------------------------------------

# which roads we use
# see: https://wiki.openstreetmap.org/wiki/Key:highway
# TODO: zkontrolovat typy silnic -- graf musí být souvislý
SUPPORTED_ROAD_CLASSES <- c("motorway", "motorway_link",
                            "trunk", "trunk_link",
                            "primary", "primary_link",
                            "secondary", "secondary_link",
                            "tertiary", "tertiary_link",
                            "residential", "living_street", "pedestrian",
                            "service", "unclassified", "road",
                            # "track", "bus_guideway", "escape",
                            # "raceway"
                            # "footway", "bridleway", "steps", "corridor", "path",
                            # "cycleway", "sidewalk", "crossing",
                            "construction"
)



# district map creating --------------------------------------------------------

DISTRICT_BUFFER_SIZE <- 1e3
