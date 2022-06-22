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

# parallel processing ----------------------------------------------------------

# how many cores should be used in parallel
NO_OF_WORKERS <- "auto"  # future::availableCores()
NO_OF_WORKERS_ACCIDENTS <- "auto"

# how much ram should be available per core in parallel processing when the
# number of cores used is detected automatically
RAM_PER_CORE_GENERAL <- 3
RAM_PER_CORE_ACCIDENTS <- 10



# constants --------------------------------------------------------------------

# projections
PLANARY_PROJECTION <- 5514  # Křovák
WGS84 <- 4326  # WGS84


# district map creating
DISTRICT_BUFFER_SIZE <- 1e3


# lixelization
LIXEL_SIZE <- 5  # meters
LIXEL_MIN_DIST <- 2  # meters


# snapping accidents to roads
ACCIDENT_TO_ROAD_MAX_DISTANCE <- 30  # meters


# road types = which roads we use
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


# cost of various damages in mil. CZK
# UNIT_COSTS <- c(dead = 12, serious_injury = 3, light_injury = 0.5)
UNIT_COSTS <- list(
    accident_cost = c(dead = 12, serious_injury = 3, light_injury = 0.5),
    accident_higher_cost = c(dead = 30, serious_injury = 30/4,
                             light_injury = 30/24)
)


# NKDE parameters
NKDE_WEIGHTS <- "accident_cost" # weights
NKDE_BW <- 300                  # bw
NKDE_ADAPTIVE <- FALSE          # adaptive
NKDE_TRIM_BW <- 600             # trim_bw
NKDE_METHOD <- "discontinuous"  # method
NKDE_AGG = 1                    # agg



# paths ------------------------------------------------------------------------

# start of all paths
DIR_ORIGIN <- "guts"


# path to time-window file
PATH_TO_TIME_WINDOW <- file.path(DIR_ORIGIN, "time_windows.tsv")


# path to raw data
RAW_DATA_DIR <- file.path(DIR_ORIGIN, "rawdata")

# path to original maps
RAW_MAPS_DIR <- file.path(RAW_DATA_DIR, "maps")
PATH_TO_RAW_DISTRICTS <- file.path(RAW_MAPS_DIR,
                                   "arccr", "AdministrativniCleneni_v13.gdb/")
PATH_TO_RAW_ROADS_OSM <- file.path(RAW_MAPS_DIR, "czech-republic-latest.osm.pbf")

# path to original accidents file
RAW_ACCIDENTS_DIR <- file.path(RAW_DATA_DIR, "accidents")
# PATH_TO_RAW_ACCIDENTS <- file.path(RAW_ACCIDENTS_DIR, "pcrdata.RData")
PATH_TO_RAW_ACCIDENTS <- file.path(RAW_ACCIDENTS_DIR, "raw_accidents.rds")


# paths to created data
DATA_DIR <- file.path(DIR_ORIGIN, "data")

# paths to districts
DISTRICTS_DIR <- file.path(DATA_DIR, "districts")
PATH_TO_DISTRICTS <- file.path(DISTRICTS_DIR, "districts.rds")

# paths to filtered/converted maps
OSM_MAPS_DIR <- file.path(DATA_DIR, "maps")
SF_MAPS_DIR <- file.path(DATA_DIR, "maps")
LIXEL_MAPS_DIR <- file.path(DATA_DIR, "lixels")

# paths to accidents data by districts
ACCIDENTS_DIR <- file.path(DATA_DIR, "accidents")

# path to NKDEs by districts
DENSITIES_DIR <- file.path(DATA_DIR, "densities")


# path to folder where the final product used in shiny in stored
SHINY_DIR <- file.path(DIR_ORIGIN, "shiny")


# logging
LOG_DIR <- file.path(DIR_ORIGIN, "log")
