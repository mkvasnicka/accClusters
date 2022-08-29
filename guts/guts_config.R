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

# # projections
# PLANARY_PROJECTION <- 5514  # Křovák
# WGS84 <- 4326  # WGS84


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
UNIT_COSTS_DEAD <- 12
UNIT_COSTS_SERIOUS_INJURY <- UNIT_COSTS_DEAD / 4
UNIT_COSTS_LIGHT_INJURY <- UNIT_COSTS_DEAD / 24
UNIT_COSTS_MATERIAL <- 1
UNIT_COST_CONST <- 0


# NKDE parameters
NKDE_WEIGHTS <- "cost"          # weights
NKDE_BW <- 300                  # bw
NKDE_ADAPTIVE <- FALSE          # adaptive
NKDE_TRIM_BW <- 600             # trim_bw
NKDE_METHOD <- "discontinuous"  # method
NKDE_AGG = 1                    # agg


# cluster parameters
CLUSTER_MIN_QUANTILE <- 0.995
CLUSTER_ADDITIONAL_STEPS <- 5
VISUAL_MIN_QUANTILE <- 0.95


# time window---either path to TSV file, or tibble with from_date and to_date
# columns
# TIME_WINDOW <- file.path(DIR_ORIGIN, "time_windows.tsv")
TIME_WINDOW <- tibble::tribble(
    ~from_date,    ~to_date,
    "2019-01-01",  "2021-12-31",
    "2018-01-01",  "2020-12-31"
)



# paths ------------------------------------------------------------------------

# start of all paths
DIR_ORIGIN <- "guts"


# path to raw data
RAW_DATA_DIR <- file.path(DIR_ORIGIN, "rawdata")

# paths to created data
DATA_DIR <- file.path(DIR_ORIGIN, "data")

# outputs
OUTPUT_DIR <- file.path(DIR_ORIGIN, "output")

# logging
LOG_DIR <- file.path(DIR_ORIGIN, "log")


# accidents files masks --------------------------------------------------------

ACCIDENTS_FILE_NAME_PATTERN <- "\\d{4}_databaze_nehody.csv"
ACCIDENTS_GPS_FILE_NAME_PATTERN <- "\\d{4}_databaze_GPS.csv"
