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

# remove DIR_ORIGIN---it isn't supported
rm(DIR_ORIGIN)



# parallel processing ----------------------------------------------------------

# how many cores should be used in parallel
NO_OF_WORKERS <- "auto"             # or positive number
NO_OF_WORKERS_ACCIDENTS <- "auto"   # or positive number

# how much ram in GB should be available per core in parallel processing when
# the number of cores used is detected automatically
RAM_PER_CORE_GENERAL <- 3
RAM_PER_CORE_ACCIDENTS <- 10



# maps preparation -------------------------------------------------------------

# buffer sizer around districts in meters---necessary for NKDE
DISTRICT_BUFFER_SIZE <- 1e3

# road lixelization in meters
LIXEL_SIZE <- 5
LIXEL_MIN_DIST <- 2

# snapping accidents to roads---accident farther away from supported roads are
# discarded when computing densities and clusters---in meters
ACCIDENT_TO_ROAD_MAX_DISTANCE <- 30

# road types = which roads we use
# see: https://wiki.openstreetmap.org/wiki/Key:highway
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



# accidents files masks --------------------------------------------------------

# regex for major accident-data file
ACCIDENTS_FILE_NAME_PATTERN <- "\\d{4}_databaze_nehody.csv"

# regex for major gps file of accident data
ACCIDENTS_GPS_FILE_NAME_PATTERN <- "\\d{4}_databaze_GPS.csv"




# damage costs -----------------------------------------------------------------

# cost of various damages in mil. CZK: dead people, serious, and light injuries
UNIT_COSTS_DEAD <- 12
UNIT_COSTS_SERIOUS_INJURY <- UNIT_COSTS_DEAD / 4
UNIT_COSTS_LIGHT_INJURY <- UNIT_COSTS_DEAD / 24

# material cost (in mil. CZK) is multiplied by this coefficient
UNIT_COSTS_MATERIAL <- 1

# the following coefficient (in mil. CZK) is added to each accidents' cost
UNIT_COST_CONST <- 0



# NKDE parameters --------------------------------------------------------------

# NKDE weights---either "cost" or "equal"
NKDE_WEIGHTS <- "cost"

# NKDE bandwidth in meters
NKDE_BW <- 300

# whether adaptive bandwidth is used---either TRUE or FALSE; TRUE may produce
# slightly more precise results but the computation would be much slower
NKDE_ADAPTIVE <- FALSE

# maximum bandwidth tried when NKDE_ADAPTIVE is TRUE---in meters
NKDE_TRIM_BW <- 600

# NKDE method---either "continuous" or "discontinuous"; "discontinuous" is less
# precise but much faster
NKDE_METHOD <- "discontinuous"

# to which distance in meters are the accidents aggregated---for a faster
# computation
NKDE_AGG = 1



# cluster parameters -----------------------------------------------------------

# lixels with at least this density quantile (0, 1) constitute cluster cores
CLUSTER_MIN_QUANTILE <- 0.995

# this many lixels are added to each cluster
CLUSTER_ADDITIONAL_STEPS <- 5

# lixels with at least this density quantile (0, 1) are stored for density
# visualization
VISUAL_MIN_QUANTILE <- 0.95



# time windows -----------------------------------------------------------------

# from- and to-dates when densities and clusters are computed
# format: YYYY-MM-DD
TIME_WINDOW <- tibble::tribble(
    ~from_date,    ~to_date,
    "2019-01-01",  "2021-12-31",
    "2018-01-01",  "2020-12-31"
)
