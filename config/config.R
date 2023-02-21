# -------------------------------------
# Script:   config.R
# Author:   Michal Kvasnička
# Purpose:  This script defines the implicit values for road accident hotspots
#           detection.
# -------------------------------------


# time windows -----------------------------------------------------------------

# whether to create time windows for the last complete years
TIME_WINDOW_AUTO = TRUE

# if so, how long should each window be (in years)
TIME_WINDOW_LENGTH = 3

# and how many such windows should be created
TIME_WINDOW_NUMBER = 2

# possible additional time windows; format: YYYY-MM-DD
# TIME_WINDOW = tibble::tribble(
#     ~from_date,    ~to_date,
#     "2019-01-01",  "2021-12-31",
#     "2018-01-01",  "2020-12-31"
# )



# damage costs -----------------------------------------------------------------

# cost of each deceased persion in mil. CZK
UNIT_COST_DEAD = 12
# cost of each seriously injured person in mil. CZK
UNIT_COST_SERIOUS_INJURY = UNIT_COST_DEAD * 0.25
# cost of each lightly injured person in mil. CZK
UNIT_COST_LIGHT_INJURY = UNIT_COST_DEAD * 0.003
# material cost (in mil. CZK) is multiplied by the following coefficient
UNIT_COST_MATERIAL = 1
# the following coefficient (in mil. CZK) is added to each accidents' cost
UNIT_COST_CONST = 0
# a fixed cost in mil. CZK added to accidents where someone lost her life
CONST_COST_DEAD = 0
# a fixed cost in mil. CZK added to accidents where the worst damage was serious
# injury
CONST_COST_SERIOUS_INJURY
# a fixed cost in mil. CZK added to accidents where the worst damage was light
# injury
CONST_COST_LIGHT_INJURY
# a fixed cost in mil. CZK added to accidents with material cost only
CONST_COST_MATERIAL



# NKDE parameters --------------------------------------------------------------

# NKDE weights---either "cost" or "equal"
NKDE_WEIGHTS = "cost"

# NKDE method---either "continuous" or "discontinuous"; "discontinuous" is less
# precise but much faster
NKDE_METHOD = "continuous"

# NKDE bandwidth in meters
NKDE_BW = 300

# whether adaptive bandwidth is used---either TRUE or FALSE; TRUE may produce
# slightly more precise results but the computation would be much slower
NKDE_ADAPTIVE = FALSE

# maximum bandwidth tried when NKDE_ADAPTIVE is TRUE---in meters
NKDE_TRIM_BW = 600

# to which distance in meters are the accidents aggregated---for a faster
# computation
NKDE_AGG = 1



# maps preparation -------------------------------------------------------------

# buffer sizer around districts in meters---necessary for NKDE
DISTRICT_BUFFER_SIZE = 3e3

# road lixelization in meters
LIXEL_SIZE = 5
LIXEL_MIN_DIST = 2

# snapping accidents to roads---accident farther away from supported roads are
# discarded when computing densities and clusters---in meters
ACCIDENT_TO_ROAD_MAX_DISTANCE = 30

# road types = which roads we use
# see: https://wiki.openstreetmap.org/wiki/Key:highway
SUPPORTED_ROAD_CLASSES = c("motorway", "motorway_link",
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



# parallel processing ----------------------------------------------------------

# how many cores should be used in parallel
NO_OF_WORKERS = "auto"             # or positive number
NO_OF_WORKERS_ACCIDENTS = "auto"   # or positive number

# how much ram in GB should be available per core in parallel processing when
# the number of cores used is detected automatically
RAM_PER_CORE_GENERAL = 4.5
RAM_PER_CORE_ACCIDENTS = 10

# how many districts should be extracted by osmium from the geofabric map shape
# file at once; if the amount of available memory is 32 GB, 10 seems to be safe;
# if the amount of available memory is lower, you should lower the constant; if
# it is higher, you can raise it
OSMIUM_DISTRICTS_IN_ONE_GO = 10
