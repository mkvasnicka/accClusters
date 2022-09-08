# mandatory profile name
PROFILE_NAME <- "default"



# damage costs -----------------------------------------------------------------

# cost of various damages in mil. CZK: dead people, serious, and light injuries
UNIT_COST_DEAD <- 12
UNIT_COST_SERIOUS_INJURY <- UNIT_COST_DEAD / 4
UNIT_COST_LIGHT_INJURY <- UNIT_COST_DEAD / 24

# material cost (in mil. CZK) is multiplied by this coefficient
UNIT_COST_MATERIAL <- 1

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

# whether to create time windows for the last complete years
TIME_WINDOW_AUTO <- TRUE

# if so, how long should each window be (in years)
TIME_WINDOW_LENGHT <- 3

# and how many such windows should be created
TIME_WINDOW_NUMBER <- 2

# possible additional time windows; format: YYYY-MM-DD
# TIME_WINDOW <- tibble::tribble(
#     ~from_date,    ~to_date,
#     "2019-01-01",  "2021-12-31",
#     "2018-01-01",  "2020-12-31"
# )
