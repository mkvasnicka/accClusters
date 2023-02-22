# -------------------------------------
# Script:   profile_default.R
# Author:   Michal Kvasnička
# Purpose:  This script shows which variables may be redefined in profiles.
#           The uncommented variables must be always present.
#           The commented ones (lines with code behind #) may be used to
#           redefine the default values defined in config.R.
# -------------------------------------


# mandatory profile name and comment string
PROFILE_NAME = "default"
PROFILE_COMMENT = "some string shown in shiny app"


# damage costs -----------------------------------------------------------------

# cost of each deceased persion in mil. CZK (d1)
# UNIT_COST_DEAD = 12
# cost of each seriously injured person in mil. CZK (s1)
# UNIT_COST_SERIOUS_INJURY = UNIT_COST_DEAD * 0.25
# cost of each lightly injured person in mil. CZK (l1)
# UNIT_COST_LIGHT_INJURY = UNIT_COST_DEAD * 0.003
# material cost (in mil. CZK) is multiplied by the following coefficient (m1)
# UNIT_COST_MATERIAL = 1
# a fixed cost in mil. CZK added to accidents where someone lost her life (d2)
# CONST_COST_DEAD = 0
# a fixed cost in mil. CZK added to accidents where the worst damage was serious
# injury (s2)
# CONST_COST_SERIOUS_INJURY = 0
# a fixed cost in mil. CZK added to accidents where the worst damage was light
# injury (l2)
# CONST_COST_LIGHT_INJURY = 0
# a fixed cost in mil. CZK added to accidents with material cost only (m2)
# CONST_COST_MATERIAL = 0
# the following coefficient (in mil. CZK) is added to each accidents' cost (c)
# UNIT_COST_CONST = 0



# NKDE parameters --------------------------------------------------------------

# NKDE weights---either "cost" or "equal"
# NKDE_WEIGHTS = "cost"

# NKDE bandwidth in meters
# NKDE_BW = 300

# whether adaptive bandwidth is used---either TRUE or FALSE; TRUE may produce
# slightly more precise results but the computation would be much slower
# NKDE_ADAPTIVE = FALSE

# maximum bandwidth tried when NKDE_ADAPTIVE is TRUE---in meters
# NKDE_TRIM_BW = 600

# NKDE method---either "continuous" or "discontinuous"; "discontinuous" is less
# precise but much faster
# NKDE_METHOD = "continuous"

# to which distance in meters are the accidents aggregated---for a faster
# computation
# NKDE_AGG = 1
