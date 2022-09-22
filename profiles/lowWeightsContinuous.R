# this profile computes NKDE with weights = 1, i.e., each accident has the same
# weight

# profile name
PROFILE_NAME <- "lowWeightsContinuous"

# NKDE parameters
NKDE_WEIGHTS <- "accident_cost" # weights
NKDE_BW <- 300                  # bw
NKDE_ADAPTIVE <- FALSE          # adaptive
NKDE_TRIM_BW <- 600             # trim_bw
NKDE_METHOD <- "continuous"     # method
NKDE_AGG = 1                    # agg
