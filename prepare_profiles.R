# -------------------------------------
# Script:   prepare_profiles.R
# Author:   Michal Kvasnička
# Purpose:  This scripts reads user config and profiles, checks them, and
#           stores them in a file.
# Inputs:   at least config.R; profile_xxx.R, too, if present. Log file, too
# Outputs:  profiles.rds file
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
source(file.path(RSCRIPTDIR, "functions_profiles_preparation.R"))


# create profiles
create_profiles(path_to_configs = path_to_configs(),
                path_to_source_configs = path_to_source_configs())
