# -------------------------------------
# Script:   prepare_sidecars.R
# Author:   Michal Kvasnička
# Purpose:  This script creates/updates the sidecars that the shiny app uses
#           to find out for which time/profile it has clusters available.
# Inputs:   clusters, profiles, and log file
# Outputs:  sidecars files
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
source(file.path(RSCRIPTDIR, "functions_sidecars_preparation.R"))


# read user config/profiles
profiles <- read_profiles()


# read in districts
districts <- read_districts()


# create sidecars for cluster files
create_sidecars(districts, profiles)
