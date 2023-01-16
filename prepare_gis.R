# -------------------------------------
# Script:   prepare_gis.R
# Author:   Michal Kvasnička
# Purpose:  This script exports the clusters for the use in GIS software.
# Inputs:   accidents, clusters, profiles, and log file
# Outputs:  GIS files
#
# Copyright(c) Corporation Name
# -------------------------------------

# TEMP: remove
# supply path to RSCRIPTDIR if it was not supplied outside
if (!exists("RSCRIPTDIR")) RSCRIPTDIR <- "."
# supply path to folder where user stores her config and profile
if (!exists("DIR_ORIGIN")) DIR_ORIGIN <- "data"


# source necessary scripts
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_gis_preparation.R"))


# process command-line parameters
# read user config/profiles
profiles <- read_profiles()


# read in districts
districts <- read_districts()


# write GIS shape file; this doesn't update but it rewrites existing files
write_gis_files(districts,
                gis_dir = gis_dir(),
                shiny_dir = shiny_dir(),
                profiles)
