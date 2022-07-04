# -------------------------------------
# Script:
# Author:
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Corporation Name
# -------------------------------------

# load necessary packages
library(readr)


# supply path to RSCRIPTDIR if it was not supplied outside
if (!exists("RSCRIPTDIR")) RSCRIPTDIR <- "guts"


# source necessary scripts
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_gis_preparation.R"))


# process command-line parameters
process_command_line_arguments(RSCRIPTDIR)


# read in districts
districts <- readr::read_rds(path_to_districts())


# write GIS shape file; this doesn't update but it rewrites existing files
write_gis_files(districts,
                time_window = TIME_WINDOW,
                gis_dir = gis_dir(),
                shiny_dir = shiny_dir())
