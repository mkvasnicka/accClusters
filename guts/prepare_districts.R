# -------------------------------------
# Script:   prepare_maps.R
# Author:   Michal Kvasnička
# Purpose:  This script creates/updates districts (okresy now). It reads them
#           from ARCCR maps and tranforms to Křovák CRS. These polygons are used
#           to crop roads to districts.
# Inputs:   ARCCR maps of political districts in the Czech Republic.
# Outputs:  sf tibble districts (saved on disk)
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# supply path to RSCRIPTDIR if it was not supplied outside
if (!exists("RSCRIPTDIR")) RSCRIPTDIR <- "guts"


# source necessary scripts
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_districts_preparation.R"))


# process command-line parameters
process_command_line_arguments(RSCRIPTDIR)


# create major districts table
create_districts(path_to_districts = path_to_districts(),
                 path_to_raw_districts = path_to_raw_districts())
