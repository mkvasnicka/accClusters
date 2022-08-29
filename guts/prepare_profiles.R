# -------------------------------------
# Script:   prepare_profiles.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------


# supply path to RSCRIPTDIR if it was not supplied outside
if (!exists("RSCRIPTDIR")) RSCRIPTDIR <- "guts"


# source necessary scripts
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_profiles_preparation.R"))


# process command-line parameters
process_command_line_arguments(RSCRIPTDIR)


# start logging
start_logging(log_dir())


# create profiles
create_profiles(path_to_districts = path_to_districts(),
                path_to_raw_districts = path_to_raw_districts())
