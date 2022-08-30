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


# TEMP: remove
# supply path to RSCRIPTDIR if it was not supplied outside
if (!exists("RSCRIPTDIR")) RSCRIPTDIR <- "guts"
# supply path to folder where user stores her config and profile
if (!exists("CONFIGDIR")) CONFIGDIR <- file.path(RSCRIPTDIR, "config")


# source necessary scripts
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_profiles_preparation.R"))


# # process command-line parameters
# process_command_line_arguments(RSCRIPTDIR)


# create profiles
create_profiles(path_to_configs = path_to_configs(CONFIGDIR),
                path_to_source_configs = CONFIGDIR)
