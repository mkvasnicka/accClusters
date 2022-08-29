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


# supply path to folder where user stores her config and profile
if (!exists("CONFIGDIR")) CONFIGDIR <- "guts/config"


# source necessary scripts
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_profiles_preparation.R"))


# process command-line parameters
process_command_line_arguments(RSCRIPTDIR)


# create profiles
# TODO: nefunguje, protože logování používá LOG_DIR, ale ten se vytváří až v
# configu; shit, jak na to?
create_profiles(path_to_configs = path_to_configs(),
                path_to_source_configs = CONFIGDIR)
