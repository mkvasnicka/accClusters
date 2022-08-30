# -------------------------------------
# Script:   start_logging.R
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
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))


# read profiles
profiles <- read_rds(file.path(path_to_configs(CONFIGDIR)))


# # process command-line parameters
# process_command_line_arguments(RSCRIPTDIR)


# create a log file
create_log_file(log_dir())
