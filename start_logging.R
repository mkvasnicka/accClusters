# -------------------------------------
# Script:   start_logging.R
# Author:   Michal Kvasnička
# Purpose:  This script create a new log file which is used by the following
#           scripts for logging.
# Inputs:   none
# Outputs:  it creates the log file.
# Notes:
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


# create a log file
create_log_file(log_dir())
