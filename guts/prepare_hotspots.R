# -------------------------------------
# Script:   prepare_hotspots.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# load necessary packages
library(readr)


# source necessary scripts
RSCRIPTDIR <- "guts"
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_hotspots_preparation.R"))


# read in districts
districts <- readr::read_rds(PATH_TO_DISTRICTS)


# compute hotspots, i.e., NKDE
    compute_densities(districts,
                      maps_dir = SF_MAPS_DIR,
                      lixel_dir = LIXEL_MAPS_DIR,
                      sample_dir = LIXEL_MAPS_DIR,
                      accidents_dir = ACCIDENTS_DIR,
                      density_dir = DENSITIES_DIR,
                      from_date = "2019-01-01", to_date = "2021-1-31",
                      weights = "accident_cost",
                      bw = 300,
                      adaptive = FALSE, trim_bw = 600,
                      method = "discontinuous", agg = 1,
                      workers = NO_OF_WORKERS,
                      other_files = PATH_TO_DISTRICTS)
