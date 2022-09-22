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

library(readr)
library(purrr)

maindir <- "guts_data/rawdata/accidents"
files <- list.files(maindir, pattern = ".csv", full.names = TRUE)

skip_files <- function(file) {
    txt <- read_lines(file = file, skip =  6)
    write_lines(txt, file = file)
}

walk(files, skip_files)
