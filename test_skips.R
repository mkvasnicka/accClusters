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
files <- list.files(maindir, pattern = ".csv")

test_file <- function(file, maindir) {
    message(file.path(maindir, file))
    ori <- read_lines(file = file.path(maindir, "old", file), skip =  6)
    chn <- read_lines(file = file.path(maindir, file))
    identical(all.equal(ori, chn), TRUE)
}

map_lgl(files, test_file, maindir) |> all()
