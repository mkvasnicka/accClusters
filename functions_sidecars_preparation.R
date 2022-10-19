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

# load packages
library(dplyr, verbose = FALSE, warn.conflicts = FALSE)
library(tidyr, verbose = FALSE, warn.conflicts = FALSE)
library(purrr, verbose = FALSE, warn.conflicts = FALSE)
library(stringr, verbose = FALSE, warn.conflicts = FALSE)
library(readr, verbose = FALSE, warn.conflicts = FALSE)
library(sf, verbose = FALSE, warn.conflicts = FALSE)
library(openssl, verbose = FALSE, warn.conflicts = FALSE)


write_sidecar <- function(one_row) {
    file_name <- shiny_file_name(one_row,
                                 from_date = one_row$from_date,
                                 to_date = one_row$to_date,
                                 profile_name = one_row$PROFILE_NAME) |>
        str_replace("\\.rds", "_sidecar.rds")
    file_path <- file.path(shiny_dir(), file_name)
    write_rds(one_row, file = file_path)
}


create_sidecars <- function(districts, profiles) {
    profiles <- tidyr::unnest(profiles, TIME_WINDOW)
    districts <- sf::st_drop_geometry(districts)
    goo <- tidyr::expand_grid(districts, profiles)
    goo$file_name <- shiny_file_name(goo, from_date = goo$from_date,
                                     to_date = goo$to_date,
                                     profile_name = goo$PROFILE_NAME)
    hash_fun <- purrr::possibly(openssl::md5, otherwise = NA)
    goo$hash <- purrr::map(goo$file_name,
                           ~hash_fun(file(file.path(shiny_dir(), .))))
    # if (any(map_lgl(goo$hash, ~is.na(.[[1]]))))
    #     stop()
    goo <- goo |>
        dplyr::rowwise() |>
        dplyr::group_split()
    purrr::walk(goo, write_sidecar)
}
