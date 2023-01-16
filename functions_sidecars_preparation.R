# -------------------------------------
# Script:   functions_sidecars_preparation.R
# Author:   Michal Kvasnička
# Purpose:  This scripts defines functions for exporting sidecars for cluster
#           files.
# Inputs:   none
# Outputs:  functions
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# load packages
library(dplyr, verbose = FALSE, warn.conflicts = FALSE)
library(tidyr, verbose = FALSE, warn.conflicts = FALSE)
library(purrr, verbose = FALSE, warn.conflicts = FALSE)
library(stringr, verbose = FALSE, warn.conflicts = FALSE)
library(readr, verbose = FALSE, warn.conflicts = FALSE)
library(sf, verbose = FALSE, warn.conflicts = FALSE)
library(openssl, verbose = FALSE, warn.conflicts = FALSE)


# write_sidecar(one_row) writes one sidecar on the disk
#
# inputs:
# - one_row ... (one-row tibble)
#
# see help for create_sidecars()
write_sidecar <- function(one_row) {
    file_name <- shiny_file_name(one_row,
                                 from_date = one_row$from_date,
                                 to_date = one_row$to_date,
                                 profile_name = one_row$PROFILE_NAME) |>
        str_replace("\\.rds", "_sidecar.rds")
    file_path <- file.path(shiny_dir(), file_name)
    write_rds(one_row, file = file_path)
}


# cluster_file_hash(file_name)
#
# inputs:
# - file_name ... (character scalar) name of a cluster file (without the folder)
#
# value:
#   a has for the file
cluster_file_hash <- function(file_name) {
    file_path <- file.path(shiny_dir(), file_name)
    openssl::md5(file(file_path))
}
cluster_file_hash <- purrr::possibly(cluster_file_hash, otherwise = NA)


# create_sidecars(districts, profiles) writes sidecar files for cluster files;
# the sidecars are used in shiny---they signal the system setting used to
# compute the particular clusters; thus is containt information from districts
# and from profiles; it also containt hash for the cluster files
#
# inputs:
# - districts ... tibble of districts created by prepare_districts script
# - profiles ... tibble of profiles created by prepare_profiles script
#
# output:
#   none; it only writes the sidecars on the disk
#
# notes:
# - each sidecar is .rds; it containt one-row tibble that includes all pieces of
#   information on profiles and districts (except their geometry) used to
#   compute the corresponding cluster files + the name of the cluster file + the
#   hash of the cluster file
# - the name of the sidecar is the same as the name of the corresponding cluster
#   file with "_sidecar" added in front of ".rds"
# - the sidecars are stored in the same folder as the clusters files
create_sidecars <- function(districts, profiles) {
    start_logging(log_dir())
    profiles <- tidyr::unnest(profiles, TIME_WINDOW)
    districts <- sf::st_drop_geometry(districts)
    goo <- tidyr::expand_grid(districts, profiles)
    logging::loginfo("sidecars prep: creating %s sidecars", nrow(goo))
    goo$file_name <- shiny_file_name(goo, from_date = goo$from_date,
                                     to_date = goo$to_date,
                                     profile_name = goo$PROFILE_NAME)
    hash_fun <- purrr::possibly(openssl::md5, otherwise = NA)
    goo$hash <- purrr::map(goo$file_name, cluster_file_hash)
    no_hash <- map_lgl(goo$hash, ~is.na(.[[1]]))
    if (any(no_hash)) {
        logging::logerror("sidecars prep: failed to create hash for %s",
                          stringr::str_c(goo$file_name[no_hash],
                                         collapse = ","))
        stop("sidecars prep failed, see log")
    }
    goo <- goo |>
        dplyr::rowwise() |>
        dplyr::group_split()
    purrr::walk(goo, write_sidecar)
    logging::loginfo("sidecars prep: sidecars created")
}
