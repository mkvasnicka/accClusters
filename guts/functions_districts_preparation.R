# -------------------------------------
# Script:   districts_preparation_functions.R
# Author:   Michal Kvasnička
# Purpose:  This script defines functions to read in polygons of individual
#           districts in the Czech Republic. Implicitly "okresy".
# Inputs:   none
# Outputs:  functions
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# needed packages
require(dplyr)
require(glue)
require(sf)



# read districts ---------------------------------------------------------------

# function read_arccr_districts() reads districts from a path
#
# inputs:
# - path_to_districts ... (character scalar) a path to ARCCR database
# - layer ... (optional, character scaler) the name of the layer
#
# output:
# - sf table
#
# for previous version of data
read_arccr_districts <- function(path_to_districts, layer = "OkresyPolygony") {
    sf::st_read(path_to_districts, layer = layer, stringsAsFactors = FALSE) |>
        dplyr::select(KOD_OKRES:NAZ_CZNUTS3) |>
        dplyr::mutate(district_name = NAZ_LAU1,
                      district_id = KOD_OKRES) |>
        dplyr::select(-NAZ_LAU1, -KOD_OKRES) |>
        dplyr::select(district_id, district_name, everything()) |>
        sf::st_transform(crs = PLANARY_PROJECTION)
}
# arc cr 4.1
read_arccr_districts <- function(path_to_districts, layer = "Okres_SLDB") {
    sf::st_read(path_to_districts, layer = layer, stringsAsFactors = FALSE) |>
        dplyr::select(
            district_id = nutslau,
            district_name = nazev
        ) |>
        sf::st_transform(crs = PLANARY_PROJECTION)
}
# cuzk
read_arccr_districts <- function(path_to_districts, layer = "SPH_OKRES") {
    sf::st_read(path_to_districts, layer = layer, stringsAsFactors = FALSE) |>
        dplyr::select(
            district_id = KOD_LAU1,
            district_name = NAZEV_LAU1
        ) |>
        sf::st_set_crs(PLANARY_PROJECTION)
}


# create_districts() creates/updates districts table
#
# inputs:
# - path_to_districts ... (character scalar) path where districts table should
#   be written
# - path_to_raw_districts ... (character scalar) path to folder where ARCČR data
#   (AdministrativniCleneni_v13.gdb) are stored
#
# value:
#   none; data are written to disk
create_districts <- function(path_do_districts, path_to_raw_districts) {
    logging::loginfo("districts prep: checking for updates")
    if (is_behind(path_do_districts, path_to_raw_districts)) {
        logging::loginfo(
            "districts prep: districts table is behind and will be updated")
        tryCatch({
            districts <- read_arccr_districts(path_to_raw_districts)
            write_dir_rds(districts, file = path_do_districts)},
        error = function(e) {
            logging::logerror("districts prep failed: %s", e)
            stop("districts prep failed---stopping evaluation")})
        logging::loginfo(
            "districts prep: districts table has been updated")
    } else {
        loginfo("districts are uptodate---skipping")
    }
}
