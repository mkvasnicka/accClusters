# -------------------------------------
# Script:   functions_districts_preparation.R
# Author:   Michal Kvasnička
# Purpose:  This script defines functions for reading and writing polygons
#           of individual districts in the Czech Republic. Implicitly "okresy".
# Inputs:   none
# Outputs:  functions
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# packages
library(dplyr, verbose = FALSE, warn.conflicts = FALSE)
library(glue, verbose = FALSE, warn.conflicts = FALSE)
library(sf, verbose = FALSE, warn.conflicts = FALSE)
library(nngeo, verbose = FALSE, warn.conflicts = FALSE)


# projections
PLANARY_PROJECTION <- 5514  # Křovák
WGS84 <- 4326  # WGS84



# read districts ---------------------------------------------------------------

# functions read_districts_*() read districts from a path and return
# standardized sf table
#
# inputs:
# - path_to_districts ... (character scalar) a path to ARCCR database
# - layer ... (optional, character scaler) the name of the layer
#
# output:
# - sf table
#
# there are several versions now:
# - for ARC CR 3
read_districts_arccr3 <- function(path_to_districts, layer = "OkresyPolygony") {
    sf::st_read(path_to_districts, layer = layer, stringsAsFactors = FALSE) |>
        dplyr::select(KOD_OKRES:NAZ_CZNUTS3) |>
        dplyr::mutate(district_name = NAZ_LAU1,
                      district_id = KOD_OKRES) |>
        dplyr::select(-NAZ_LAU1, -KOD_OKRES) |>
        dplyr::select(district_id, district_name, everything()) |>
        sf::st_transform(crs = PLANARY_PROJECTION)
}
# - for ARC CR 4.1
read_districts_arccr41 <- function(path_to_districts, layer = "Okres_SLDB") {
    sf::st_read(path_to_districts, layer = layer, stringsAsFactors = FALSE) |>
        dplyr::select(
            district_id = nutslau,
            district_name = nazev
        ) |>
        sf::st_transform(crs = PLANARY_PROJECTION)
}
# -for CUZK
#   - web page: https://geoportal.cuzk.cz/(S(uufahw3bishqszcklzne0nex))/Default.aspx?mode=TextMeta&side=dSady_hranice10&metadataID=CZ-CUZK-SH-V&mapid=5&head_tab=sekce-02-gp&menu=2521
#   - direct link: https://geoportal.cuzk.cz/zakazky/SPH/SPH_SHP_JTSK.zip
#   - warning: projection is not declared in the layer!
read_districts_cuzk <- function(path_to_districts, layer = "SPH_OKRES") {
    sf::st_read(path_to_districts, layer = layer, stringsAsFactors = FALSE) |>
        dplyr::select(
            district_id = KOD_LAU1,
            district_name = NAZEV_LAU1
        ) |>
        sf::st_set_crs(PLANARY_PROJECTION)
}


# add_greater_brno(districts) adds one more district into the district tibble:
# joined Brno město and Brno venkov
#
# warning: this function is country and coding dependent!
add_greater_brno <- function(districts) {
    bm <- dplyr::filter(districts, district_id == "CZ0642") |>
        dplyr::select(-everything())
    bv <- dplyr::filter(districts, district_id == "CZ0643") |>
        dplyr::select(-everything())
    greater_brno <- st_union(bm, bv) |>
        nngeo::st_remove_holes() |>
        mutate(district_id = "CZ06423",
               district_name = "Brno-město+venkov")
    dplyr::bind_rows(districts,
                     greater_brno)
}


# create_districts() creates/updates districts table
#
# inputs:
# - path_to_districts ... (character scalar) path where districts table should
#   be written
# - path_to_raw_districts ... (character scalar) path to folder where ARCČR data
#   (AdministrativniCleneni_v13.gdb) are stored
# - reader ... (closure) function that reads the districts table; several of
#   these functions are implemented above
#
# value:
#   none; data are written to disk
#
# WARNINGS:
# - it may be necessary to implement a new reader function whenever
#   a new/updated districts shapefile is used; it is because the providers
#   change the variable names, coding, etc., between versions
# - it add a district that joins Brno-město and Brno-venkov; this is country and
#   coding specific; see add_greater_brno() function
create_districts <- function(path_to_districts, path_to_raw_districts,
                             reader = read_districts_cuzk,
                             profiles) {
    start_logging(log_dir())
    logging::loginfo("districts prep: checking for updates")
    if (is_behind(path_to_districts,
                  c(path_to_raw_districts, path_to_configs()))) {
        logging::loginfo(
            "districts prep: districts table is behind and will be updated")
        tryCatch({
            districts <- suppressMessages(reader(path_to_raw_districts))
            # this line is country and coding specific; remove if used for any
            # other country
            districts <- add_greater_brno(districts)
            if ("DISTRICTS" %in% names(profiles)) {
                districts <- districts |>
                    dplyr::filter(district_id %in% profiles$DISTRICTS[[1]])
                logging::loginfo("districts prep: removing all districts but %s",
                                 str_flatten(profiles$DISTRICTS[[1]],
                                             collapse = ", "))
            }
            write_dir_rds(districts, file = path_to_districts)
            logging::loginfo("districts prep: districts table has been updated")
        },
        error = function(e) {
            logging::logerror("districts prep failed: %s", e)
            stop("districts prep failed---stopping evaluation")})
    } else {
        loginfo("districts are up-to-date---skipping")
    }
}
