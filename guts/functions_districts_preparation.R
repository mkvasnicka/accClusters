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

# packages
require(dplyr)
require(glue)
require(sf)


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
# WARNING: it may be necessary to implement a new reader function whenever
#   a new/updated districts shapefile is used; it is because the providers
#   change the variable names, coding, etc., between versions
create_districts <- function(path_to_districts, path_to_raw_districts,
                             reader = read_districts_cuzk) {
    logging::loginfo("districts prep: checking for updates")
    if (is_behind(path_to_districts, path_to_raw_districts)) {
        logging::loginfo(
            "districts prep: districts table is behind and will be updated")
        tryCatch({
            districts <- suppressMessages(reader(path_to_raw_districts))
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
