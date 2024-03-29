# -------------------------------------
# Script:   functions_districts_preparation.R
# Author:   Michal Kvasnička
# Purpose:  This script defines functions for reading and writing polygons
#           of individual districts in the Czech Republic (implicitly "okresy")
#           and ORPs, too.
# Inputs:   none
# Outputs:  function definitions
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



# create districts -------------------------------------------------------------

# functions read_districts_*() read districts from a path and return
# standardized sf table
#
# inputs:
# - path_to_districts ... (character scalar) a path to spatial database
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
# - path_to_raw_districts ... (character scalar) path to folder where
#   the spatial data are stored
# - reader ... (closure) function that reads the districts table; several of
#   these functions are implemented above
# - profiles ... profile
# - shiny ... (logical scalar) if FALSE (default), the resulting sf object is
#   in the PLANARY_PROJECTION; if TRUE, it is reprojected to WGS84
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
                             profiles,
                             shiny = FALSE) {
    start_logging(log_dir())
    logging::loginfo("districts prep: checking for updates")
    if (is_behind(path_to_districts,
                  c(dir(path_to_raw_districts(), full.names = TRUE),
                    path_to_configs()))) {
        logging::loginfo(
            "districts prep: districts table is behind and will be updated")
        tryCatch({
            districts <- suppressMessages(reader(path_to_raw_districts))
            # this line is country and coding specific; remove if used for any
            # other country
            # districts <- add_greater_brno(districts)
            if ("DISTRICTS" %in% names(profiles)) {
                districts <- districts |>
                    dplyr::filter(district_id %in% profiles$DISTRICTS[[1]])
                logging::loginfo("districts prep: removing all districts but %s",
                                 str_flatten(profiles$DISTRICTS[[1]],
                                             collapse = ", "))
            }
            if (shiny) {
                districts <- st_transform(districts, crs = WGS84)
            }
            write_dir_rds(districts, file = path_to_districts, compress = !shiny)
            logging::loginfo("districts prep: districts table has been updated")
        },
        error = function(e) {
            logging::logerror("districts prep failed: %s", e)
            stop("districts prep failed---stopping evaluation")})
    } else {
        loginfo("districts are up-to-date---skipping")
    }
}



# create ORPs ------------------------------------------------------------------

# functions read_orps_cuzk() reads ORPs from a path and returns standardized
# sf table
#
# inputs:
# - path_to_districts ... (character scalar) a path to spatial database
# - layer ... (optional, character scaler) the name of the layer
#
# output:
# - sf table
#
# source of the data is CUZK
# - web page: https://geoportal.cuzk.cz/(S(uufahw3bishqszcklzne0nex))/Default.aspx?mode=TextMeta&side=dSady_hranice10&metadataID=CZ-CUZK-SH-V&mapid=5&head_tab=sekce-02-gp&menu=2521
# - direct link: https://geoportal.cuzk.cz/zakazky/SPH/SPH_SHP_JTSK.zip
# - warning: projection is not declared in the layer!
read_orps_cuzk <- function(path_to_districts, layer = "SPH_ORP") {
    sf::st_read(path_to_districts, layer = layer, stringsAsFactors = FALSE) |>
        sf::st_set_crs(PLANARY_PROJECTION)
}


# create_orps() creates/updates ORPs table
#
# inputs:
# - path_to_orps ... (character scalar) path where ORPs table should
#   be written
# - path_to_raw_districts ... (character scalar) path to folder where spatial
#   data are stored
# - reader ... (closure) function that reads the ORPs table; several of
#   these functions can be implemented
# - profiles ... profile
#
# value:
#   none; data are written to disk
#
# WARNINGS:
# - it may be necessary to implement a new reader function whenever
#   a new/updated districts/ORPs shapefile is used; it is because the providers
#   change the variable names, projection, etc., between versions
create_orps <- function(path_to_orps, path_to_raw_districts,
                        reader = read_orps_cuzk,
                        profiles) {
    start_logging(log_dir())
    logging::loginfo("orps prep: checking for updates")
    if (is_behind(path_to_orps,
                  c(dir(path_to_raw_districts(), full.names = TRUE),
                    path_to_configs()))) {
        logging::loginfo("orps prep: orps table is behind and will be updated")
        tryCatch({
            districts <- read_districts()
            orps <- suppressMessages(reader(path_to_raw_districts)) |>
                dplyr::select(ORP_NUTS3 = KOD_NUTS3,
                              ORP_CODE = KOD_ORP,
                              ORP_NAME = NAZEV_ORP)
            suppressWarnings(
                j <- orps |>
                    sf::st_centroid() %>%
                    sf::st_join(districts) |>
                    sf::st_drop_geometry() |>
                    dplyr::select(ORP_CODE, contains("district")) |>
                    tibble::as_tibble()
            )
            orps <- dplyr::left_join(orps, j) |>
                dplyr::select(contains("district")) |>
                sf::st_transform(crs = WGS84)
            if ("DISTRICTS" %in% names(profiles)) {
                orps <- orps |>
                    dplyr::filter(district_id %in% profiles$DISTRICTS[[1]])
                logging::loginfo("orps prep: removing all orps but %s",
                                 str_flatten(profiles$DISTRICTS[[1]],
                                             collapse = ", "))
            }
            write_dir_rds(orps, file = path_to_orps)
            logging::loginfo("orps prep: orps table has been updated")
        },
        error = function(e) {
            logging::logerror("orps prep failed: %s", e)
            stop("orps prep failed---stopping evaluation")})
    } else {
        loginfo("orps are up-to-date---skipping")
    }
}


# create state shapefile -------------------------------------------------------

# functions read_state_polygon_cuzk() reads the whole state's polygonfrom a path
# and returns standardized sf table
#
# inputs:
# - path_to_districts ... (character scalar) a path to spatial database
# - layer ... (optional, character scaler) the name of the layer
#
# output:
# - sf table
#
# source of the data is CUZK
# - web page: https://geoportal.cuzk.cz/(S(uufahw3bishqszcklzne0nex))/Default.aspx?mode=TextMeta&side=dSady_hranice10&metadataID=CZ-CUZK-SH-V&mapid=5&head_tab=sekce-02-gp&menu=2521
# - direct link: https://geoportal.cuzk.cz/zakazky/SPH/SPH_SHP_JTSK.zip
# - warning: projection is not declared in the layer!
read_state_polygon_cuzk <- function(path_to_districts, layer = "SPH_STAT") {
    sf::st_read(path_to_districts, layer = layer, stringsAsFactors = FALSE) |>
        sf::st_set_crs(PLANARY_PROJECTION)
}


# create_state_polygon() creates/updates the whole state's polygon table
#
# inputs:
# - path_to_state_polygon ... (character scalar) path where the state polygon
#   table should be written
# - path_to_raw_districts ... (character scalar) path to folder where spatial
#   data are stored
# - reader ... (closure) function that reads the ORPs table; several of
#   these functions can be implemented
# - profiles ... profile
#
# value:
#   none; data are written to disk
#
# WARNINGS:
# - it may be necessary to implement a new reader function whenever
#   a new/updated districts/ORPs shapefile is used; it is because the providers
#   change the variable names, projection, etc., between versions
create_state_polygon <- function(path_to_state_polygon, path_to_raw_districts,
                                 reader = read_state_polygon_cuzk,
                                 profiles) {
    start_logging(log_dir())
    logging::loginfo("state polygon prep: checking for updates")
    if (is_behind(path_to_state_polygon,
                  c(dir(path_to_raw_districts(), full.names = TRUE),
                    path_to_configs()))) {
        logging::loginfo("state polygon prep: state polygon table is behind and will be updated")
        tryCatch({
            state_polygon <- suppressMessages(reader(path_to_raw_districts))
            write_dir_rds(state_polygon, file = path_to_state_polygon,
                          compress = TRUE)
            logging::loginfo("state polygon prep: state polygon table has been updated")
        },
        error = function(e) {
            logging::logerror("state polygon prep failed: %s", e)
            stop("state polygon prep failed---stopping evaluation")})
    } else {
        loginfo("state polygon are up-to-date---skipping")
    }
}
