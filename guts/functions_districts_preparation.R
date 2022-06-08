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

# read districts from ARC ČR and convert it to class "district" to prohibit
# RStudio to freeze when it tries to print it


# adds a class name to object -- at the first place
add_class <- function(object, cls) {
    class(object) <- unique(c(cls, class(object)))
    object
}

remove_first_class <- function(object) {
    class(object) <- class(object)[-1]
    object
}


# function read_arccr_districts() reads districts from a path
#
# inputs:
# - path_to_districts ... (character scalar) a path to ARCCR database
# - layer ... (optional, character scaler) the name of the layer
#
# output:
# - sf table
read_arccr_districts <- function(path_to_districts, layer = "OkresyPolygony") {
    sf::st_read(path_to_districts, layer = layer, stringsAsFactors = FALSE) |>
        dplyr::select(KOD_OKRES:NAZ_CZNUTS3) |>
        dplyr::mutate(district_name = NAZ_LAU1,
                      district_id = KOD_OKRES) |>
        dplyr::select(-NAZ_LAU1, -KOD_OKRES) |>
        dplyr::select(district_id, district_name, everything()) |>
        dplyr::mutate(osm_file_name = glue("district_{district_id}.osm"),
                      sf_file_name = glue("district_{district_id}.rds"),
                      lixel_file_name = glue("lixel_{district_id}.rds"),
                      lixel_sample_file_name =
                          glue("lixel_sample_{district_id}.rds"),
                      accidents_file_name =
                          glue("accidents_{district_id}.rds")) |>
        sf::st_transform(crs = PLANARY_PROJECTION) |>
        add_class("districts")
}


# prohibits RStudio to freeze when it tries to view districts with added slots
print.districts <- function(d, ...) {
    cat("Districts\n  no. of districts:", nrow(d),
        "\n  columns:", paste(names(d), collapse = ", "))
}
#str.districts <- print.districts
filter.districts <- function(d, ...) {
    remove_first_class(d) |>
        dplyr::filter(...) |>
        add_class("districts")
}
mutate.districts <- function(d, ...) {
    remove_first_class(d) |>
        dplyr::mutate(...) |>
        add_class("districts")
}
select.districts <- function(d, ...) {
    remove_first_class(d) |>
        dplyr::select(...) |>
        add_class("districts")
}
