# -------------------------------------
# Script:   functions_point_preparation.R
# Author:
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Corporation Name
# -------------------------------------


# https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf
# https://r-spatial.github.io/sf/reference/st_nearest_feature.html

require(dplyr)
require(purrr)
require(readr)
require(lubridate)
require(sf)


# prepare raw police data ------------------------------------------------------

# TODO: připravit skutečná policejní data


# read_raw_accidents(folder, skip = 6) reads and geolocates all accidents
#
# inputs:
# - folder ... (character scalar) path to folder where the data are stored
# - skip ... (integer scalar) number of first row in each CSV file that should
#   be skipped
#
# value:
#   sf tibble of all accidents
#
# assumptions:
# - all data are stored in sub-folders of one folder
# - each year is stored in one sub-folder
# - within this sub-folder, the data are stored in CSV (the first skip rows can
#   be skipped---the same number in each file)
# - within each sub-folder, the following files are present:
#   - *databáze_GPS*.csv
#   - *databáze_chodci*.csv
#   - *databáze_nasledky*.csv
#   - *databáze_nehody*.csv
#   - databáze_vozidla*.csv
# - column p1 is own key (there are duplicities; it's a bug sometimes)
# - coding is in  kody1.xls
# - geolocation are columns D and E in *databaze_GPS*.xls---CRS is Křovák
#   - X is column D
#   - Y is column E
#   - both of them must be divided by 1000
#
# WARNINGS:
# - accidents that are not geolocated (columns d or e is missing in the GPS
#   table) are dropped from the tibble
# - duplicated rows are dropped
#
# notes:
# - the CSVs can be extracted from XLS by prepare_raw_accidents.sh script
read_raw_accidents <- function(folder, skip = 6) {
    read_accidents <- function(path, skip = 6) {
        accidents <- readr::read_csv(path,
                                     skip = skip,
                                     na = c("", "NA", "NULL"),
                                     col_types = cols(
                                         .default = col_integer(),
                                         p1 = col_character(),
                                         p2a = col_character()  #,
                                         # p2b = col_integer(),
                                         # p3 = col_character(),
                                         # p4a = col_double(),
                                         # p4b = col_double(),
                                         # p4c = col_double(),
                                         # p5a = col_double(),
                                         # p5b = col_double(),
                                         # p6 = col_double(),
                                         # p7 = col_double(),
                                         # p8 = col_double(),
                                         # p9 = col_double(),
                                         # p10 = col_double(),
                                         # p11 = col_double(),
                                         # p12 = col_double(),
                                         # p13a = col_double(),
                                         # p13b = col_double(),
                                         # p13c = col_double(),
                                         # p14 = col_double(),
                                         # p34 = col_double(),
                                         # p35 = col_double(),
                                         # p36 = col_double(),
                                         # p15 = col_double(),
                                         # p16 = col_double(),
                                         # p17 = col_double(),
                                         # p18 = col_double(),
                                         # p19 = col_double(),
                                         # p20 = col_double(),
                                         # p21 = col_double(),
                                         # p22 = col_double(),
                                         # p23 = col_double(),
                                         # p24 = col_double(),
                                         # p27 = col_double(),
                                         # p28 = col_double(),
                                         # p37 = col_character(),
                                         # p38 = col_character(),
                                         # p39 = col_character(),
                                         # p40 = col_character(),
                                         # p41 = col_character(),
                                         # krok = col_double(),
                                         # typ = col_double(),
                                         # dzt = col_double(),
                                         # dzl = col_double(),
                                         # dzb = col_double()
                                     )) |>
            mutate(p2a = lubridate::dmy(p2a))
    }

    read_gps <- function(path, skip = 6) {
        read_csv(path,
                 skip = 6,
                 col_types = cols(
                     .default = col_skip(),
                     p1 = col_character(),
                     # a = col_number(),
                     # b = col_number(),
                     # c = col_character(),
                     d = col_number(),
                     e = col_number()  # ,
                     # f = col_number(),
                     # g = col_number(),
                     # h = col_character(),
                     # i = col_character(),
                     # j = col_logical(),
                     # k = col_character(),
                     # l = col_character(),
                     # m = col_character(),
                     # n = col_character(),
                     # o = col_character(),
                     # p = col_character(),
                     # q = col_character(),
                     # r = col_double(),
                     # s = col_double(),
                     # t = col_character()
                 )) |>
            rename(coord_x = d, coord_y = e) |>
            mutate(coord_x = coord_x / 1e3,
                   coord_y = coord_y / 1e3)
    }

    read_one_year <- function(folder, skip = 6) {
        message(folder)
        accidents <- read_accidents(
            list.files(folder, pattern = "databáze_nehody.*csv$",
                       full.names = TRUE),
            skip = skip)
        gps <- read_gps(
            list.files(folder, pattern = "databáze_GPS.*csv$",
                       full.names = TRUE),
            skip = skip)
        dplyr::left_join(accidents, gps, by = "p1") |>
            dplyr::filter(!is.na(coord_x), !is.na(coord_y)) |>
            dplyr::distinct() |>
            sf::st_as_sf(coords = c("coord_x", "coord_y"),
                         crs = PLANARY_PROJECTION)

    }

    folders <- list.dirs(folder)[-1]
    purrr::map(folders, read_one_year, skip = skip) |>
        dplyr::bind_rows()
}



# snap points to lines ---------------------------------------------------------

# buffer_lines(lines, dist, verbose) takes lines (e.g., roads) and returns the
# polygon(s) that include the roads
#
# inputs:
# - lines (sf) ... lines, e.g., roads
# - dist (numeric scalar) ... a distance from lines (default is 100 m)
# - verbose (logical scalar) ... if TRUE, it shows the progress
#
# value:
#   the polygons that include the union of buffers around the lines
#
# WARNING: the user is not supposed to run this function directly
buffer_lines <- function(lines, dist = 100, verbose = FALSE) {
    if (verbose) message("Creating buffer for lines...")
    sf::st_buffer(lines, dist = dist) |> sf::st_union()
}


# get_points_close_to_lines(points, lines, dist, verbose) returns a logical
# vector which indicates which points lie inside a buffer around lines
#
# inputs:
# - points (sf) ... points, e.g., road accidents
# - lines (sf) ... lines, e.g., roads
# - dist (numeric scalar) ... a distance from lines (default is 100 m)
# - verbose (logical scalar) ... if TRUE, it shows the progress
#
# value:
#   a logical vector which indicates which point lies in dist distance
#   to any line
#
# WARNING: the user is not supposed to run this function directly
get_points_close_to_lines <- function(points, lines, dist = 100,
                                      verbose = FALSE) {
    buff <- buffer_lines(lines, dist = dist, verbose = verbose)
    if (verbose) message("Finding which points are in buffer...")
    sf::st_intersects(points, buff, sparse = FALSE)
}


# snap_points_to_lines(points, lines, dist, all_points = FALSE, verbose = FALSE)
# snaps points to lines
#
# inputs:
# - points (sf) ... points, e.g., road accidents
# - lines (sf) ... lines, e.g., roads
# - dist (numeric scalar) ... a distance from lines (default is 100 m)
# - all_points (logical scalar) ... if TRUE, the points outside the buffer are
#   added, otherwise they are omitted (default)
# - verbose (logical scalar) ... if TRUE, it shows the progress
#
# value:
#   a sf of points; these points are snapped to the lines; only points
#   in distance of dist or lower are snapped; the other points are either
#   omitted (if all_points == FALSE) or are left alone (if all_points == TRUE).
#   If all_points == TRUE, a new attribut is added ("valid"). It is TRUE for
#   points within the buffer and FALSE otherwise. All original attribute values
#   are kept.
#
# usage:
#   snapped_points <- snap_points_to_lines(points, lines, dist = 100)
#
# TODO: rozmyslet si, jestli potřebuju držet body, které nejsou přilepené na silnice
snap_points_to_lines <- function(points, lines, dist = 100,
                                 all_points = FALSE, verbose = FALSE) {
    inside <- get_points_close_to_lines(points, lines, dist = dist,
                                        verbose = verbose)
    buff_points <- points[inside, ]
    if (verbose) message("Finding nearest features...")
    nf <- sf::st_nearest_feature(buff_points, lines)
    if (verbose) message("Finding nearest points...")
    np <- sf::st_nearest_points(buff_points, lines[nf, ], pairwise = TRUE)
    np <- sf::st_cast(np, "POINT")[c(FALSE, TRUE)]
    if (verbose) message("Adding attributes...")
    out <- sf::st_drop_geometry(buff_points)
    out$geometry <- np
    out <- st_as_sf(out)
    if (all_points) {
        if (verbose) message("Adding points outside buffer...")
        out$valid <- TRUE
        outside <- points[!inside, ]
        outside$valid <- FALSE
        out <- dplyr::bind_rows(out, outside)
    }
    out
}



# create districts' accidents --------------------------------------------------

# create_districts_accidents() selects accidents for each district and writes
# them to disk
#
# inputs:
# - districts ... districts SF tibble
# - accidents ... (SF tibble) all accidents including their attributes
# - max_distance ... (numeric scalar) maximum distance from the (selected)
#   roads; it the distance is higher, the accident is removed from the dataset
# - map_dir ... (character scalar) path to folder there sf maps of districts are
#   stored
# - accident_dir ... (character scaler) path to folder where the new accidents
#   files should be stored
#
# value:
#   none; data are written to disk
#
# notes:
# - the accidents that are farther from roads than max_distance are removed
# - remaining accident are snapped to roads, i.e., their position is changed
#   such that they lie on a road---they are moved to their closest points on
#   their closest line
create_districts_accidents <- function(districts, accidents, max_distance,
                                       map_dir, accident_dir,
                                       workers = 1) {
    one_file <- function(input_file, output_file, accidents) {
        lines <- readr::read_rds(input_file) |>
            sfnetworks::activate("edges") |>
            st_as_sf()
        snapped_points <- snap_points_to_lines(accidents, lines,
                                               dist = max_distance)
        write_dir_rds(snapped_points, output_file)
    }

    tab <- tibble::tibble(
        input_file = file.path(map_dir, districts$sf_file_name),
        output_file = file.path(accident_dir, districts$accidents_file_name))
    PWALK(tab, one_file, accidents = accidents, workers = workers)
}
