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

require(sf)
require(dplyr)


# prepare raw police data ------------------------------------------------------

# TODO: připravit skutečná policejní data



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
                                       map_dir, accident_dir) {
    one_file <- function(input_file, output_file, accidents) {
        lines <- readr::read_rds(input_file)
        snapped_points <- snap_points_to_lines(accidents, lines,
                                               dist = max_distance)
        write_dir_rds(snapped_points, output_file)
    }

    tibble::tibble(input_file = file.path(map_dir, districts$sf_file_name),
                   output_file =
                       file.path(accident_dir, districts$accidents_file_name)) |>
        purrr::pwalk(one_file, accidents = accidents)
}
