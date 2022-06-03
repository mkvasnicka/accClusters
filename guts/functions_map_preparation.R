# -------------------------------------
# Script:   map_preparation_functions.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:   none
# Outputs:  functions
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

require(tibble)
require(dplyr)
require(purrr)
require(glue)
require(sf)
require(tidygraph)
require(sfnetworks)
require(geojsonio)
require(jsonlite)



# create district geojson -------------------------------------------------

# suppress messages when writing geojson
silent_geojson_write <- function(input, file)
    suppressMessages(geojsonio::geojson_write(input, file = file))


# functions to create geojson for map filtering
#
# inputs:
# - district/districts ... (sf) a table of districts; must include district_id
#   and geometry; district can have only one row
# - buffer_size ... (numeric scalar) how much in meters should be the districts'
#   polygons enlarged
# - folder ... (character scalar) a path to folder where geojson(s) shoud be
#   written (it is created if it does not exist)
# - verbose ... (logical, default FALSE) whether progress bar is created
# - pb ... don't use; just for internal purposes
#
# output:
# - none ... the functions are run for their side effects---they write geojson(s)
#   for individual districts enlarged by some buffer to disk
write_one_district_geojson <- function(district, buffer_size, folder, pb) {
    if (!dir.exists(folder))
        dir.create(folder)
    output_path <- file.path(folder,
                             glue("district_{district$district_id}.geojson"))
    district %>%
        sf::st_transform(crs = planary_projection) %>%
        sf::st_buffer(dist = buffer_size) %>%
        sf::st_transform(crs = wgs_projection) %>%
        silent_geojson_write(file = output_path)
    if (!is.null(pb))
        pb$tick(1)
}
write_districts_geojson <- function(districts, buffer_size, folder,
                                    verbose = FALSE) {
    if (verbose) {
        message("Creating geojsons...")
        pb <- progress_bar$new(format = "  creating geojson [:bar] :current/:total in :elapsed eta: :eta",
                               total = nrow(districts), clear = FALSE, width = 60)
        pb$tick(0)
    } else {
        pb <- NULL
    }
    purrr::walk(seq_len(nrow(districts)),
                ~write_one_district_geojson(districts[., ], buffer_size, folder, pb))
}



# transform osm maps ------------------------------------------------------


# function filter_osm_roads() reads in an OSM map from input path, filters
# selected roads, and writes the result to outputpath
#
# inputs:
# - input_path ... (character scalar) a path the input OSM map should be read
#   from
# - output_path ... (character scalar) a path to which the filtered map should
#   be written to
# - road_types ... (character scalar or vector) selected types of roads to be
#   kept in the map; if one string, then the types must be separated by commas;
#   if not given, all road are kept
#
# output:
# - none ... the function works for its side effect---it writes the map to disk
filter_osm_roads <- function(input_path, output_path, road_types = NULL,
                             verbose = FALSE) {
    if (verbose)
        message("Filtering roads...")
    if (is.null(road_types)) {
        system(
            glue("osmium tags-filter {input_path} nw/highway -o {output_path}")
        )
    } else {
        road_types <- paste(road_types, collapse = ",")
        system(
            glue::glue("osmium tags-filter {input_path} ",
                       "w/highway={road_types} -o {output_path}"))
    }
}


# function filter_district_roads() reads an osm map from input_path and create
# one osm file for each district
#
# DEPRACATED: use filter_all_osm_district_roads() instead!
#
# inputs:
# - district/districts ... (sf) must include at least district_id and geometry;
#   if district, then it must include only one row
# - input_path ... (character scalar) a path to an OSM map; it is expected that
#   it includes only selected roads, i.e. is prefiltered with filter_osm_roads()
# - folder ... a folder where geojsons are present and where the function should
#   write the individual osm(s)
# - verbose ... (logical, default FALSE) whether progress bar is created
# - pb ... don't use; just for internal purposes
#
# output:
# - none ... the functions only write the individual osm(s)
#
# notes:
# - osmium uses strategy "complete_ways" which secures that all necessary nodes
#   are included in the map, i.e. it is reference-complete, see
#   https://osmcode.org/osmium-tool/manual.html#creating-geographic-extracts;
#   the price for this is that the source file is read twice for each resulting
#   map; the time can be saved by switching the strategy to "simple"; in such a
#   case, however, the osmar_to_linnet() function would have to be augmented;
#   moreover, it may work only when the buffer is large enough
# - maybe the time could be saved if all extractions are done in one geojson
#   file---perhaps osmium could process all districts at once---check it!
filter_osm_one_district_roads <- function(district, input_path, folder, pb = NULL) {
    geojson <- file.path(folder, glue("district_{district$district_id}.geojson"))
    outfile <- file.path(folder, glue("district_{district$district_id}.osm"))
    system(glue("osmium extract -p {geojson} {input_path} -o {outfile}"))
    if (!is.null(pb))
        pb$tick(1)
}
filter_osm_district_roads <- function(districts, input_path, folder,
                                      verbose = FALSE) {
    if (verbose) {
        message("Extracting districts...")
        pb <- progress_bar$new(format = "  creating osm [:bar] :current/:total in :elapsed eta: :eta",
                               total = nrow(districts), clear = FALSE, width = 60)
        pb$tick(0)
    } else {
        pb <- NULL
    }
    purrr::walk(seq_len(nrow(districts)),
                ~filter_osm_one_district_roads(districts[., ], input_path, folder, pb))
}


# function create_json_do_file() creates a json config file for osmium so that
# it can process several districts in one go.
#
# inputs:
# - districts ... (sf) must include at least district_id and geometry
# - folder ... (character scalar) a path to a folder where individual geojsons
#   are stored and where the created config file is written
# - verbose ... (logical, default FALSE) whether verbose
#
# output:
# - none ... it writes the config file
create_json_do_file <- function(districts, folder, verbose) {
    if (verbose)
        message("Creating do all json...")
    extracts <- tibble::tibble(
        output = glue::glue("district_{districts$district_id}.osm"),
        file_name = glue::glue("district_{districts$district_id}.geojson"),
        file_type = "geojson"
    ) %>%
        dplyr::mutate(across(everything(), as.character)) %>%
        nest_by(output, .key = "polygon") %>%
        mutate(polygon = jsonlite::unbox(polygon))
    jsonlite::toJSON(list(directory = jsonlite::unbox(folder), extracts = extracts),
                     pretty = TRUE) %>%
        silent_geojson_write(file = file.path(folder, "districts.json"))
}


# function filter_all_osm_district_roads() extracts many district from one OSM
# map
#
# inputs:
# - districts ... (sf) must include at least district_id and geometry
# - input_path ... (character scalar) a path where an OSM map is; it is expected
#   that the map has been processed with filter_osm_roads() first
# - folder ... (character scalar) a path to a folder where individual geojsons
#   and config file are stored and where the created created maps are written
# - districts_in_one_go ... (integer scalar) how many districts should be
#   processed in one go; if too high, the process would failed due to lack of
#   memory; 10 is ok if you have 32 GB of RAM
# - verbose ... (logical, default FALSE) whether verbose
#
# output:
# - none ... it writes extracts from OSM maps (non-compressed)
#
# notes:
# - osmium uses strategy "complete_ways" which secures that all necessary nodes
#   are included in the map, i.e. it is reference-complete, see
#   https://osmcode.org/osmium-tool/manual.html#creating-geographic-extracts;
#   the price for this is that the source file is read twice for each resulting
#   map; the time can be saved by switching the strategy to "simple"; in such a
#   case, however, the osmar_to_linnet() function would have to be augmented;
#   moreover, it may work only when the buffer is large enough
filter_all_osm_district_roads <- function(districts, input_path, folder,
                                          districts_in_one_go,
                                          verbose = FALSE) {
    if (verbose)
        message("Extracting districts...")
    if (verbose && nrow(districts) > districts_in_one_go) {
        pb <- progress_bar$new(format = "  creating osm [:bar] :current/:total in :elapsed eta: :eta",
                               total = nrow(districts), clear = FALSE, width = 60)
        pb$tick(0)
    } else {
        pb <- NULL
    }
    do_all_file <- file.path(folder, "districts.json")
    for (k in seq(from = 1, to = nrow(districts), by = districts_in_one_go)) {
        idx <- k:(k + districts_in_one_go - 1)
        idx <- idx[idx <= nrow(districts)]
        create_json_do_file(districts[idx, ], folder, verbose = FALSE)
        system(glue("osmium extract -c {do_all_file} {input_path}"))
        if (!is.null(pb))
            pb$tick(districts_in_one_go)
    }
}


# function create_osm_district_roads() reads in OSM map for whole country and
# filters given types of roads and then creates individual maps (non-compressed)
# for each district; if buffer_size is given, it creates geojsons automatically
#
# inputs:
# - districts ... (sf) table of districts; must include at least district_id and
#   geometry (multi-polygon)
# - input_path ... (character scalar) a path to OSM map
# - road_types ... (character scalar or vector or NULL) if NULL, all roads are
#   kept; if character scalar then it road types must be separated with commas;
#   if vector, it is reformated automatically
# - buffer_size ... (numeric scalar) the buffer size in meters for geojson
# - folder ... (character scalar) a path where geojsons, the intermediate and
#   resulting OSM maps are written
# - districts_in_one_go ... (integer scalar) how many districts should be
#   processed in one go; if too high, the process would failed due to lack of
#   memory; 10 is ok if you have 32 GB of RAM
# - verbose ... (logical, default is FALSE) verbose or not
#
# TODO: verbose nefunguje -- chybí progress bar
# TODO: cleaning: odstranit roads.osm a různé .json a .geojson soubory
create_osm_district_roads <- function(districts, input_path, road_types = NULL,
                                      buffer_size, folder, districts_in_one_go = 10,
                                      verbose = FALSE) {
    if (!dir.exists(folder))
        dir.create(folder)
    road_map <- file.path(folder, "roads.osm")
    filter_osm_roads(input_path, road_map, road_types, verbose = verbose)
    write_districts_geojson(districts, buffer_size, folder, verbose)
    # old version: each district is processed by itself -- slow but conserves
    # memory
    # # create_json_do_file(districts, folder, verbose = verbose)
    # # filter_osm_district_roads(districts, road_map, folder, verbose = verbose)
    # new version: districts_in_one_go districts are processed in one go -- it
    # faster but needs a lot more memory
    filter_all_osm_district_roads(districts, road_map, folder,
                                  districts_in_one_go = districts_in_one_go,
                                  verbose = verbose)
}



# sf simplification ------------------------------------------------------------

# remove_network_pseudo_points(sfnet) removes nodes from the network that are
# not necessary, i.e., that are degree 2 (only one edge is going in and one edge
# is going out)
#
# inputs:
# - sfnet ... (sfnetwork) sfnetwork
#
# output:
# sfnetwork with only necessary network nodes
remove_network_pseudo_points <- function(sfnet) {
    stopifnot(inherits(sfnet, "sfnetwork"))
    sfnet |> tidygraph::convert(sfnetworks::to_spatial_smooth)
}


# straigthen_network_roads(sfnet, dTolerance) simplifies, i.e., straightens
# roads in the sfnet network
#
# inputs:
# - sfnet ... (sfnetwork) sfnetwork
# - dTolerance ... (numeric scalar) is a tolerance in meters, i.e., the maximum
#   permissible deviation from a straight line
straigthen_network_roads <- function(sfnet, dTolerance = 5) {
    stopifnot(inherits(sfnet, "sfnetwork"))
    sfnet |>
        sfnetworks::activate("edges") |>
        sf::st_simplify(preserveTopology = TRUE, dTolerance = dTolerance)
}


# simplify_network_intersections(sfnet, max_distance) contracts close nodes in
# the network, i.e., it simplifies the intersections
#
# inputs:
# - sfnet ... (sfnetwork) sfnetwork
# - max_distance ... (numeric scalar) a maximum distnace between nodes in meters
#   for the points to be contracted
#
# output:
# sfnet with contracted points
#
# usage:
# sfnet <- simplify_network_intersections(sfnet, max_distance = 5)
#
# WARNING: This function may introduce "lifts in mixmasters", i.e., it may
# connect off-grade intersection that were not previously connected.
# Therefore, the function is temporarily switched off.
#
# A test examples:
#
# The first case should be connected:
# p11 <- st_point(c(0, 0))
# p12 <- st_point(c(1, 1))
# p13 <- st_point(c(1, 1.1))
# p14 <- st_point(c(3, 2))
# p21 <- st_point(c(0,2))
# lines <- st_sfc(st_linestring(c(p11, p12)),
#                 st_linestring(c(p12, p13)),
#                 st_linestring(c(p13, p14)),
#                 st_linestring(c(p12, p21)))
#
# The second case should NOT be connected
# p11 <- st_point(c(-1, 0))
# p12 <- st_point(c(0.1, 0))
# p13 <- st_point(c(1, 0))
# p21 <- st_point(c(0, -1))
# p22 <- st_point(c(0, 0.1))
# p23 <- st_point(c(0,1))
# lines <- st_sfc(st_linestring(c(p11, p12)),
#                 st_linestring(c(p12, p13)),
#                 st_linestring(c(p21, p22)),
#                 st_linestring(c(p22, p23)),
#                 st_linestring(c(p11, p21)))
#
# # test
# net = as_sfnetwork(lines)
#
# edge_colors = function(x) rep(sf.colors(12, categorical = TRUE)[-2], 2)[c(1:ecount(x))]
#
# plot(st_geometry(net, "edges"), col = edge_colors(net), lwd = 4)
# plot(st_geometry(net, "nodes"), pch = 20, cex = 2, add = TRUE)
#
# net2 <- simplify_network_intersections(net, 0.2) |>
#     straigthen_network_roads(dTolerance = 0.3)
#
# plot(st_geometry(net2, "edges"), col = edge_colors(net), lwd = 4)
# plot(st_geometry(net2, "nodes"), pch = 20, cex = 2, add = TRUE)
#
simplify_network_intersections <- function(sfnet, max_distance = 0.5) {
    # TODO: remove the following line as soon as the function is improved
    return(sfnet)
    stopifnot(inherits(sfnet, "sfnetwork"))
    # retrieve the coordinates of the nodes
    node_coords <-  sfnet %>%
        activate("nodes") %>%
        st_coordinates()
    # cluster the nodes with the DBSCAN spatial clustering algorithm;
    # nodes within a distance of eps from each other will be in the same cluster;
    # a node is assigned a cluster even if it is the only member of that cluster
    clusters <- dbscan(node_coords, eps = max_distance, minPts = 1)$cluster
    # if there are no close points, return the input sfnet
    if (!any(duplicated(clusters)))
        return(sfnet)
    # contract the points
    sfnet <- sfnet %>%
        # add the cluster information to the nodes of the network
        activate("nodes") %>%
        mutate(cls = clusters) %>%
        # TODO: the following is probably insufficient---I want to contract only
        #   point that are directly connected by an edge!
        # they should also be connected; two nodes that are close to each other
        # but not connected, can never be part of the same intersection
        mutate(cmp = group_components())
    # the combination of the cluster index and the component index can now be
    # used to define the groups of nodes to be contracted
    convert(sfnet, to_spatial_contracted, cls, cmp, simplify = TRUE)
}


# simplify_sf(sf, max_distance = 0.5, dTolerance = 5) simplifies road sf tibble;
# it does three things (in this order):
# 1. it removes pseudo-points, i.e., points of degree two, i.e., points where
#   just one edge goes into and just one edge gout out of the point
# 2. it joins the points that are too close (max_distance) to each other if they
#   are connected by an edge
# 3. it straightens the lines---it uses XXX algorithm to replace a linestring
#   with one straight line if no point of the former linestring is farther from
#   the new line than dTolerance meters
#
# inputs:
# - sf ... (projected sf tibble of lines) road network
# - max_distance ... (numeric scalar) maximal distance of connected points
#   which should be replace a new point (see step 2)
# - dTolerance ... (numeric scalar) maximal distance between a new line and
#   and any point in a linestring (see step 3)
#
# value:
#   projected sf; its lines are simplified, the network topology is preserved
#
# WARNIG: step 2 is not implemented yet
simplify_sf <- function(sf, max_distance = 0.5, dTolerance = 5) {
    stopifnot(inherits(sf, "sf"))
    # convert to sfnetwork
    sfnet <- sfnetworks::as_sfnetwork(sf)
    # start simplification
    sfnet <- sfnet |>
        # remove pseudo points, i.e., points that are not nodes of the
        # graph/network
        remove_network_pseudo_points() |>
        # simplify intersections
        simplify_network_intersections(max_distance = max_distance) |>
        # simplify the roads, i.e., straingten the roads;
        straigthen_network_roads(dTolerance = dTolerance)
    # convert back to sf
    sfnet |> sfnetworks::activate("edges") |> sf::st_as_sf()
}



# read osm as sf ---------------------------------------------------------------

# create_sf_district_roads(districts, input_folder, output_folder) creates SF
# maps from OSM maps and saves them to disk
#
# inputs:
# - districts ... (SF tibble) districts
# - input_folder ... (character scalar) folder where district-filtered OSM files
#   live
# - output_folder ... (character scalar) folder where district-filtered SF files
#   should be stored
#
# outputs:
#   none; files are written to output_folder
create_sf_district_roads <- function(districts, input_folder, output_folder,
                                     max_distance = 0.5, dTolerance = 5) {
    one_file <- function(osm_file_name, sf_file_name,
                         input_folder, output_folder) {
        input <- file.path(input_folder, osm_file_name)
        output <- file.path(output_folder, sf_file_name)
        map <- sf::st_read(input, layer = "lines") |>
            st_transform(crs = planary_projection) |>
            select(-c(waterway, aerialway, barrier, man_made)) |>
            simplify_sf(max_distance = max_distance, dTolerance = dTolerance)
        write_dir_rds(map, output)
    }
    districts |>
        dplyr::select(osm_file_name, sf_file_name) |>
        sf::st_drop_geometry() |>
        purrr::pwalk(one_file,
                     input_folder = input_folder,
                     output_folder = output_folder)
    # one_file("guts/data/maps/district_40711.osm", xxx)
}



# test maps --------------------------------------------------------------------

# test_sf_maps(districts, sf_maps_dir) returns some basic statistics about the
# road networks prepared by create_sf_district_roads()
#
# inputs:
# - districts ... a district table
# - sf_maps_dir ... (character scalar) path to the folder where SF maps in .rds
#   are stored
#
# available statistics:
# - lt#m ... the number of lines shorter than # meters
# - total ... the toal number of lines
#
# TODO: počet komponent sítě; testy geometrie ala Gelb
test_sf_maps <- function(districts, sf_maps_dir) {
    one_file <- function(path) {
        oo <- readRDS(path)
        len <- oo |> sf::st_length() |> as.numeric()
        tibble(lt1m = sum(len < 1),
               lt3m = sum(len < 3),
               lt5m = sum(len < 5),
               lt10m = sum(len < 10),
               lt20m = sum(len < 20),
               total = length(len))
    }
    paths <- file.path(sf_maps_dir, districts$sf_file_name)
    tab <- purrr::map_dfr(paths, one_file)
    dplyr::bind_cols(districts, tab)
}
