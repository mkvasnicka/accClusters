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
require(spNetwork)
require(geojsonio)
require(jsonlite)
require(spdep)
require(igraph)
require(osmar)
require(spatstat)




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
    district |>
        sf::st_transform(crs = PLANARY_PROJECTION) |>
        sf::st_buffer(dist = buffer_size) |>
        sf::st_transform(crs = WGS84) |>
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
    ) |>
        dplyr::mutate(across(everything(), as.character)) |>
        nest_by(output, .key = "polygon") |>
        mutate(polygon = jsonlite::unbox(polygon))
    jsonlite::toJSON(list(directory = jsonlite::unbox(folder), extracts = extracts),
                     pretty = TRUE) |>
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



# reading osm via linnet -------------------------------------------------------

# convert vertices and edges back to linnet
# TODO: net$lines$marks are missing
# TODO: network$lines$markformat are different
graph_to_linnet <- function(vertices, edges, window) {
    spatstat.linnet::linnet(
        vertices = spatstat.geom::as.ppp(cbind(vertices$x, vertices$y),
                                         W = window),
        edges = cbind(edges$from, edges$to),
        sparse = TRUE)
}


# converts sf geometry to columns
# taken from https://github.com/r-spatial/sf/issues/231
sfc_as_cols <- function(x, names = c("x","y")) {
    stopifnot(inherits(x, "sf") && inherits(sf::st_geometry(x), "sfc_POINT"))
    ret <- do.call(rbind, sf::st_geometry(x))
    ret <- tibble::as_tibble(ret)
    stopifnot(length(names) == ncol(ret))
    ret <- setNames(ret,names)
    dplyr::bind_cols(x,ret)
}


# function osmar_to_linnet() convets an osmar object obj to linnet; since osmar
# is in WGS84, it must be projected
#
# inputs:
# - obj ... osmar object
# - crs ... a planary crs
#
# output:
# - linnet
osmar_to_linnet <- function(obj, crs) {
    # check inputs
    stopifnot(inherits(obj, "osmar"))
    # get vertices, remove useless stuff, and reproject
    vertices <- obj$nodes[[1]] |>  #  (taken from osmar::as_igraph())
        dplyr::select(id, lat, lon) |>
        sf::st_as_sf(coords = c("lon", "lat"), crs = WGS84) |>
        sf::st_transform(crs = crs) |>
        sfc_as_cols() |>
        sf::st_drop_geometry()
    # get edges (roads) and add reference to node row
    edges <- obj$ways[[3]]   # (taken from osmar::as_igraph())
    edges$vertex <- match(edges$ref, vertices$id)
    # break edges to from--to pairs
    edges <- edges |>
        dplyr::group_by(id) |>
        dplyr::mutate(from = vertex, to = lead(vertex)) |>
        dplyr::filter(!is.na(to))
    # convert to linnet
    window <- c(range(vertices$x), range(vertices$y))
    # linnet(vertices = as.ppp(cbind(vertices$x, vertices$y), W = window),
    #        edges = cbind(edges$from, edges$to),
    #        sparse = TRUE)
    graph_to_linnet(vertices, edges, window)
}


# function read_osm() reads osm to osmar (which does the heavy lifting)
#
# inputs:
# - path ... (character scalar) a path to a OSM file
#
# output:
# - osmar object
#
# notes:
# - it works only for non-compressed files (.osm)
# - it works for small maps (up to a district) only
read_osm <- function(path) {
    osmar::get_osm(osmar::complete_file(), source = osmar::osmsource_file(path))
}


# function read_osm_to_linnet() read a prefiltered OSM file from path and
# converts it to linnet; it is projected on the fly
#
# inputs:
# - path ... (character scalar) a path to a OSM file
# - crs ... a plannary projection
#
# output:
# - linnet object
#
# notes:
# - it works only for non-compressed files (.osm)
# - it works for small maps (up to a district) only
read_osm_to_linnet <- function(path, crs) {
    # don't join with |> -- it creates a misterious warning
    oo <- read_osm(path)
    osmar_to_linnet(oo, crs = crs)
}


# read_osm_to_sfnetwork(path, crs) reads prefiltered .osm files and converts
# them to sfnetwork
#
# inputs:
# - path ... (character scalar) path to .osm file
# - crs ... (numeric scalar) planary projection
#
# value:
#   sfnetwork
#
# REASON for this function:
#   .osm file can be read with sf:st_read() and then converted with
#   as_sfnetwork(); however, this breaks the topology of the road network and it
#   is splitted to many not-connected components; this function is a workaround
#   around this problem: osmar and spatstat do the heavy-lifting
#
# TODO: vypisuje varování -- opravit
# Warning messages:
# 1: The `x` argument of `as_tibble.matrix()` must have unique column names if `.name_repair` is omitted as of tibble 2.0.0.
# Using compatibility `.name_repair`.
# This warning is displayed once every 8 hours.
# Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.
# 2: Duplicated segments were ignored
read_osm_to_sfnetwork <- function(path, crs) {
    lnet <- read_osm_to_linnet(path, crs = crs)
    sfnetworks::as_sfnetwork(lnet, directed = FALSE, edges_as_lines = TRUE) |>
        sf::st_set_crs(crs)
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
# WARNING: This function doesn't work.
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

    # for each cluster, it gets all components of graph that consists of nodes
    # in the cluster and their direct connections in the sfnetwork edges; the
    # relationship is transitive
    get_cluster_components <- function(sfnet, clusters) {
        # checks that i and j are connected by an edge in cons; cons is tibble
        # with from and to columns
        connected <- function(cons, i, j) {
            (any(cons$to[cons$from == i] == j) ||
                any(cons$to[cons$from == j] == i)) &&
                min(c(cons$len[cons$from == i & cons$to == j],
                      cons$len[cons$from == j & cons$to == i])) <= max_distance
        }

        # computes components of graph consisting of points in a cluster; cons
        # is tibble with from and to columns; ii is a integer vector of nodes in
        # a cluster
        components <- function(cons, ii) {
            # all combinations of nodes in a cluster; drop not-connected
            # combinations
            cmbs <- t(combn(ii, 2))
            cc <- purrr::map_lgl(seq_len(nrow(cmbs)),
                                 ~connected(cons, cmbs[., 1], cmbs[., 2]))
            cmbs <- cmbs[cc, , drop = FALSE]
            # add the nodes that have been dropped---connect them to themselves
            out <- setdiff(ii, unique(as.numeric(cmbs)))
            out <- rep(out, each = 2) |> matrix(ncol = 2, byrow = TRUE)
            cmbs <- rbind(cmbs, out)
            # convert to characters (otherwise igraph adds many non-existent
            # nodes)
            cmbs <- as.character(cmbs) |> matrix(ncol = 2)
            # convert to graph and find its components
            gg <- igraph::graph_from_edgelist(cmbs, directed = FALSE)
            res <- igraph::components(gg)$membership
            as.integer(res[order(as.integer(names(res)))])
        }

        # get table of connections
        cons <- sfnet |>
            tidygraph::activate("edges") |>
            mutate(len = as.numeric(sfnetworks::edge_length())) |>
            tibble::as_tibble() |>
            sf::st_drop_geometry() |>
            dplyr::select(from, to, len)

        # ids of reals clusters, i.e., clusters with at least two nodes
        cls <- clusters[duplicated(clusters)] |> unique()

        # components vector; everything is one component at the beginning
        cmp <- purrr::rep_along(clusters, 1)
        # for each cluster, update components
        for (k in seq_along(cls)) {
            ii <- which(clusters == cls[k])
            cmp[ii] <- components(cons, ii)
        }

        # return
        cmp
    }

    # retrieve the coordinates of the nodes
    node_coords <-  sfnet |>
        tidygraph::activate("nodes") |>
        sf::st_coordinates()
    # cluster the nodes with the DBSCAN spatial clustering algorithm; nodes
    # within a distance of eps from each other will be in the same cluster; a
    # node is assigned a cluster even if it is the only member of that cluster
    clusters <- dbscan::dbscan(node_coords, eps = max_distance,
                               minPts = 1)$cluster
    # if there are no close points, return the input sfnet
    if (!any(duplicated(clusters)))
        return(sfnet)
    # contract the points
    sfnet <- sfnet |>
        # add the cluster information to the nodes of the network
        tidygraph::activate("nodes") |>
        dplyr::mutate(cls = clusters) |>
        # the sfnetwork vignette uses  mutate(cmp = group_components()), which
        # is insufficient---I want to contract only point that are
        # directly connected by an edge! they should also be connected; two
        # nodes that are close to each other but not connected, can never be
        # part of the same intersection
        dplyr::mutate(cmp = get_cluster_components(sfnet, clusters))
    # the combination of the cluster index and the component index can now be
    # used to define the groups of nodes to be contracted
    # TODO: problém: simplify = FALSE tam nechá i hrany, které má odstranit;
    #   simplify = TRUE ale odstraní i všechy smyčky (loops), což je problém!
    tidygraph::convert(sfnet, sfnetworks::to_spatial_contracted, cls, cmp,
                       simplify = FALSE)
}




# simplify_sfnetwork(net, max_distance = 0.5, dTolerance = 5) simplifies road
# sfnetwork; it does three things (in this order):
# 1. it removes pseudo-points, i.e., points of degree two, i.e., points where
#   just one edge goes into and just one edge gout out of the point
# 2. it joins the points that are too close (max_distance) to each other if they
#   are connected by an edge
# 3. it straightens the lines---it uses XXX algorithm to replace a linestring
#   with one straight line if no point of the former linestring is farther from
#   the new line than dTolerance meters
#
# inputs:
# - sf ... (projected sfnetwork) road network
# - max_distance ... (numeric scalar) maximal distance of connected points
#   which should be replace a new point (see step 2)
# - dTolerance ... (numeric scalar) maximal distance between a new line and
#   and any point in a linestring (see step 3)
#
# value:
#   projected sf; its lines are simplified, the network topology is preserved
#
# WARNIG: step 2 is not implemented yet
simplify_sfnetwork <- function(net, max_distance = 0.5, dTolerance = 5) {
    stopifnot(inherits(net, "sfnetwork"))
    net |>
        # remove pseudo points, i.e., points that are not nodes of the
        # graph/network
        remove_network_pseudo_points() |>
        # simplify intersections
        simplify_network_intersections(max_distance = max_distance) |>
        # simplify the roads, i.e., straingten the roads;
        straigthen_network_roads(dTolerance = dTolerance)
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
# - crs ... (numeric scalar) planary projection
# - max_distance ... (numeric scalar) maximal distance of connected points
#   which should be replace a new point (see step 2)
# - dTolerance ... (numeric scalar) maximal distance between a new line and
#   and any point in a linestring (see step 3)
# - workers ... (integer scalar) how many cores should be used in parallel
#
# outputs:
#   none; files are written to output_folder
#
# notes:
# - this function cannot use sf::st_read(); for the reason, see help on
#   read_osm_to_sfnetwork()
create_sf_district_roads <- function(districts, input_folder, output_folder,
                                     crs,
                                     max_distance = 0.5, dTolerance = 5,
                                     workers = 1) {
    one_file <- function(osm_file_name, sf_file_name,
                         input_folder, output_folder) {
        input <- file.path(input_folder, osm_file_name)
        output <- file.path(output_folder, sf_file_name)
        # map <- sf::st_read(input, layer = "lines") |>
        #     st_transform(crs = PLANARY_PROJECTION) |>
        #     select(-c(waterway, aerialway, barrier, man_made)) |>
        #     simplify_sf(max_distance = max_distance, dTolerance = dTolerance)
        map <- read_osm_to_sfnetwork(input, crs = crs) |>
            remove_sfnetwork_minor_components() |>
            simplify_sfnetwork(max_distance = max_distance,
                               dTolerance = dTolerance)
        write_dir_rds(map, output)
    }
    districts |>
        dplyr::select(osm_file_name, sf_file_name) |>
        sf::st_drop_geometry() |>
        PWALK(one_file,
              workers = workers,
              input_folder = input_folder,
              output_folder = output_folder)
}



# map lixelization -------------------------------------------------------------

# create_lixelized_roads() lixelizes all sf maps in input_folder
#
# inputs:
# - districts ... districts table
# - input_folder ... (character scalar) a path to folder where SF maps in .rds
#   are stored
# - output_folder ... (character scalar) a path to folder where new lixels
#   should be stored
# - lx_length ... (numeric scalar) length of lixels, see spNetwork help
# - mindist ... (numeric scalar) the minimum length of a lixel; after cut, if
#   the length of the final lixel is shorter than the minimum distance, then it
#   is added to the previous lixel, see spNetwork help
# - workers ... number of cores used; if not given, it is read from
#   NO_OF_WORKERS; if it doesn't exist, 1 core is used
# - chunk_size ... (integer scalar) size of a chunk used for multiprocessing
#   (default is 100), see spNetwork help
#
# value:
#   none; files are writen to disk
create_lixelized_roads <- function(districts, input_folder, output_folder,
                                   lx_length, mindist = NULL,
                                   workers = NULL,
                                   chunk_size = 100) {
    one_file <- function(input_path, output_path, lx_length, mindist,
                         workers, chunk_size) {
        network <- readr::read_rds(input_path)
        if (workers == 1)
            lixels <- spNetwork::lixelize_lines(network, lx_length = lx_length,
                                                mindist = mindist)
        else
            lixels <- spNetwork::lixelize_lines.mc(network,
                                                   lx_length = lx_length,
                                                   mindist = mindist,
                                                   chunk_size = chunk_size)
        lixels$len <- sf::st_length(lixels)
        write_dir_rds(lixels, output_path)
    }

    if (is.null(workers))
        workers <- if_else(exists("NO_OF_WORKERS"), NO_OF_WORKERS, 1L)

    if (workers > 1) {
        oplan <- future::plan()
        future::plan(future::multisession(workers = workers))
    }

    tibble(
        input_path = file.path(input_folder, districts$sf_file_name),
        output_path = file.path(output_folder, districts$lixel_file_name)
    ) |>
        pwalk(one_file, lx_length = lx_length, mindist = mindist,
              workers = workers, chunk_size = chunk_size)

    if (workers > 1)
        future::plan(oplan)
}


# create_lixel_samples_for_roads(districts, input_folder, output_folder) creates
# center of lixels (samples) for all districts
#
# inputs:
# - districts
# - input_folder
# - output_folder
#
# value:
#   none, data are written to disk
create_lixel_samples_for_roads <- function(districts,
                                           input_folder, output_folder) {
    one_file <- function(input_path, output_path) {
        network <- readr::read_rds(input_path)
        samples <- spNetwork::lines_center(network)
        write_dir_rds(samples, output_path)
    }
    tibble(
        input_path = file.path(input_folder, districts$lixel_file_name),
        output_path = file.path(output_folder, districts$lixel_sample_file_name)
    ) |>
        pwalk(one_file)
}



# convert maps into nb ---------------------------------------------------------

# create_sf_nb(sf) converts sf tibble of lines (or edges of sfnetwork) into
# spdep::nb
#
# inputs:
# - sf ... (sf of lines or sfnetwork)
#
# value:
#   nb, i.e., a list with one slot for each line/edge in sf (in their order);
#   each slot contains the rows of lines/edges that touch the line, i.e., are
#   its neighbors in the network
#
# notes:
# - the algorithm is taken from
#   https://stackoverflow.com/questions/62119516/generate-neighbour-list-object-for-spatial-lines-in-r
# - it is imperfect because it uses sf lines, not the true topology of the
#   networkt (however, it is blazingly fast)
create_sf_nb <- function(sf) {
    stopifnot(inherits(sf, "sf") || inherits(sf, "sfnetwork"))

        # define ad-hoc function to translate sgbp into nb (as documented in
    # https://r-spatial.github.io/spdep/articles/nb_sf.html#creating-neighbours-using-sf-objects)
    as.nb.sgbp <- function(x) {
        attrs <- attributes(x)
        x <- lapply(x, function(i) {if (length(i) == 0L) 0L else i} )
        attributes(x) <- attrs
        class(x) <- "nb"
        x
    }

    if (inherits(sf, "sfnetwork"))
        sf <- sf |> activate("edges") |> sf::st_as_sf()
    net <- sf::st_touches(sf)
    as.nb.sgbp(net)
}


# not_connected_segments(nb) returns logical values for each segment of a sf
# network that is TRUE if the edge is connected to no other edge, and FALSE
# otherwise
#
# inputs:
# - nb ... (sf, sfnetwork, or spdep::nb) network
#
# value:
#   logical vector, see above
not_connected_segments <- function(nb) {
    if (inherits(nb, "sf") || inherits(nb, "sfnetwork"))
        nb <- create_sf_nb(nb)
    stopifnot(inherits(nb, "nb"))
    purrr::map_lgl(nb, ~(0 %in% .))
}


# sfnetwork_components(net) accepts sfnetwork net and returns vector; each
# element of the vector corresponds to one node in the net; it is id (integer
# number) of component to which the node belongs to
#
# inputs:
# - net ... (sfnetwork) a line network
#
# value:
#   integer vector corresponding to the nodes in net; the nodes with the same
#   number constitute one component of the network
sfnetwork_components <- function(net) {
    net |>
        sfnetworks::activate("edges") |>
        dplyr::select(from, to) |>
        tibble::as_tibble() |>
        sf::st_drop_geometry() |>
        as.matrix() |>
        igraph::graph_from_edgelist(directed = FALSE) |>
        igraph::components() |>
        purrr::pluck("membership")
}


# remove_sfnetwork_minor_components(net) removes all minor components from the
# network
#
# inputs:
# - net ... (sfnetwork) road network
#
# value:
#   sfnetwork consisting of the major component of net
remove_sfnetwork_minor_components <- function(net) {
    stopifnot(inherits(net, "sfnetwork"))
    cls <- sfnetwork_components(net)
    largest <- cls |>
        table() |>
        sort(decreasing = TRUE) |>
        names() |>
        purrr::pluck(1) |>
        as.integer()
    nodes <- which(cls == largest)
    net |>
        activate("edges") |>
        dplyr::filter(from %in% nodes | from %in% nodes) |>
        activate("nodes") |>
        filter(!node_is_isolated())
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

# plot_not_connected_segments(sf, col, lwd) plots sf network and shows edges
# that are not connected to any other edge (this criteria is very weak)
#
# inputs:
# - sf ... (sf or sfnetwork) lines of a network
# - col ... (character scaler of color) color used to visualize not connected
#   edges
# - lwd ... (numeric scaler) line width  used to visualize not connected edges
#
# value:
#   tmap plot; if printed, it is plotted, but it can be connected with `+` with
#   other tmap plots
#
# usage:
#   plot_not_connected_segments(brno, col = "green") +
#       plot_not_connected_segments(brno_simplified, col = "red)
plot_not_connected_segments <- function(sf, col = "red", lwd = 3) {
    idx <- not_connected_segments(sf)
    tmap::tm_shape(sf) + tmap::tm_lines() +
        tmap::tm_shape(sf[idx, ]) + tmap::tm_lines(col = col, lwd = lwd)
}


# plot_out_of_major_component(sf, col, lwd) plots sf network and shows edges
# that are not part of the major comonent
#
# inputs:
# - net ... (sf or sfnetwork) lines of a network
# - col ... (character scaler of color) color used to visualize edges that are
#   disconnected from the major component
# - lwd ... (numeric scaler) line width  used to visualize edges that are
#   disconnected from the major component
#
# value:
#   tmap plot; if printed, it is plotted
plot_out_of_major_component <- function(net, col = "red", lwd = 2) {
    if (inherits(net, "sf"))
        net <- sfnetworks::as_sfnetwork(net, directed = FALSE)
    stopifnot(inherits(net, "sfnetwork"))
    cls <- sfnetwork_components(net)
    largest <- cls |>
        table() |>
        sort(decreasing = TRUE) |>
        names() |>
        pluck(1) |>
        as.integer()
    nodes <- which(cls == largest)
    sf <- net |> activate("edges") |> st_as_sf()
    plt <- tmap::tm_shape(sf) + tmap::tm_lines()
    sf <- dplyr::filter(sf, !(from %in% nodes | from %in% nodes))
    if (nrow(sf) > 0)
        plt <- plt + tmap::tm_shape(sf) + tmap::tm_lines(col = col, lwd = lwd)
    plt
}


# test_roads() test processed sfnetwork roads
#
# inputs:
# - districts ... (sf tibble) districts
# - number ... (integer scalar) which district to review
# - base_path ... (character scalar)
test_roads <- function(districts, number, max_len = 1, base_path = SF_MAPS_DIR) {
    dn <- districts$district_name[number]
    di <- districts$district_id[number]
    cat("District", dn, "-- id", di, "\n")

    path <- file.path(base_path, districts$sf_file_name[number])
    net <- read_rds(path)
    cmp <- sfnetwork_components(net) |> table() |> sort(decreasing = TRUE)
    cat("Number of components:", length(cmp), "\n")
    if (length(cmp) > 1)
        cat("Components: ", cmp, "\n")

    len <- net |> activate("edges") |> st_length() |> as.numeric()
    mlen <- sum(len < max_len)
    cat("Number of lines shorter than", max_len, "meter(s):", mlen, "\n")
    if (mlen > 0) {
        cat("Short lenghts are", sort(len[len < max_len]), "\n")
        sf <- net |> activate("edges") |> st_as_sf()
        tmm <- tmap::tmap_mode()
        tmap::tmap_mode("view")
        plt <- tm_shape(sf) + tm_lines() +
            tm_shape(sf[len < max_len, ]) + tm_lines(col = "red", lwd = 3)
        print(plt)
        tmap::tmap_mode(tmm)
    }
}



# tests ------------------------------------------------------------------------

if (FALSE) {
    # library(readr)
    # brno <- read_rds("guts/data/maps/district_40711.rds")

    # brno <- read_osm_to_linnet("guts/data/maps/district_40711.osm",
    #                            crs = PLANARY_PROJECTION)
    # brno_net <- as_sfnetwork(brno, directed = FALSE, edges_as_lines = TRUE) |>
    #     st_set_crs(PLANARY_PROJECTION)

    brno_net <- read_osm_to_sfnetwork("guts/data/maps/district_40711.osm",
                                      crs = PLANARY_PROJECTION)
    brno <- brno_net |> activate("edges") |> st_as_sf()

    system.time(brno_nb <- brno |> create_sf_nb())
    system.time(idx <- not_connected_segments(brno_nb))
    sum(idx)

    library(tmap)
    tmap_mode("view")
    # plot_not_connected_segments(brno)
    plot_out_of_major_component(brno_net)

    test_roads("guts/data/maps/district_40711.rds")


    library(readr)
    brno <- read_rds("guts/data/maps/district_40711.rds")
    brno_simp <- brno |> simplify_network_intersections(max_distance = 3)
    sf <- brno |> activate("edges") |> st_as_sf()
    sf <- sf |> mutate(len = st_length(sf))
    sf_simp <- brno_simp |> activate("edges") |> st_as_sf()
    sf_simp <- sf_simp |> mutate(len = st_length(sf_simp))
    sfx <- sf |> filter(as.numeric(len) <= 3)

    tmap_mode("view")
    tm_shape(sf) + tm_lines() +
        tm_shape(sf_simp) + tm_lines(col = "green") +
        tm_shape(sfx) + tm_lines(col = "red", lwd = 3)


}
