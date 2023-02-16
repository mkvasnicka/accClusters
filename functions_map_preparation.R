# -------------------------------------
# Script:   map_preparation_functions.R
# Author:   Michal Kvasnička
# Purpose:  This script defines functions to read in roads from OpenStreetMaps.
# Inputs:   none
# Outputs:  function definitions
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# packages
library(tibble, verbose = FALSE, warn.conflicts = FALSE)
library(dplyr, verbose = FALSE, warn.conflicts = FALSE)
library(purrr, verbose = FALSE, warn.conflicts = FALSE)
library(glue, verbose = FALSE, warn.conflicts = FALSE)
library(sf, verbose = FALSE, warn.conflicts = FALSE)
library(tidygraph, verbose = FALSE, warn.conflicts = FALSE)
library(sfnetworks, verbose = FALSE, warn.conflicts = FALSE)
library(spNetwork, verbose = FALSE, warn.conflicts = FALSE)
library(geojsonio, verbose = FALSE, warn.conflicts = FALSE)
library(jsonlite, verbose = FALSE, warn.conflicts = FALSE)
library(spdep, verbose = FALSE, warn.conflicts = FALSE)
library(igraph, verbose = FALSE, warn.conflicts = FALSE)
library(osmar, verbose = FALSE, warn.conflicts = FALSE)
library(spatstat, verbose = FALSE, warn.conflicts = FALSE)


# projections
PLANARY_PROJECTION <- 5514  # Křovák
WGS84 <- 4326  # WGS84



# create district geojson -------------------------------------------------

# suppress messages when writing geojson
silent_geojson_write <- function(input, file) {
    suppressMessages(geojsonio::geojson_write(input, file = file))
}


# functions to create geojson for map filtering
#
# inputs:
# - district/districts ... (sf) a table of districts; must include district_id
#   and geometry; district can have only one row
# - buffer_size ... (numeric scalar) how much in meters should be the districts'
#   polygons enlarged
# - folder ... (character scalar) a path to folder where geojson(s) shoud be
#   written (it is created if it does not exist)
#
# output:
# - none ... the functions are run for their side effects---they write geojson(s)
#   for individual districts enlarged by some buffer to disk
write_one_district_geojson <- function(district, buffer_size, folder) {
    if (!dir.exists(folder))
        dir.create(folder)
    output_path <- file.path(folder,
                             glue("district_{district$district_id}.geojson"))
    district |>
        sf::st_transform(crs = PLANARY_PROJECTION) |>
        sf::st_buffer(dist = buffer_size) |>
        sf::st_transform(crs = WGS84) |>
        silent_geojson_write(file = output_path)
}
write_districts_geojson <- function(districts, buffer_size, folder) {
    logging::loginfo("osm maps prep: creating geojsons...")
    purrr::walk(seq_len(nrow(districts)),
                ~write_one_district_geojson(districts[., ], buffer_size,
                                            folder))
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
filter_osm_roads <- function(input_path, output_path, road_types = NULL) {
    logging::loginfo("osm maps prep: filtering roads...")
    if (is.null(road_types)) {
        system(
            glue("osmium tags-filter {input_path} nw/highway -o {output_path}",
                 " --overwrite")
        )
    } else {
        road_types <- paste(road_types, collapse = ",")
        system(
            glue::glue("osmium tags-filter {input_path} ",
                       "w/highway={road_types} -o {output_path} ",
                       "--overwrite"))
    }
}


# function create_json_do_file() creates a json config file for osmium so that
# it can process several districts in one go.
#
# inputs:
# - districts ... (sf) must include at least district_id and geometry
# - folder ... (character scalar) a path to a folder where individual geojsons
#   are stored and where the created config file is written
#
# output:
# - none ... it writes the config file
create_json_do_file <- function(districts, folder) {
    extracts <- tibble::tibble(
        output = glue::glue("district_{districts$district_id}.osm"),
        file_name = glue::glue("district_{districts$district_id}.geojson"),
        file_type = "geojson"
    ) |>
        dplyr::mutate(across(everything(), as.character)) |>
        nest_by(output, .key = "polygon") |>
        mutate(polygon = jsonlite::unbox(polygon))
    jsonlite::toJSON(list(directory = jsonlite::unbox(folder),
                          extracts = extracts), pretty = TRUE) |>
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
                                          districts_in_one_go) {
    logging::loginfo("osm maps prep: extracting districts...")
    do_all_file <- file.path(folder, "districts.json")
    for (k in seq(from = 1, to = nrow(districts), by = districts_in_one_go)) {
        idx <- k:(k + districts_in_one_go - 1)
        idx <- idx[idx <= nrow(districts)]
        logging::loginfo("osm maps prep: extracting districts %i to %i",
                         range(idx)[1], range(idx)[2])
        create_json_do_file(districts[idx, ], folder)
        system(glue("osmium extract -c {do_all_file} {input_path} --overwrite"))
    }
}


# function create_osm_district_roads() reads in OSM map for whole country and
# filters given types of roads and then creates individual maps (non-compressed)
# for each district; if buffer_size is given, it creates geojsons automatically
#
# inputs:
# - districts ... (sf) table of districts; must include at least district_id and
#   geometry (multi-polygon)
# - path_to_osm_maps ... (character scalar) a path to OSM map
# - path_to_geojsons ... (character scalar) a path where geojsons, the
#   intermediate and resulting OSM maps are written
# - profiles ... (list of lists of named variables) list of all profiles
# - districts_in_one_go ... (integer scalar) how many districts should be
#   processed in one go; if too high, the process would failed due to lack of
#   memory; 10 is ok if you have 32 GB of RAM
#
# TODO: cleaning: odstranit roads.osm a různé .json a .geojson soubory
#
# notes:
# - road_types ... (character scalar or vector or NULL) if NULL, all roads are
#   kept; if character scalar then it road types must be separated with commas;
#   if vector, it is reformated automatically
# - buffer_size ... (numeric scalar) the buffer size in meters for geojson
create_osm_district_roads <- function(districts,
                                      path_to_osm_maps,
                                      path_to_geojsons,
                                      profiles) {
    start_logging(log_dir())
    logging::loginfo("osm maps prep: checking for updates")
    districts_in_one_go <- profiles$OSMIUM_DISTRICTS_IN_ONE_GO[[1]]
    if (is_behind(osm_file_name(districts, path_to_geojsons),
                  c(path_to_osm_maps, path_to_districts(), path_to_configs()))) {
        logging::loginfo(
            "osm maps prep: osm maps are behind and will be updated")
        tryCatch({
            if (!dir.exists(path_to_geojsons))
                dir.create(path_to_geojsons)
            road_map <- file.path(path_to_geojsons, "roads.osm")
            filter_osm_roads(path_to_osm_maps, road_map,
                             road_types = profiles$SUPPORTED_ROAD_CLASSES[[1]])
            write_districts_geojson(
                districts,
                buffer_size = profiles$DISTRICT_BUFFER_SIZE[[1]],
                path_to_geojsons
            )
            filter_all_osm_district_roads(districts, road_map, path_to_geojsons,
                                          districts_in_one_go = districts_in_one_go)
        },
        error = function(e) {
            logging::logerror("osm maps prep failed: %s", e)
            stop("osm maps prep failed---stopping evaluation")})
        logging::loginfo("osm maps have been updated")
    } else {
        logging::loginfo("osm maps are up-to-date---skipping")
    }
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
            cmbs <- cmbs[cc, drop = FALSE]
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

# create_sf_district_roads(districts, path_to_osm_maps, path_to_sf_maps) creates
# SF maps from OSM maps and saves them to disk
#
# inputs:
# - districts ... (SF tibble) districts
# - path_to_osm_maps ... (character scalar) folder where district-filtered OSM
#   files live
# - path_to_sf_maps ... (character scalar) folder where district-filtered SF
#   files should be stored
# - profiles ... (list of lists of named variables) list of all profiles
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
create_sf_district_roads <- function(districts,
                                     path_to_osm_maps,
                                     path_to_sf_maps,
                                     profiles,
                                     crs = PLANARY_PROJECTION,
                                     max_distance = 0.5, dTolerance = 5,
                                     workers = profiles$NO_OF_WORKERS[[1]]) {
    one_file <- function(district_id, input_file, output_file) {
        start_logging(log_dir())
        logging::loginfo("road sfnetwork prep: creating %s", output_file)
        map <- read_osm_to_sfnetwork(input_file, crs = crs) |>
            remove_sfnetwork_minor_components() |>
            simplify_sfnetwork(max_distance = max_distance,
                               dTolerance = dTolerance)
        write_dir_rds(map, output_file, compress = TRUE)
        logging::loginfo("road sfnetwork prep: %s has been created",
                         output_file)
    }

    start_logging(log_dir())
    logging::loginfo("road sfnetwork prep: checking for updates")
    districts <- districts_behind(districts,
                                  target_fun = sf_file_name,
                                  source_fun = osm_file_name,
                                  target_folder = path_to_sf_maps,
                                  source_folder = path_to_osm_maps,
                                  other_files = c(path_to_districts(),
                                                  path_to_configs()))
    txt <- dplyr::if_else(nrow(districts) == 0, "---skipping", " in parallel")
    logging::loginfo("road sfnetwork prep: %d districts will be updated%s",
                     nrow(districts), txt)
    tryCatch({
        tab <- tibble::tibble(
            district_id = districts$district_id,
            input_file = osm_file_name(districts, path_to_osm_maps),
            output_file = sf_file_name(districts, path_to_sf_maps)
        )
        PWALK(tab, one_file,
              workers = profiles$NO_OF_WORKERS[[1]],
              ram_needed = profiles$RAM_PER_CORE_GENERAL[[1]])
        if (nrow(tab) > 0)
            logging::loginfo("road sfnetwork: road sfnetworks have been updated")
    },
    error = function(e) {
        logging::logerror("road sfnetwork prep failed: %s", e)
        stop("road sfnetwork prep failed---stopping evaluation")})
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
                                   profiles) {
    one_file <- function(district_id, input_file, output_file, lx_length, mindist) {
        start_logging(log_dir())
        logging::loginfo("lixel prep: creating %s", output_file)
        network <- readr::read_rds(input_file) |>
            sfnetworks::activate("edges") |>
            st_as_sf()
        lixels <- spNetwork::lixelize_lines(network, lx_length = lx_length,
                                            mindist = mindist)
        lixels$len <- sf::st_length(lixels)
        lixels$lixel_id <- seq_len(nrow(lixels))
        write_dir_rds(lixels, output_file, compress = TRUE)
        logging::loginfo("lixel prep: %s has been created", output_file)
    }

    start_logging(log_dir())
    logging::loginfo("lixel prep: checking for updates")
    districts <- districts_behind(districts,
                                  target_fun = lixel_file_name,
                                  source_fun = sf_file_name,
                                  target_folder = output_folder,
                                  source_folder = input_folder,
                                  other_files = c(path_to_districts(),
                                                  path_to_configs()))
    txt <- dplyr::if_else(nrow(districts) == 0, "--skipping", " in parallel")
    logging::loginfo("lixel prep: %d districts will be updated%s",
                     nrow(districts), txt)
    tryCatch({
        tab <- tibble(
            district_id = districts$district_id,
            input_file = sf_file_name(districts, input_folder),
            output_file = lixel_file_name(districts, output_folder)
        )
        PWALK(tab, one_file,
              workers = profiles$NO_OF_WORKERS[[1]],
              ram_needed = profiles$RAM_PER_CORE_GENERAL[[1]],
              lx_length = profiles$LIXEL_SIZE[[1]],
              mindist = profiles$LIXEL_MIN_DIST[[1]])
        if (nrow(tab) > 0)
            logging::loginfo("lixel prep: lixels have been updated")
    },
    error = function(e) {
        logging::logerror("lixel prep failed: %s", e)
        stop("lixel prep failed---stopping evaluation")})
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
                                           input_folder, output_folder,
                                           profiles) {
    one_file <- function(district_id, input_file, output_file) {
        start_logging(log_dir())
        logging::loginfo("lixel samples prep: creating %s", output_file)
        network <- readr::read_rds(input_file)
        samples <- spNetwork::lines_center(network)
        write_dir_rds(samples, output_file, compress = TRUE)
        logging::loginfo("lixel samples prep: %s has been created", output_file)
    }

    start_logging(log_dir())
    logging::loginfo("lixel samples prep: checking for updates")
    districts <- districts_behind(districts,
                                  target_fun = lixel_sample_file_name,
                                  source_fun = lixel_file_name,
                                  target_folder = output_folder,
                                  source_folder = input_folder,
                                  other_files = c(path_to_districts(),
                                                  path_to_configs()))
    txt <- dplyr::if_else(nrow(districts) == 0, "---skipping", " in parallel")
    logging::loginfo("lixel samples prep: %d districts will be uppdated%s",
                     nrow(districts), txt)
    tryCatch({
        tab <- tibble(
            district_id = districts$district_id,
            input_file = lixel_file_name(districts, input_folder),
            output_file = lixel_sample_file_name(districts, output_folder)
        )
        PWALK(tab, one_file,
              workers = profiles$NO_OF_WORKERS[[1]],
              ram_needed = profiles$RAM_PER_CORE_GENERAL[[1]])
        if (nrow(districts) > 0)
            logging::loginfo(
                "lixel samples prep: lixel samples have been updated")
    },
    error = function(e) {
        logging::logerror("lixel samples prep failed: %s", e)
        stop("lixel samples prep failed---stopping evaluation")})
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


# create_lixel_nbs() creates neighbors' list for each districts lixels
#
# inputs:
# - districts ... (sf tibble) table of districts
# - input_folder ... (character scalar) folder where lixels are stored
# - output_folder ... (character scalar) folder where nbs should be written to
# - workers ... (numeric scalar) number of cores used for parallel computation
#
# value:
#   none, data are written to disk
create_lixel_nbs <- function(districts, input_folder, output_folder,
                             profiles) {
    one_district <- function(district_id, input_file, output_file) {
        start_logging(log_dir())
        logging::loginfo("lixel nbs prep: creating %s", output_file)
        lixels <- readr::read_rds(input_file)
        nb <- create_sf_nb(lixels)
        write_dir_rds(nb, output_file, compress = TRUE)
        logging::loginfo("lixel nbs prep: %s has been created", output_file)
    }

    start_logging(log_dir())
    logging::loginfo("lixel nbs prep: checking for updates")
    districts <- districts_behind(districts,
                                  target_fun = lixel_nb_file_name,
                                  source_fun = lixel_file_name,
                                  target_folder = output_folder,
                                  source_folder = input_folder,
                                  other_files = c(path_to_districts(),
                                                  path_to_configs()))
    txt <- if_else(nrow(districts) == 0, "---skipping", " in parallel")
    logging::loginfo("lixel nbs prep: %d will be updated%s",
                     nrow(districts), txt)
    tryCatch({
        tab <- tibble::tibble(
            district_id = districts$district_id,
            input_file = lixel_file_name(districts, input_folder),
            output_file = lixel_nb_file_name(districts, output_folder)
        )
        PWALK(tab, one_district,
              workers = profiles$NO_OF_WORKERS[[1]],
              ram_needed = profiles$RAM_PER_CORE_GENERAL[[1]])
        if (nrow(districts) > 0)
            logging::loginfo("lixel nbs  have been updated")
    },
    error = function(e) {
        logging::logerror("lixel nbs prep failed: %s", e)
        stop("lixel nbs prep failed---stopping evaluation")})
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
