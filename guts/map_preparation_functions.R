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
create_sf_district_roads <- function(districts, input_folder, output_folder) {
    one_file <- function(osm_file_name, sf_file_name, input_folder, output_folder) {
        input <- file.path(input_folder, osm_file_name)
        output <- file.path(output_folder, sf_file_name)
        map <- sf::st_read(input, layer = "lines")
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
