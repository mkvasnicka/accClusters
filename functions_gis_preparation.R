# -------------------------------------
# Script:   functions_gis_preparation.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# load packages
library(dplyr, verbose = FALSE, warn.conflicts = FALSE)
library(tidyr, verbose = FALSE, warn.conflicts = FALSE)
library(purrr, verbose = FALSE, warn.conflicts = FALSE)
library(stringr, verbose = FALSE, warn.conflicts = FALSE)
library(readr, verbose = FALSE, warn.conflicts = FALSE)
library(sf, verbose = FALSE, warn.conflicts = FALSE)
library(foreign, verbose = FALSE, warn.conflicts = FALSE)


gis_dbf_profiles_filename <- function(folder = gis_dir()) {
    file.path(folder, "profiles.dbf")
}
gis_csv_profiles_filename <- function(folder = gis_dir()) {
    file.path(folder, "profiles.csv")
}

gis_suffix <- function(tab) {
    tab |>
        dplyr::distinct(district_id, profile_name, from_date, to_date) |>
        dplyr::mutate(file_name = stringr::str_c(district_id,
                                                 profile_name,
                                                 from_date, to_date,
                                                 sep = "_")) |>
        dplyr::pull(file_name)
}
gis_add_folder_and_suffix <- function(base, folder, suffix) {
    if (!is.null(suffix))
        base <- stringr::str_c(base, ".shp")
    if (!is.null(folder))
        base <- file.path(folder, base)
    base
}
gis_append_folder <- function(folder, tab) {
    if (is.null(folder))
        return(folder)
    add <- tab |>
        dplyr::distinct(district_id, profile_name, from_date, to_date) |>
        dplyr::pull(district_id)
    file.path(folder, add)
}
gis_clusters_filename <- function(tab, folder = NULL, suffix = NULL) {
    folder <- gis_append_folder(folder, tab)
    gis_add_folder_and_suffix(stringr::str_c("clusters_", gis_suffix(tab)),
                              folder, suffix)
}
gis_clustered_accidents_filename <- function(tab, folder = NULL,
                                             suffix = NULL) {
    folder <- gis_append_folder(folder, tab)
    gis_add_folder_and_suffix(stringr::str_c("clustered_accidents_",
                                             gis_suffix(tab)),
                              folder, suffix)
}


# write_to_shapefile(tbl, folder, layer) writes (geolocated) table into ESRI
# shapefile layer in folder folder
#
# inputs:
# - tbl ... (sf or tibble) (geolocated) table
# - folder ... (characater scalar) path to folder where the tbl should be
#   written
# - layer ... (characater scalar) file/layer name
#
# value:
#   none; data are writtent to disk
#
# WARNINGS:
# - this function saves to .shp; it seems that characters cannot include
#   accented letters in this format; therefore, accents are removed, see
#   write_gis_files()
write_to_shapefile <- function(tbl, folder, layer) {
    create_dir_for_file(file.path(folder, layer))
    tbl <- tbl |>
        dplyr::mutate(dplyr::across(where(is.character),
                                    ~iconv(.,
                                           from = "UTF-8",
                                           to = "ASCII//TRANSLIT")))
    sf::st_write(tbl,
                 folder,  # file.path(folder, stringr::str_c(layer, ".shp")),
                 layer = layer,
                 driver = "ESRI Shapefile",
                 delete_layer = TRUE)
}


write_gis_data <- function(tbl, folder, layer) {
    create_dir_for_file(file.path(folder, layer))
    foreign::write.dbf(as.data.frame(tbl),
                       file = file.path(folder,
                                        stringr::str_c(layer, ".dbf")))
}


# write_profiles(profiles, folder) writes profile descriptions into the gis
# shapefile folder
#
# inputs:
# - profiles ... (tibble) profiles tibble created by prepare_profiles script
# - folder ... (character scalar) path to the gis folder
#
# output:
#   none; it only writes data on the disk
#
# notes:
# - the data are in CSV nad .dbf presently
write_profiles <- function(profiles, folder) {
    profiles <- profiles |>
        tidyr::unnest(TIME_WINDOW) |>
        dplyr::mutate(
            SUPPORTED_ROAD_CLASSES =
                map_chr(SUPPORTED_ROAD_CLASSES,
                        ~stringr::str_c(., collapse = " | "))
        ) |>
        dplyr::rename(FROM_DATE = from_date, TO_DATE = to_date) |>
        dplyr::select(PROFILE_NAME, FROM_DATE, TO_DATE, everything()) |>
        as.data.frame()
    foreign::write.dbf(profiles, file = gis_dbf_profiles_filename(folder ))
    readr::write_csv(profiles, file = gis_csv_profiles_filename(folder))
}


# write_gis_files() writes all cluster information into GIS shapefiles
#
# inputs:
# - districts ... (sf tibble) districts table
# - time_window ...
# - gis_dir ... (character scalar) path to folder where GIS shapefiles will be
#   stored
# - shiny_dir ... (character scalar) path to folder where cluster (shiny) data
#   are stored
#
# value:
#   none; data are written to disk
#
# notes:
# - the result includes three layers:
#   - lixels ... table of lixels with high densities
#   - accidents ... (non-spatial) table of accidents that constitute clusters
#   - clusters ... table of clusters and their cost
# - all layers include all districts and all periods of time which clusters are
#   computed; in GIS, you shoud filter the data and/or differentiate using
#   symbols
#
# WARNINGS:
# - this function saves to .shp; it seems that this format cannot have attribute
#   names longer than 8 letters and characters cannot include accented letters;
#   therefore, attribute names are shortened and accents are removed; see
#   write_to_shapefile()
write_gis_files <- function(districts, gis_dir, shiny_dir, profiles) {
    process_one_shiny_file <- function(row, gis_dir) {
        start_logging(log_dir())
        tryCatch({
            logging::loginfo("gis prep: creating %s", row$shiny_filename)
            folder <- gis_append_folder(gis_dir, row)
            cluster_layer <- gis_clusters_filename(row)
            accident_filename <- gis_clustered_accidents_filename(row)
            cls <- readr::read_rds(row$shiny_filename)
            row <- row |>
                dplyr::select(-c(shiny_filename)) |>
                dplyr::rename(dst_id = district_id, dst_name = district_name,
                              start = from_date, end = to_date,
                              profile = profile_name)
            accidents <- dplyr::bind_cols(row, cls) |>
                dplyr::select(-clusters) |>
                tidyr::unnest(accidents) |>
                dplyr::rename(
                    acc_id = accident_id,
                    add_lix = additional_lixels,
                    cost = accident_cost)
            clusters <- dplyr::bind_cols(row, cls) |>
                dplyr::select(-accidents) |>
                tidyr::unnest(clusters) |>
                sf::st_as_sf(sf_column_name = "geometry") |>
                dplyr::select(-c(cost_per_meter, X, Y)) |>
                dplyr::rename(add_lix = additional_lixels,
                              lenght = total_length,
                              density = total_density)
            write_to_shapefile(clusters, folder, cluster_layer)
            write_gis_data(accidents, folder, accident_filename)
            logging::loginfo("gis prep: %s has been created",
                             row$shiny_filename)
        },
        error = function(e) {
            logging::logerror("gis prep failed: %s", e)
            stop("gis prep failed---stopping evaluation", call. = FALSE)
        }
        )
    }

    start_logging(log_dir())
    logging::loginfo("gis prep: checking for updates")
    tryCatch({
        districts <- districts |>
            st_drop_geometry() |>
            bind_cols(profiles |>
                          dplyr::select(PROFILE_NAME, TIME_WINDOW) |>
                          tidyr::unnest(TIME_WINDOW) |>
                          tidyr::nest(data = everything())) |>
            tidyr::unnest(data) |>
            rename(profile_name = PROFILE_NAME)
        tab <- districts |>
            dplyr::mutate(shiny_filename = shiny_file_name(districts,
                                                           folder = shiny_dir,
                                                           profile_name = profile_name,
                                                           from_date = from_date,
                                                           to_date = to_date)) |>
            dplyr::select(district_id, district_name, from_date, to_date,
                          profile_name, shiny_filename)
        if (is_behind(
            target = c(
                    gis_clusters_filename(tab, gis_dir, suffix = ".shp"),
                    gis_clustered_accidents_filename(tab, gis_dir,
                                                     suffix = ".dbf"),
                    gis_dbf_profiles_filename(gis_dir),
                    gis_csv_profiles_filename(gis_dir)
            ),
            source = tab$shiny_filename)) {
            logging::loginfo("gis prep: gis files are behind; processing %i shiny files",
                             nrow(tab))
            create_dir_for_file(gis_csv_profiles_filename(gis_dir))
            write_profiles(profiles, gis_dir)
            tab |>
                dplyr::rowwise() |>
                dplyr::group_split() |>
                purrr::walk(process_one_shiny_file, gis_dir)
            logging::loginfo("gis prep: gis files are created")
        } else {
            logging::loginfo("gis prep: everything is up-to-date---skipping")
        }
    },
    error = function(e) {
        logging::logerror("gis prep failed: %s", e)
        stop("gis prep failed---stopping evaluation")})
}
