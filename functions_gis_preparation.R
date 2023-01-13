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



# file names -------------------------------------------------------------------

# name of the file where profiles are listed
# - in dBASE format
gis_dbf_profiles_filename <- function(folder = gis_dir()) {
    file.path(folder, "profiles.dbf")
}
# - in CSV
gis_csv_profiles_filename <- function(folder = gis_dir()) {
    file.path(folder, "profiles.csv")
}


# gis_suffix(tab) returns for each unique combination of district, profile, and
# time window a string that can serve as a basis of gis file names; the
# individual variable values are separated with undersore
gis_suffix <- function(tab) {
    tab |>
        dplyr::distinct(district_id, profile_name, from_date, to_date) |>
        dplyr::mutate(file_name = stringr::str_c(district_id,
                                                 profile_name,
                                                 from_date, to_date,
                                                 sep = "_")) |>
        dplyr::pull(file_name)
}


# gis_add_folder_and_suffix() take a base part of a file name and possibly adds
# a folder name and a suffix
gis_add_folder_and_suffix <- function(base, folder, suffix) {
    if (!is.null(suffix))
        base <- stringr::str_c(base, ".shp")
    if (!is.null(folder))
        base <- file.path(folder, base)
    base
}


# gis_append_folder(folder, tab) adds to a folder name subfolder taken from tab:
# it adds district descriptor for each distinct combination of district,
# profile, and time window
#
# inputs:
# - folder ... (character scalar) a path to a folder
# - tab ... (tibble) description of one cluster file
#
# output:
#   character scalar/vector of folder paths
gis_append_folder <- function(folder, tab) {
    if (is.null(folder))
        return(folder)
    add <- tab |>
        dplyr::distinct(district_id, profile_name, from_date, to_date) |>
        dplyr::pull(district_id)
    file.path(folder, add)
}


# gis_clusters_filename() and gis_clustered_accidents_filename() return file
# names of cluster shape file and accidents table respectively
#
# inputs:
# - tab ... (tibble) table describing all cluster files created given the
#   districts and profiles
# - folder ... (character scalar) if given, it is appended to the path
# - suffix ... (caracter scalar) if given, it is appended to the file name
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



# writing functions ------------------------------------------------------------

# write_to_shapefile(tbl, folder, layer) writes (geolocated) table into ESRI
# shapefile layer in folder folder
#
# inputs:
# - tbl ... (sf) (geolocated) table
# - folder ... (characater scalar) path to folder where the tbl should be
#   written
# - layer ... (characater scalar) file/layer name
#
# value:
#   none; data are written to disk
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


# write_gis_data(tbl, folder, layer) writes (non-geolocated) data in dBASE table
#
# inputs:
# - tbl ... (tibble) (non-geolocated) table
# - folder ... (characater scalar) path to folder where the tbl should be
#   written
# - layer ... (characater scalar) file/layer name
#
# value:
#   none; data are written to disk
write_gis_data <- function(tbl, folder, layer) {
    create_dir_for_file(file.path(folder, layer))
    foreign::write.dbf(as.data.frame(tbl),
                       file = file.path(folder,
                                        stringr::str_c(layer, ".dbf")))
}


# write_profiles(profiles, folder) writes profile descriptions into the major
# gis folder
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
        dplyr::select(-any_of("DISTRICTS")) |>
        as.data.frame()
    foreign::write.dbf(profiles, file = gis_dbf_profiles_filename(folder ))
    readr::write_csv(profiles, file = gis_csv_profiles_filename(folder))
}



# gis export -------------------------------------------------------------------

# write_gis_files() writes all cluster information into GIS shapefiles
#
# inputs:
# - districts ... (sf tibble) districts table
# - gis_dir ... (character scalar) path to folder where GIS shapefiles will be
#   stored
# - shiny_dir ... (character scalar) path to folder where cluster (shiny) data
#   are stored
#
# value:
#   none; data are written to disk
#
# notes:
# - one folder (database) is created for each district within gis_dir
# - for each district, the database include separate shapes for each combination
#   of profile and folder
# - there are two types of files for each combination of district, profile, and
#   time window
#   - accidents ... (non-spatial) table of accidents that constitute clusters
#   - clusters ... (spatial) shapefile of clusters and their cost
# - each of these files includes data for many combinations of cluster severity
#   and additional lixels; the analyst should filter one of these before she
#   starts her work
# - cluster ids are unique only within each combination of district, profile,
#   time window, cluster severity, and additional lixels; the same cluster id in
#   two different combinations of factors stated above have nothing in common
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
            fn <- row$shiny_filename
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
            logging::loginfo("gis prep: %s has been created", fn)
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
            logging::loginfo("gis prep: gis files have been created")
        } else {
            logging::loginfo("gis prep: everything is up-to-date---skipping")
        }
    },
    error = function(e) {
        logging::logerror("gis prep failed: %s", e)
        stop("gis prep failed---stopping evaluation")})
}
