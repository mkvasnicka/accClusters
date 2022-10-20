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
        dplyr::distinct(profile_name, from_date, to_date) |>
        dplyr::mutate(file_name = stringr::str_c(profile_name,
                                                 from_date, to_date,
                                                 sep = "_")) |>
        dplyr::pull(file_name)
}
gis_clusters_filename <- function(tab) {
    stringr::str_c("clusters_", gis_suffix(tab))
}
gis_clustered_accidents_filename <- function(tab) {
    stringr::str_c("clustered_accidents_", gis_suffix(tab))
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
    process_one_shiny_file <- function(row) {
        cls <- readr::read_rds(row$shiny_filename)
        row <- row |>
            dplyr::select(-c(shiny_filename)) |>
            dplyr::rename(dst_id = district_id, dst_name = district_name,
                          start = from_date, end = to_date,
                          profile = profile_name)
        lixels <- dplyr::bind_cols(row, cls$lixels)
        accidents <- dplyr::bind_cols(row, cls$accidents)
        clusters <- dplyr::bind_cols(row, cls$cluster_statistics)
        list(lixels = lixels, accidents = accidents, clusters = clusters)
    }

    process_one_shiny_chunk <- function(tab, gis_dir) {
        oo <- purrr::map(seq_len(nrow(tab)), ~process_one_shiny_file(tab[., ]))
        lixels <- oo |> purrr::map("lixels") |>
            dplyr::bind_rows() |>
            dplyr::rename(lixel = lixel_id)
        accidents <- oo |> purrr::map("accidents") |>
            dplyr::bind_rows() |>
            dplyr::rename(cost = accident_cost)
        clusters <- oo |> purrr::map("clusters") |>
            dplyr::bind_rows() |>
            dplyr::rename(lenght = total_length,
                          density = total_density,
                          cpm = cost_per_meter)

        write_to_shapefile(lixels, gis_dir, "high_densities")
        write_to_shapefile(accidents, gis_dir, "clustered_accidents")
        write_to_shapefile(clusters, gis_dir, "clusters")
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
                file.path(gis_dir,
                          stringr::str_c(gis_clusters_filename(tab), ".shp")),
                file.path(gis_dir,
                          stringr::str_c(gis_clustered_accidents_filename(tab),
                                         ".shp")),
                gis_dbf_profiles_filename(gis_dir),
                gis_csv_profiles_filename(gis_dir)
            ),
            source = tab$shiny_filename)) {
            logging::loginfo("gis prep: gis files are behind; processing %i shiny files",
                             nrow(tab))
            create_dir_for_file(gis_csv_profiles_filename(gis_dir))
            write_profiles(profiles, gis_dir)
            tab <- tab |>
                dplyr::group_by(profile_name, from_date, to_date) |>
                dplyr::group_split()
            purrr::walk(tab, process_one_shiny_chunk, gis_dir)
            logging::loginfo("gis prep: gis files are created")
        } else {
            logging::loginfo("gis prep: everything is up-to-date---skipping")
        }
    },
    error = function(e) {
        logging::logerror("gis prep failed: %s", e)
        stop("gis prep failed---stopping evaluation")})
}
