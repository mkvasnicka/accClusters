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

require(dplyr)
require(purrr)
require(stringr)
require(readr)
require(sf)


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
    sf::st_write(tbl, file.path(folder, stringr::str_c(layer, ".shp")),
                 layer = layer,
                 driver = "ESRI Shapefile",
                 delete_layer = TRUE)
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
write_gis_files <- function(districts, time_window, gis_dir, shiny_dir) {
    profile_name <- NULL
    if (exists("PROFILE_NAME"))
        profile_name <- PROFILE_NAME

    time_window <- handle_time_window(time_window)

    districts <- districts |> sf::st_drop_geometry()
    tab <- purrr::map(seq_len(nrow(time_window)),
                      ~(districts |>
                            dplyr::mutate(from_date = time_window$from_date[.],
                                          to_date = time_window$to_date[.]))) |>
        dplyr::bind_rows()
    tab <- tab |>
        dplyr::mutate(shiny_filename = shiny_file_name(tab, folder = shiny_dir,
                                                       profile_name = profile_name,
                                                       from_date = from_date,
                                                       to_date = to_date)) |>
        dplyr::select(-c(KOD_LAU1, KOD_KRAJ, KOD_CZNUTS3, NAZ_CZNUTS3))

    f <- function(row) {
        cls <- readr::read_rds(row$shiny_filename)
        row <- row |>
            dplyr::select(-c(shiny_filename)) |>
            dplyr::rename(dst_id = district_id, dst_name = district_name,
                          start = from_date, end = to_date)
        lixels <- dplyr::bind_cols(row, cls$lixels)
        accidents <- dplyr::bind_cols(row, cls$accidents)
        clusters <- dplyr::bind_cols(row, cls$cluster_statistics)
        list(lixels = lixels, accidents = accidents, clusters = clusters)
    }
    oo <- purrr::map(seq_len(nrow(tab)), ~f(tab[., ]))
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
