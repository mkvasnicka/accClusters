# -------------------------------------
# Script:   functions_accidents_preparation.R
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

# packages
library(dplyr, verbose = FALSE, warn.conflicts = FALSE)
library(purrr, verbose = FALSE, warn.conflicts = FALSE)
library(tidyr, verbose = FALSE, warn.conflicts = FALSE)
library(readr, verbose = FALSE, warn.conflicts = FALSE)
library(stringr, verbose = FALSE, warn.conflicts = FALSE)
library(lubridate, verbose = FALSE, warn.conflicts = FALSE)
library(sf, verbose = FALSE, warn.conflicts = FALSE)


# projections
PLANARY_PROJECTION <- 5514  # Křovák
WGS84 <- 4326  # WGS84



# prepare raw police data ------------------------------------------------------

# read_raw_accidents_files() reads accidents files
#
# inputs:
# - path ... (character vector) paths to files with major accident tables in CSV
# - skip ... (round nonnegative numeric scalar) how many first rows in CSV
#   should be skipped
#
# value:
#    tibble
read_raw_accidents_files <- function(path, skip) {
    readr::read_csv(path,
                    skip = skip,
                    col_types = readr::cols(
                        .default = col_integer(),
                        p1 = col_character(),
                        p2a = col_character(),
                        p2b = col_character()
                    )
    ) |>
        dplyr::select(
            p1,   # ID nehody
            p2a,  # datum
            p2b,  # čas
            p6,   # druh nehody
            p8,   # Srážky
            p10,  # zavinění
            p11,  # alkohol u viníka
            p12,  # Příčina nehody
            p13a, # Následky: usmrceno
            p13b, # Následky: těžce zraněno osob
            p13c, # Následky: lehce zraněno osob
            p14   # Následky: hmotná škoda
        ) |>
        dplyr::mutate(
            p2b = stringr::str_pad(p2b, width = 4, side = "left", pad = "0"),
            p2b = ifelse(
                stringr::str_sub(p2b, 3, 3) == 6,
                stringr::str_c(str_sub(p2b, 1, 2), "0", str_sub(p2b, 4)),
                p2b
            ),
            p2b = ifelse(p2b == "2500", "2400", p2b),
            p2 = stringr::str_c(p2a, " ",
                                stringr::str_sub(p2b, 1, 2), ":",
                                stringr::str_sub(p2b,3,4)) |>
                lubridate::as_datetime(format = "%d.%m.%Y %H:%M")
        ) |>
        dplyr::select(-p2b, -p2a)
}


# read_raw_gps_files() reads GPS files
#
# inputs:
# - path ... (character vector) paths to files with major GPS tables in CSV
# - skip ... (round nonnegative numeric scalar) how many first rows in CSV
#   should be skipped
#
# value:
#    tibble
read_raw_gps_files <- function(path, skip) {
    readr::read_csv(path,
                    skip = skip,
                    col_types = readr::cols(.default = col_character())
    ) |>
        dplyr::mutate(
            d = stringr::str_replace_all(d, ",", ".") %>% as.double(),
            e = stringr::str_replace_all(e, "," ,".") %>% as.double()
        ) |>
        dplyr::select(p1, d, e) |>
        dplyr::rename(coord_x = d, coord_y = e)
}


# read_raw_outcomes_files() reads outcomes files
#
# inputs:
# - path ... (character vector) paths to files with major outcomes tables in CSV
# - skip ... (round nonnegative numeric scalar) how many first rows in CSV
#   should be skipped
#
# value:
#    tibble
read_raw_outcomes_files <- function(path, skip) {
    readr::read_csv(path,
                    skip = skip,
                    col_types = readr::cols(
                        .default = col_integer(),
                        p1 = col_character()
                    ))
        # dplyr::filter(p59g == 1) |>
        # dplyr::left_join(., data_vozidla_type, by = c("p1", "id_vozidla") ) |>
        # dplyr::mutate(
        #     type = dplyr::case_when(
        #         p44 %in% c(0,1,2) ~ "motobike",
        #         p44 == 13 ~ "bike",
        #         p44 %in% c(3,4) ~ "car",
        #         p44 %in% c(5,6,7) ~ "truck",
        #         TRUE ~ "other"
        #     ),
        #     driver = ifelse(p59a == 1, "driver", "crew")
        # ) |>
        # dplyr::group_by(p1, type, driver) %>%
        # dplyr::summarise(
        #     obs = n(),
        #     .groups = "drop"
        # ) |>
        # tidyr::pivot_wider(
        #     names_from = c(type, driver),
        #     values_from = obs,
        #     id_cols = p1,
        #     names_sep = "_",
        #     names_prefix = "casualties_",
        #     values_fill = 0
        # )


    # TODO: vlastní funkce! + není duplicita dále?
    # data_vozidla_type <-
    #     readr::read_csv(path,
    #                     skip = skip,
    #                     col_types = cols(
    #                         .default = col_integer(),
    #                         p1 = col_character()
    #                     )
    #     ) |>
    #     dplyr::distinct(p1, id_vozidla, p44)

    # readr::read_csv(path,
    #                 skip = skip,
    #                 col_types = readr::cols(
    #                     .default = col_integer(),
    #                     p1 = col_character()
    #                 )) |>
    #     dplyr::filter(p59g == 1) |>
    #     dplyr::left_join(., data_vozidla_type, by = c("p1", "id_vozidla") ) |>
    #     dplyr::mutate(
    #         type = dplyr::case_when(
    #             p44 %in% c(0,1,2) ~ "motobike",
    #             p44 == 13 ~ "bike",
    #             p44 %in% c(3,4) ~ "car",
    #             p44 %in% c(5,6,7) ~ "truck",
    #             TRUE ~ "other"
    #         ),
    #         driver = ifelse(p59a == 1, "driver", "crew")
    #     ) |>
    #     dplyr::group_by(p1, type, driver) %>%
    #     dplyr::summarise(
    #         obs = n(),
    #         .groups = "drop"
    #     ) |>
    #     tidyr::pivot_wider(
    #         names_from = c(type, driver),
    #         values_from = obs,
    #         id_cols = p1,
    #         names_sep = "_",
    #         names_prefix = "casualties_",
    #         values_fill = 0
    #     )
}


# read_raw_age_files() reads age files
#
# inputs:
# - path ... (character vector) paths to files with major age tables in CSV
# - skip ... (round nonnegative numeric scalar) how many first rows in CSV
#   should be skipped
#
# value:
#    tibble
# read_raw_age_files <- function(path, skip) {
#     readr::read_csv(path,
#                     skip = 6,
#                     col_types = readr::cols(
#                         .default = col_integer(),
#                         p1 = col_character()
#                     )) |>
#         # the first vehicle only
#         dplyr::filter(id_vozidla == 1) |>
#         # data for drivers only
#         dplyr::filter(p59a == 1) |>
#         dplyr::mutate(
#             vek = ifelse(
#                 p59d <= (year(today()) - 2000),
#                 2000 + p59e,
#                 1900 + p59e
#             ),
#             vek = vek - year(p2)
#         ) |>
#         dplyr::select(p1,driver_age = vek)
# }


# read_raw_pedestrians_files() reads pedestrians files
#
# inputs:
# - path ... (character vector) paths to files with major pedestrians tables in
#   CSV
# - skip ... (round nonnegative numeric scalar) how many first rows in CSV
#   should be skipped
#
# value:
#    tibble
read_raw_pedestrians_files <- function(path, skip) {
    readr::read_csv(path,
                    skip = skip,
                    col_types = readr::cols(
                        .default = col_integer(),
                        p1 = col_character()
                    )) |>
        dplyr::group_by(p1) |>
        dplyr::summarise(
            casualties_pedestrian = sum(p33g == 1, na.rm = TRUE)
        ) |>
        mutate(involved_pedestrian = TRUE)
}


# read_raw_vehicles_files() reads vehicles files
#
# inputs:
# - path ... (character vector) paths to files with major vehicles tables in CSV
# - skip ... (round nonnegative numeric scalar) how many first rows in CSV
#   should be skipped
#
# value:
#    tibble
read_raw_vehicles_files <- function(path, skip) {
    readr::read_csv(path,
                    skip = skip,
                    col_types = cols(
                        .default = col_integer(),
                        p1 = col_character()
                    )
    )

    # readr::read_csv(path,
    #                 skip = skip,
    #                 col_types = cols(
    #                     .default = col_integer(),
    #                     p1 = col_character()
    #                 )
    # ) |>
    #     dplyr::mutate(
    #         vehicle_bike = p44 == 13,
    #         vehicle_motobike = p44 %in% c(0,1,2)
    #     ) |>
    #     dplyr::group_by(p1) |>
    #     dplyr::summarise(
    #         involved_bike = any(vehicle_bike),
    #         involved_motobike = any(vehicle_motobike),
    #         .groups = "drop"
    #     )
}


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
# - data on individual years are stored separately in CSVs in form
#   - ###_databaze_GPS*.csv
#   - ###_databaze_chodci*.csv
#   - ###_databaze_nasledky*.csv
#   - ###_databaze_nehody*.csv
#   - ###_databaze_vozidla*.csv
#   where #### is year (e.g., 2021)
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
#
# TODO: standardizovat názvy polí nehodách
read_raw_accidents <- function(folder, profiles) {

    accidents <- purrr::map(
        list.files(path = folder,
                   pattern = profiles$ACCIDENTS_FILE_NAME_PATTERN[[1]],
                   full.names = TRUE),
        read_raw_accidents_files,
        skip = profiles$ACCIDENTS_FILES_SKIP[[1]]
    ) |>
        dplyr::bind_rows()

    gps <- purrr::map(
        list.files(path = folder,
                   pattern = profiles$ACCIDENTS_GPS_FILE_NAME_PATTERN[[1]],
                   full.names = TRUE),
        read_raw_gps_files,
        skip = profiles$ACCIDENTS_FILES_SKIP[[1]]
    ) |>
        dplyr::bind_rows()

    outcomes <- purrr::map(
        list.files(path = folder,
                   pattern = profiles$ACCIDENTS_OUTCOMES_FILE_NAME_PATTERN[[1]],
                   full.names = TRUE),
        read_raw_outcomes_files,
        skip = profiles$ACCIDENTS_FILES_SKIP[[1]]
    ) |>
        dplyr::bind_rows()

    # age <- purrr::map(list.files(path = folder,
    #                              pattern = ACCIDENTS_FILE_NAME_PATTERN,
    #                              full.names = TRUE),
    #                   read_raw_age_files, skip = skip) |>
    #     dplyr::bind_rows()

    pedestrians <- purrr::map(
        list.files(path = folder,
                   pattern = profiles$ACCIDENTS_PEDESTRIANS_FILE_NAME_PATTERN[[1]],
                   full.names = TRUE),
        read_raw_pedestrians_files,
        skip = profiles$ACCIDENTS_FILES_SKIP[[1]]
    ) |>
        dplyr::bind_rows()

    vehicles <- purrr::map(
        list.files(path = folder,
                   pattern = profiles$ACCIDENTS_VEHICLES_FILE_NAME_PATTERN[[1]],
                   full.names = TRUE),
        read_raw_vehicles_files,
        skip = profiles$ACCIDENTS_FILES_SKIP[[1]]
    ) |>
        dplyr::bind_rows()


    # Data transformations
    accidents <- gps |>
        # Remove missing observations with missing coordinates
        tidyr::drop_na(starts_with("coord")) |>
        # Remove duplicities (keeps the first recrod)
        dplyr::distinct(p1, .keep_all = TRUE) |>
        # Join accidents data
        dplyr::right_join(accidents, by = "p1") |>
        # Remove unmatched coordinates and accidents without ID
        tidyr::drop_na(p1, starts_with("coord")) |>
        # Remove duplicities (keeps first record)
        dplyr::distinct(p1, .keep_all = TRUE)

    casualties <- outcomes |>
        dplyr::left_join(vehicles, by = c("p1", "id_vozidla")) |>
        dplyr::group_by(p1) |>
        dplyr::filter(p59g == 1) |>
        dplyr::mutate(
            type = dplyr::case_when(
                p44 %in% c(0,1,2) ~ "motobike",
                p44 == 13 ~ "bike",
                p44 %in% c(3,4) ~ "car",
                p44 %in% c(5,6,7) ~ "truck",
                TRUE ~ "other"
            ),
            driver = ifelse(p59a == 1,"driver","crew")
        ) |>
        dplyr::group_by(p1, type, driver) |>
        dplyr::summarise(
            obs = dplyr::n(),
            .groups = "drop"
        ) |>
        tidyr::pivot_wider(
            names_from = c(type,driver),
            values_from = obs,
            id_cols = p1,
            names_sep = "_",
            names_prefix = "casualties_",
            values_fill = 0
        )

    # Year of birth of a driver from the first vehicle
    birth <- outcomes |>
        dplyr::filter(id_vozidla == 1) |>
        dplyr::filter(p59a == 1) |>
        mutate(
            vek = ifelse(
                p59d <= (
                    lubridate::year(
                        lubridate::today()
                    )-2000),
                2000 + p59d,
                1900 + p59d
            )
        ) %>%
        dplyr::select(p1, driver_birth = vek) |>
        dplyr::distinct(p1, .keep_all = TRUE)

    involved_vehicles <- vehicles |>
        dplyr::mutate(
            vehicle_bike = p44 == 13,
            vehicle_motobike = p44 %in% c(0,1,2)
        ) %>%
        dplyr::group_by(p1) %>%
        dplyr::summarise(
            involved_bike = any(vehicle_bike),
            involved_motobike = any(vehicle_motobike),
            .groups = "drop"
        )

    # accidents <-
    #     dplyr::left_join(accidents, gps, by = "p1") |>
    #     dplyr::filter(!is.na(coord_x), !is.na(coord_y)) |>
    #     dplyr::distinct()

    accidents <- list(
        accidents,
        casualties,
        birth,
        pedestrians,
        involved_vehicles
    ) |>
        purrr::reduce(
            dplyr::left_join,
            by = "p1"
        ) |>
        # Age of drivers who caused the accident
        dplyr::mutate(
            driver_age = ifelse(
                p10 == 1,
                lubridate::year(p2) - driver_birth,
                NA
            )
        ) |>
        dplyr::select(
            -driver_birth
        ) |>
        dplyr::mutate(
            dplyr::across(
                where(is.logical),
                tidyr::replace_na, FALSE
            )
        ) |>
        dplyr::mutate(
            dplyr::across(
                tidyselect::starts_with("casualties"),
                tidyr::replace_na, 0L
            )
        ) |>
        sf::st_as_sf(coords = c("coord_x", "coord_y"),
                     crs = PLANARY_PROJECTION)

    accidents |>
        dplyr::mutate(
            accident_date = as.Date(p2),
            accident_material_cost = p14 * 100 / 1e6  # in mil. CZK
        ) |>
        dplyr::rename(
            accident_id = p1,
            accident_dead = p13a,
            accident_serious_injury = p13b,
            accident_light_injury = p13c,
            # Stepan:
            accident_datetime = p2,
            accident_type = p6,
            accident_obstacle_type = p8,
            accident_fault = p10,
            accident_alcohol = p11,
            accident_cause = p12
        ) |>
        dplyr::select(
          -p14
        ) |>
        dplyr::select(
            accident_id,
            accident_date,
            everything()
            )
}


# create_accidents(path_to_all_accidents, raw_accidents_dir) reads in all
# accidents
#
# inputs:
# - path_to_all_accidents ... (character scalar) path to file where data on all
#   accidents will be stored
# - raw_accidents_dir ... (character scalar) path to folder where input CSVs on
#   accidents are stored
#
# value:
#   none, it writes data to disk
#
# notes:
# - for assumptions on input files, see help for read_raw_accidents()
create_accidents <- function(path_to_all_accidents, raw_accidents_dir,
                             profiles) {
    start_logging(log_dir())
    logging::loginfo("accidents prep: checking for updates")
    if (is_behind(path_to_all_accidents,
                  c(list.files(raw_accidents_dir, pattern = "csv",
                             full.names = TRUE),
                    path_to_configs()))) {
        logging::loginfo("accidents prep: accidents data are behind---updating")
        tryCatch({
            accidents <- read_raw_accidents(raw_accidents_dir, profiles)
            write_dir_rds(accidents, path_to_all_accidents)
            logging::loginfo(
                "accidents prep: data on all accidents have been updated")
        },
        error = function(e) {
            logging::logerror("accidents prep failed: %s", e)
            stop("accidents prep failed---stopping evaluation")})
    } else {
        logging::loginfo("accidents prep: accidents are uptodate---skipping")
    }
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
                                 all_points = FALSE) {
    inside <- get_points_close_to_lines(points, lines, dist = dist)
    buff_points <- points[inside, ]
    nf <- sf::st_nearest_feature(buff_points, lines)
    np <- sf::st_nearest_points(buff_points, lines[nf, ], pairwise = TRUE)
    np <- sf::st_cast(np, "POINT")[c(FALSE, TRUE)]
    out <- sf::st_drop_geometry(buff_points)
    out$lixel_id <- lines$lixel_id[nf]
    out$geometry <- np
    out <- st_as_sf(out)
    if (all_points) {
        out$valid <- TRUE
        outside <- points[!inside, ]
        outside$valid <- FALSE
        outside$lixel_id <- NA_integer_
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
# - path_to_accidents ... (SF tibble) all accidents including their attributes
# - max_distance ... (numeric scalar) maximum distance from the (selected)
#   roads; it the distance is higher, the accident is removed from the dataset
# - lixel_dir ... (character scalar) path to folder there lixellized roads are
#   stored
# - accident_dir ... (character scalar) path to folder where the new accidents
#   files should be stored
# - shiny ... (logical scalar) whether the result is used for further
#   computation of densities (FALSE, default), or for shiny output (TRUE)
#
# value:
#   none; data are written to disk
#
# output:
#   - if shiny = TRUE
#       - accidents within the non-buffered districts are kept
#       - all accidents within the buffer are kept with no regards to their
#           distance to road system; variable denotes whether they are close to
#           to roads
#       - all variables are kept
#       - accidents are projected to WGS-84
#   - if shiny = FALSE
#       - accident within the buffered districts are kept
#       - only accidents close to roads are kept
#       - only variables needed for density computations are kept
#       - accidents are projected to PLANARY_PROJECTION
#
# notes:
# - the accidents that are farther from roads than max_distance are removed
# - remaining accident are snapped to roads, i.e., their position is changed
#   such that they lie on a road---they are moved to their closest points on
#   their closest line
# create_districts_accidents <- function(districts,
#                                        path_to_accidents,
#                                        lixel_dir,
#                                        accident_dir,
#                                        profiles) {
#     one_file <- function(input_file, output_file, accidents, max_distance) {
#         start_logging(log_dir())
#         logging::loginfo("district accidents prep: creating %s", output_file)
#         lines <- readr::read_rds(input_file)
#         snapped_points <- snap_points_to_lines(accidents, lines,
#                                                dist = max_distance)
#         write_dir_rds(snapped_points, output_file)
#         logging::loginfo("district accidents prep: %s has been created",
#                          output_file)
#     }
#
#     start_logging(log_dir())
#     logging::loginfo("district accidents prep: checking for uppdates")
#
#     tryCatch({
#         accidents <- readr::read_rds(path_to_accidents)
#         districts <- districts_behind(districts,
#                                       target_fun = accidents_file_name,
#                                       source_fun = lixel_file_name,
#                                       target_folder = accident_dir,
#                                       source_folder = lixel_dir,
#                                       other_files = c(path_to_districts(),
#                                                       path_to_accidents))
#         txt <- dplyr::if_else(nrow(districts) == 0, "---skipping", " in parallel")
#         logging::loginfo(
#             "district accidents prep: %d districts will be uppdated%s",
#             nrow(districts), txt)
#         tab <- tibble::tibble(
#             input_file = lixel_file_name(districts, lixel_dir),
#             output_file = accidents_file_name(districts, accident_dir))
#         PWALK(tab, one_file,
#               workers = profiles$NO_OF_WORKERS_ACCIDENTS[[1]],
#               ram_needed = profiles$RAM_PER_CORE_ACCIDENTS[[1]],
#               accidents = accidents,
#               max_distance = profiles$ACCIDENT_TO_ROAD_MAX_DISTANCE[[1]]
#               )
#         logging::loginfo(
#             "district accidents prep: district accidents have been updated")
#     },
#     error = function(e) {
#         logging::logerror("district accidents prep failed: %s", e)
#         stop("district accidents prep failed---stopping evaluation")})
# }
create_districts_accidents <- function(districts,
                                       path_to_accidents,
                                       lixel_dir,
                                       accident_dir,
                                       profiles,
                                       shiny = FALSE) {
    one_file <- function(geometry,
                         input_file,
                         output_file,
                         shiny,
                         accidents,
                         max_distance) {
        start_logging(log_dir())
        logging::loginfo("district accidents prep: creating %s", output_file)
        lines <- readr::read_rds(input_file)
        accidents <- accidents[sf::st_intersects(accidents,
                                                 geometry,
                                                 sparse = FALSE), ]
        snapped_points <- snap_points_to_lines(accidents, lines,
                                               dist = max_distance,
                                               all_points = shiny)
        # if the output is not for shiny, only a subset of necessary variables
        # is kept
        if (!shiny)
            snapped_points <- snapped_points |>
            select(accident_id, accident_date, accident_dead,
                   accident_serious_injury, accident_light_injury,
                   accident_material_cost, lixel_id)
        # if the output is for shiny, it must be re-projected to WGS84
        snapped_points <- sf::st_transform(snapped_points,
                                           crs = ifelse(shiny,
                                                        WGS84,
                                                        PLANARY_PROJECTION))
        write_dir_rds(snapped_points, output_file)
        logging::loginfo("district accidents prep: %s has been created",
                         output_file)
    }

    start_logging(log_dir())
    logging::loginfo("district accidents prep: checking for uppdates")

    tryCatch({
        accidents <- readr::read_rds(path_to_accidents)
        districts <- districts_behind(districts,
                                      target_fun = accidents_file_name,
                                      source_fun = lixel_file_name,
                                      target_folder = accident_dir,
                                      source_folder = lixel_dir,
                                      other_files = c(path_to_districts(),
                                                      path_to_accidents,
                                                      path_to_configs()))
        txt <- dplyr::if_else(nrow(districts) == 0, "---skipping", " in parallel")
        logging::loginfo(
            "district accidents prep: %d districts will be uppdated%s",
            nrow(districts), txt)
        # accidents are cropped to districts; if the output is not for shiny,
        # districts must be buffered
        if (!shiny)
            districts <- sf::st_buffer(districts,
                                       dist = profiles$DISTRICT_BUFFER_SIZE[[1]])
        tab <- districts |>
            dplyr::select() |>
            dplyr::mutate(
                input_file = lixel_file_name(districts, lixel_dir),
                output_file = accidents_file_name(districts, accident_dir),
                shiny = shiny
            )
        PWALK(tab, one_file,
              workers = profiles$NO_OF_WORKERS_ACCIDENTS[[1]],
              ram_needed = profiles$RAM_PER_CORE_ACCIDENTS[[1]],
              accidents = accidents,
              max_distance = profiles$ACCIDENT_TO_ROAD_MAX_DISTANCE[[1]]
        )
        logging::loginfo(
            "district accidents prep: district accidents have been updated")
    },
    error = function(e) {
        logging::logerror("district accidents prep failed: %s", e)
        stop("district accidents prep failed---stopping evaluation")})
}
