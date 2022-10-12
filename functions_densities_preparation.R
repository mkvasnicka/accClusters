# -------------------------------------
# Script:   functions_densities_preparation.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# necessary packages
require(spNetwork, quietly = TRUE, warn.conflicts = FALSE)
require(readr, quietly = TRUE, warn.conflicts = FALSE)
require(tibble, quietly = TRUE, warn.conflicts = FALSE)



# density computation ----------------------------------------------------------

# # compute_one_time_densities() computes densities for one time window given by
# # parameters from_date and to_date
# #
# # inputs:
# # - districts ... (sf tibble) districts table
# # - maps_dir ... (character scalar) path to folder where sfnetworks maps of
# #   roads in individual districts are stored
# # - lixel_dir ... (character scalar) path to folder where where lixelized roads
# #   in individual districts are stored
# # - sample_dir ... (character scalar) path to folder where samples of lixelized
# #   roads in individual districts are stored
# # - accidents_dir ... (character scalar) path to folder where accidents snapped
# #   to roads in individual districts are stored
# # - density_dir ... (character scalar) path to folder where computed densities
# #   will be stored
# # - from_date, to_date ... (Date scalars) describe date range of accidents that
# #   are used
# # - weights, bw, adaptive, trim_bw, method, agg ... parameters sent to
# #   spNetwork::nkde()
# # - workers ... (NULL or integer scalar) number of cores used in parallel
# # - other_files ... (character vector) pathes to other files that can determine
# #   whether existing files are up-to-date
# #
# # value:
# #   none; data are written to disk
# compute_one_time_densities <- function(districts,
#                                        maps_dir, lixel_dir, sample_dir,
#                                        accidents_dir,
#                                        density_dir,
#                                        from_date, to_date,
#                                        weights = NULL, bw = 300,
#                                        adaptive = FALSE, trim_bw = 600,
#                                        method = "discontinuous", agg = 1,
#                                        workers = NULL,
#                                        other_files = NULL) {
#   one_district <- function(map_path, lixel_path, sample_path, accidents_path,
#                            density_path,
#                            from_date, to_date,
#                            weights, bw, adaptive, trim_bw, method, agg) {
#     map <- readr::read_rds(map_path) |>
#         sfnetworks::activate("edges") |>
#         sf::st_as_sf()
#     lixels <- readr::read_rds(lixel_path)
#     samples <- readr::read_rds(sample_path)
#     accidents <- readr::read_rds(accidents_path) |>
#         dplyr::filter(p2a >= from_date, p2a <= to_date)
#     if (!(is.null(weights) ||
#           (is.character(weights) && length(weights) == 1 &&
#            weights %in% names(accidents) && is.numeric(accidents[[weights]]))))
#         stop("Weights must be eiter NULL or name of one numeric vector ",
#              "in accidents.")
#     if (is.null(weights))
#         weights <- rep(1,nrow(accidents))
#     if (is.character(weights))
#         weights <- accidents[[weights]]
#     densities <- spNetwork::nkde(map,
#                                  events = accidents,
#                                  w = weights,
#                                  samples = samples,
#                                  kernel_name = "quartic",
#                                  bw = bw, div = "bw",
#                                  adaptive = adaptive,
#                                  trim_bw = trim_bw,
#                                  method = method, digits = 1, tol = 1,
#                                  grid_shape = c(10,10), max_depth = 10,
#                                  agg = agg,
#                                  sparse = TRUE,
#                                  verbose = TRUE)
#     lixels$density <- densities
#     write_dir_rds(lixels, density_path)
#   }
#
#   from_date <- as.Date(from_date)
#   to_date <- as.Date(to_date)
#   stopifnot(from_date <= to_date)
#
#   profile_name <- NULL
#   if (exists("PROFILE_NAME"))
#       profile_name <- PROFILE_NAME
#
#   workers <- get_number_of_workers(workers)
#   districts <- districts_behind(districts,
#                                 target_fun = densities_file_name,
#                                 source_fun = list(sf_file_name, lixel_file_name,
#                                                   lixel_sample_file_name,
#                                                   accidents_file_name),
#                                 target_folder = density_dir,
#                                 source_folder = list(maps_dir, lixel_dir,
#                                                      sample_dir, accidents_dir),
#                                 other_files = other_files,
#                                 from_date = from_date, to_date = to_date,
#                                 profile_name = profile_name)
#   tab <- tibble::tibble(
#       map_path = sf_file_name(districts, maps_dir),
#       lixel_path = lixel_file_name(districts, lixel_dir),
#       sample_path = lixel_sample_file_name(districts, sample_dir),
#       accidents_path = accidents_file_name(districts, accidents_dir),
#       density_path = densities_file_name(districts, density_dir,
#                                          from_date = from_date,
#                                          to_date = to_date,
#                                          profile_name = profile_name)
#   )
#   PWALK(tab, one_district, workers = workers,
#         from_date, to_date,
#         weights = weights, bw = bw, adaptive = adaptive, trim_bw = trim_bw,
#         method = method, agg = agg)
# }
#
#
# # compute_densities() computes densities for all time periods given by time
# # window
# #
# # inputs:
# # - districts ... (sf tibble) districts table
# # - maps_dir ... (character scalar) path to folder where sfnetworks maps of
# #   roads in individual districts are stored
# # - lixel_dir ... (character scalar) path to folder where where lixelized roads
# #   in individual districts are stored
# # - sample_dir ... (character scalar) path to folder where samples of lixelized
# #   roads in individual districts are stored
# # - accidents_dir ... (character scalar) path to folder where accidents snapped
# #   to roads in individual districts are stored
# # - density_dir ... (character scalar) path to folder where computed densities
# #   will be stored
# # - path_to_time_window_file ... (character scalar) path to TSV file where time
# #   windows are stored; see help for read_time_window_file()
# # - weights, bw, adaptive, trim_bw, method, agg ... parameters sent to
# #   spNetwork::nkde()
# # - workers ... (NULL or integer scalar) number of cores used in parallel
# # - other_files ... (character vector) pathes to other files that can determine
# #   whether existing files are up-to-date
# #
# # value:
# #   none; data are written to disk
# compute_densities <- function(districts,
#                               maps_dir, lixel_dir, sample_dir, accidents_dir,
#                               density_dir,
#                               path_to_time_window_file,
#                               weights = NULL, bw = 300,
#                               adaptive = FALSE, trim_bw = 600,
#                               method = "discontinuous", agg = 1,
#                               workers = NULL,
#                               other_files = NULL) {
#     time_window <- read_time_window_file(path_to_time_window_file)
#     purrr::walk(seq_len(nrow(time_window)),
#                 ~compute_one_time_densities(districts = districts,
#                                             maps_dir = maps_dir,
#                                             lixel_dir = lixel_dir,
#                                             sample_dir = sample_dir,
#                                             accidents_dir = accidents_dir,
#                                             density_dir = density_dir,
#                                             from_date = time_window$from_date[.],
#                                             to_date = time_window$to_date[.],
#                                             weights = weights, bw = bw,
#                                             adaptive = adaptive,
#                                             trim_bw = trim_bw,
#                                             method = method, agg = agg,
#                                             workers = workers,
#                                             other_files = other_files)
#     )
# }


# compute_one_time_densities() computes densities for one time window given by
# parameters from_date and to_date
#
# inputs:
# - districts ... (sf tibble) districts table
# - maps_dir ... (character scalar) path to folder where sfnetworks maps of
#   roads in individual districts are stored
# - lixel_dir ... (character scalar) path to folder where where lixelized roads
#   in individual districts are stored
# - sample_dir ... (character scalar) path to folder where samples of lixelized
#   roads in individual districts are stored
# - accidents_dir ... (character scalar) path to folder where accidents snapped
#   to roads in individual districts are stored
# - density_dir ... (character scalar) path to folder where computed densities
#   will be stored
# - time_window ... (either character scalar or tibble with two columns)
#   - if time_window is character scalar, it is a path to TSV file where time
#       window is stored; see help for read_time_window_file()
#   - if time_window is tibble, it must have two columns (from_date and to_date)
#       which includes Dates or character vectors in YYYY-MM-DD format
#       convertible to Dates
# - weights ... (either "cost" or "equal") if "cost", the NKDE weights are set
#   to accident_cost; otherwise, they are equal to 1
# - bw, adaptive, trim_bw, method, agg ... parameters sent to
#   spNetwork::nkde()
#
# value:
#   none; data are written to disk
compute_densities <- function(districts,
                              maps_dir, lixel_dir, sample_dir,
                              accidents_dir,
                              density_dir,
                              profiles) {
    one_district <- function(map_path, lixel_path, sample_path, accidents_path,
                             output_file,
                             from_date, to_date,
                             weights, bw, adaptive, trim_bw, method, agg,
                             unit_cost_dead, unit_cost_serious_injury,
                             unit_cost_light_injury, unit_cost_material,
                             unit_cost_const) {
        start_logging(log_dir())

        # grid_shape() is a heuristics that guesses how to split districts for
        # the density computation; districts with holes (such as okres Brno
        # venkov might not work optimally); this may be useful for performance
        # reasons but it is necessary because the NKDE computation fails for
        # some districts otherwise
        grid_shape <- function(map) {
            # x and y size of the area in kms
            box <- sf::st_bbox(map)
            x <- as.numeric(box$xmax - box$xmin) / 1e3
            y <- as.numeric(box$ymax - box$ymin) / 1e3
            # assuming Brno-město should have 100 blocks (suggested by the
            # author of the NKDE package), how many block the district should
            # have
            n <- nrow(map) / 5e5 * 100
            # assuming (roughly) square block, we set the side-size of the block
            # in such a way there are n block altogether
            b <- sqrt(x * y / n)
            xn <- max(c(1, round(x / b)))
            yn <- max(c(1, round(y / b)))
            # return
            c(x = xn, y = yn)
        }

        start_logging(log_dir())
        logging::loginfo("densities prep: creating %s", output_file)
        map <- readr::read_rds(map_path) |>
            sfnetworks::activate("edges") |>
            sf::st_as_sf()
        lixels <- readr::read_rds(lixel_path)
        samples <- readr::read_rds(sample_path)
        accidents <- readr::read_rds(accidents_path) |>
            dplyr::filter(accident_date >= from_date,
                          accident_date <= to_date) |>
            add_damage_cost(unit_cost_dead, unit_cost_serious_injury,
                            unit_cost_light_injury, unit_cost_material,
                            unit_cost_const)

        if (!(is.character(weights) && length(weights) == 1 &&
               weights %in% c("cost", "equal")))
            stop("Weights must be eiter 'cost' or 'equal'.")
        if (weights == "equal") {
            weights <- rep(1,nrow(accidents))
        } else {
            weights <- accidents$accident_cost
        }

        # nkde fails with some grids; several grid are tested for this reason
        grid_shape <- grid_shape(lixels)
        gs <- rbind(grid_shape,
                    grid_shape + 1,
                    grid_shape - 1,
                    c(grid_shape[1], grid_shape[1]),
                    c(grid_shape[2], grid_shape[2]),
                    c(10, 10)) |>
            tibble::as_tibble(.name_repair = "universal") |>
            dplyr::distinct() |>
            as.matrix()
        densities <- NULL
        for (k in seq_len(nrow(gs))) {
            tryCatch(
                densities <- spNetwork::nkde(map,
                                             events = accidents,
                                             w = weights,
                                             samples = samples,
                                             kernel_name = "quartic",
                                             bw = bw, div = "bw",
                                             adaptive = adaptive,
                                             trim_bw = trim_bw,
                                             method = method, digits = 1,
                                             tol = 1,
                                             grid_shape = gs[k, ],
                                             max_depth = 10,
                                             agg = agg,
                                             sparse = TRUE,
                                             verbose = TRUE),
                error = function(e)
                    logging::logwarn("nkde failed with grid shape %s", gs[k, ])
            )
            if (!is.null(densities))
                break
        }
        if (is.null(densities))
            stop("nkde failed with all grid shapes")
        lixels$density <- densities
        write_dir_rds(lixels, output_file)
        logging::loginfo("densities prep: %s has been created", output_file)
    }

    logging::loginfo("densities prep: checking for updates")

    tryCatch({
    # time_window <- handle_time_window(time_window)

    districts <- districts |>
        bind_cols(profiles |>
                      dplyr::select(PROFILE_NAME, TIME_WINDOW,
                                    starts_with("NKDE_"),
                                    starts_with("UNIT_COST_")) |>
                      tidyr::unnest(TIME_WINDOW) |>
                      tidyr::nest(data = everything())) |>
        tidyr::unnest(data)
    districts <- districts |>
        districts_behind(target_fun = densities_file_name,
                         source_fun = list(sf_file_name, lixel_file_name,
                                           lixel_sample_file_name,
                                           accidents_file_name),
                         target_folder = density_dir,
                         source_folder = list(maps_dir, lixel_dir,
                                              sample_dir, accidents_dir),
                         other_files = path_to_districts(),
                         from_date = districts$from_date,
                         to_date = districts$to_date,
                         profile_name = districts$PROFILE_NAME)

    # districts <- purrr::map2(
    #     time_window$from_date, time_window$to_date,
    #     ~districts_behind(districts |>
    #                           mutate(from_date = .x, to_date = .y),
    #                       target_fun = densities_file_name,
    #                       source_fun = list(sf_file_name, lixel_file_name,
    #                                         lixel_sample_file_name,
    #                                         accidents_file_name),
    #                       target_folder = density_dir,
    #                       source_folder = list(maps_dir, lixel_dir,
    #                                            sample_dir, accidents_dir),
    #                       other_files = path_to_districts(),
    #                       from_date = .x, to_date = .y,
    #                       profile_name = profile_name)
    # ) |>
    #     dplyr::bind_rows()
    tab <- tibble::tibble(
        map_path = sf_file_name(districts, maps_dir),
        lixel_path = lixel_file_name(districts, lixel_dir),
        sample_path = lixel_sample_file_name(districts, sample_dir),
        accidents_path = accidents_file_name(districts, accidents_dir),
        output_file = densities_file_name(districts, density_dir,
                                          from_date = districts$from_date,
                                          to_date = districts$to_date,
                                          profile_name = districts$PROFILE_NAME),
        from_date = districts$from_date,
        to_date = districts$to_date,
        weights = districts$NKDE_WEIGHTS,
        bw = districts$NKDE_BW,
        adaptive = districts$NKDE_ADAPTIVE,
        trim_bw = districts$NKDE_TRIM_BW,
        method = districts$NKDE_METHOD,
        agg = districts$NKDE_AGG,
        unit_cost_dead = districts$UNIT_COST_DEAD,
        unit_cost_serious_injury = districts$UNIT_COST_SERIOUS_INJURY,
        unit_cost_light_injury = districts$UNIT_COST_LIGHT_INJURY,
        unit_cost_material = districts$UNIT_COST_MATERIAL,
        unit_cost_const = districts$UNIT_COST_CONST
    )

    txt <- dplyr::if_else(nrow(districts) == 0, "---skipping", " in parallel")
    logging::loginfo(
        "densities prep: %d districts x times will be uppdated%s",
        nrow(districts), txt)

    PWALK(tab, one_district,
          workers = profiles$NO_OF_WORKERS[[1]],
          ram_needed = profiles$RAM_PER_CORE_GENERAL[[1]]#,
          # weights = weights, bw = bw, adaptive = adaptive, trim_bw = trim_bw,
          # method = method, agg = agg
          )
    logging::loginfo(
        "densities prep: hotspots have been updated")
    },
    error = function(e) {
        logging::logerror("hotspots prep failed: %s", e)
        stop("hotspots prep failed---stopping evaluation")})
}
