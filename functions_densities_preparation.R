# -------------------------------------
# Script:   functions_densities_preparation.R
# Author:   Michal Kvasnička
# Purpose:  This script defines functions to compute NKDEs using spNetwork.
# Inputs:   none
# Outputs:  function definitions
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# necessary packages
library(spNetwork, verbose = FALSE, warn.conflicts = FALSE)
library(readr, verbose = FALSE, warn.conflicts = FALSE)
library(tibble, verbose = FALSE, warn.conflicts = FALSE)



# density computation ----------------------------------------------------------

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
    one_district <- function(district_id,
                             map_path, lixel_path, sample_path, accidents_path,
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

        # multiply_grid_shape() provides quite many alternative grid shapes
        # around the optimal grid shape returned by grid_shape();
        # this function exists because of a bug in spNetwork package that may
        # have been corrected by its version 0.4.3.6;
        # if the new code is correct, only the first (optimal) grid shape is
        # tried and the loss of computational efficiency is negligible
        multiply_grid_shape <- function(grid_shape) {
            adds <- expand.grid(x = -2:2, y = -2:2) |>
                dplyr::mutate(sum = x + y, abs_sum = abs(x) + abs(y)) |>
                dplyr::arrange(abs_sum, desc(sum)) |>
                dplyr::select(x, y) |>
                as.matrix()
            gs <- rep(grid_shape, nrow(adds)) |>
                matrix(ncol = 2, byrow = TRUE)
            gs <- rbind(
                gs + adds,
                c(grid_shape[1], grid_shape[1]),
                c(grid_shape[2], grid_shape[2]),
                c(10, 10)
            ) |>
                unique()
            gs[gs[, 1] > 0 & gs[, 2] > 0, ]
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
        gs <- grid_shape(lixels) |>
            multiply_grid_shape()
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
                    logging::logwarn("densities prep: nkde failed with grid shape %s in file %s",
                                     gs[k, ], output_file)
            )
            if (!is.null(densities))
                break
        }
        if (is.null(densities))
            stop("nkde failed with all grid shapes")
        # nkde() returns a list for adaptive densities; we need to extract k
        # which is the density evaluated at sample points
        if (is.list(densities)) {
            if (!("k" %in% names(densities)))
                stop("nkde failed---densities are list but there is no k slot")
            densities <- densities$k
        }
        lixels$density <- densities
        write_dir_rds(lixels, output_file, compress = TRUE)
        logging::loginfo("densities prep: %s has been created", output_file)
    }

    start_logging(log_dir())
    logging::loginfo("densities prep: checking for updates")

    tryCatch({
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
    tab <- tibble::tibble(
        district_id = districts$district_id,
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
          ram_needed = profiles$RAM_PER_CORE_GENERAL[[1]]
          )
    logging::loginfo(
        "densities prep: hotspots have been updated")
    },
    error = function(e) {
        logging::logerror("hotspots prep failed: %s", e)
        stop("hotspots prep failed---stopping evaluation")})
}
