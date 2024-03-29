# -------------------------------------
# Script:   functions_cluster_preparation.R
# Author:   Michal Kvasnička
# Purpose:  This script defines functions for finding clusters of accidents.
# Inputs:   none
# Outputs:  function definitions
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# load packages
library(sf, verbose = FALSE, warn.conflicts = FALSE)
library(sfnetworks, verbose = FALSE, warn.conflicts = FALSE)
library(spdep, verbose = FALSE, warn.conflicts = FALSE)
library(dplyr, verbose = FALSE, warn.conflicts = FALSE)
library(tidyr, verbose = FALSE, warn.conflicts = FALSE)
library(purrr, verbose = FALSE, warn.conflicts = FALSE)


# projections
PLANARY_PROJECTION <- 5514  # Křovák
WGS84 <- 4326  # WGS84



# one district cluster tibble --------------------------------------------------

# cluster_condition_density() tells which rows x in tibble lixels have densities
# at least equal to threshold
#
# inputs:
# - x ... (integer vector) which rows in lixels tibble are handled
# - lixels ... (sf tibble) must include density column
# - threshold ... (numeric scalar) density threshold
#
# value:
#   logical vector; TRUE if corresponding density >= threshold
#
# notes:
# - this function is used as parameter to get_one_cluster(); it serves there as
#   condition telling whether some neighboring lixes should be included into
#   cluster or not; the lixels with sufficient densities are included
#
# WARNING: users are not supposed to run this function directly
cluster_condition_density <- function(x, lixels, threshold){
    lixels$density[x] >= threshold
}


# produce_cluster_condition_steps() is function factory which produces functions
# (closures) with interface function(x, lixels, steps) that returns TRUE if it
# have been called at max steps times, and FALSE later
#
# inputs: none
#
# value:
#   closure
#
# notes:
# notes:
# - this function is used as parameter to get_one_cluster(); it serves there as
#   condition telling whether some neighboring lixes should be included into
#   cluster or not; lixels that are at max steps-neighbors are included, i.e.,
#   lixels that can be approached in steps steps or less
#
# WARNING: users are not supposed to run this function directly
produce_cluster_condition_steps <- function() {
    done <- 0L
    function(x, lixels, steps) {
        done <<- done + 1L
        (done <= steps)
    }
}


# get_one_cluster() returns rows in lixels on which one cluster lies; the
# function is used twice: 1) to find consecutive lixels with densities at least
# equal to threshold and 2) to enlarge the cluster---to include lixels that can
# be approached in steps or fewer steps
#
# inputs:
# - lixesl ... (sf tibble) lixels with densities
# - nb ... (spdep's nb list) a list with one component for each row in lixels
#   (in their order); each component tells which lixels in lixels are neighbors
# - core ... (integer vector) starting cluster consisting of number of rows in
#   lixels
# - condition ... (closure) presently either cluster_condition_density() or
#   closure produced by produce_cluster_condition_steps()
# - ... ... parameters sent to condition function; presently either threshold or
#   steps
#
# value:
#   integer vector of indexes of rows in lixels that create one consecutive
#   cluster
#
# WARNING: users are not supposed to run this function directly
get_one_cluster <- function(lixels, nb, core, condition, ...) {
    get_neighbors <- function(x, nb) {
        unique(unlist(nb[x]))
    }
    cluster <- core
    last_items <- core
    repeat {
        neighbors <- get_neighbors(last_items, nb)
        neighbors <- setdiff(neighbors, cluster)
        neighbors <- neighbors[condition(neighbors, lixels, ...)]
        if (length(neighbors) == 0)
            break
        cluster <- unique(c(cluster, neighbors))
        last_items <- neighbors
    }
    cluster
}


# make_one_cluster() returns indices of rows in lixels that create one
# consecutive cluster
#
# inputs:
# - lixesl ... (sf tibble) lixels with densities
# - nb ... (spdep's nb list) a list with one component for each row in lixels
#   (in their order); each component tells which lixels in lixels are neighbors
# - start ... (integer scalar) starting lixel---where the cluster is positioned;
#   e.g., a lixel with the highest density
# - threshold ... (numeric scalar) minimal density necessary in the first step
#   for the lixel to be included into cluster
# - steps ... (integer scalar) how many times the cluster is recursively
#   enlarged
#
# value:
#   integer vector of indexes of rows in lixels that create one consecutive
#   cluster
#
# notes:
# - this function 1) finds all consecutive lixels starting from start lixel
#   which have density >= threshold, and then 2) recursively add all neighboring
#   lixels in steps steps
#
# WARNING: users are not supposed to run this function directly
make_one_cluster <- function(lixels, nb, start, threshold, steps) {
    cluster_condition_steps <- produce_cluster_condition_steps()

    cluster <- get_one_cluster(lixels, nb, start,
                               cluster_condition_density, threshold)
    cluster <- get_one_cluster(lixels, nb, cluster,
                               cluster_condition_steps, steps)
    cluster
}


# make_clusters() finds all clusters in one district
#
# inputs:
# - lixesl ... (sf tibble) lixels with densities
# - nb ... (spdep's nb list) a list with one component for each row in lixels
#   (in their order); each component tells which lixels in lixels are neighbors
# - start ... (integer scalar) starting lixel---where the cluster is positioned;
#   e.g., a lixel with the highest density
# - threshold ... (numeric scalar) minimal density necessary in the first step
#   for the lixel to be included into cluster
# - steps ... (integer scalar) how many times the cluster is recursively
#   enlarged
#
# value:
#   list of integer vectors; each item in the list is one cluster; each item in
#   the integer vectors is index of one row in lixels
#
# notes:
# - cluster found by this function may overlap; therefore, join_clusters() must
#   be run afterwards
#
# WARNING: users are not supposed to run this function directly
make_clusters <- function(lixels, nb, threshold, steps) {
    clusters <- list()
    all_tab <- tibble::tibble(
        id = seq_len(nrow(lixels)),
        dens = lixels$density
    ) |>
        dplyr::filter(dens >= threshold)
    repeat {
        non_idx <- unique(unlist(clusters))
        tab <- all_tab |>
            dplyr::filter(!(id %in% non_idx))
        if (nrow(tab) == 0)
            break
        start <- tab$id[which(tab$dens == max(tab$dens))][1]
        clusters[[length(clusters) + 1]] <-
            make_one_cluster(lixels, nb, start, threshold, steps)
    }
    clusters
}


# join_clusters() takes clusters list created by make_clusters() and joins
# clusters that overlap
#
# inputs:
# - clusters ... (list of integer vectors) cluster list produced by
#   make_clusters()
#
# value:
#   similar list of integer vectors but the clusters that overlapped have been
#   joined now
#
# WARNING: users are not supposed to run this function directly
join_clusters <- function(clusters, nbs) {
    neighbors <- function(c1, c2, nbs) {
        next_to <- function(x, y, nbs) {
            length(base::intersect(unique(unlist(nbs[x])), y)) > 0
        }
        # the clusters share at least one lixel
        length(base::intersect(c1, c2)) > 0 ||
            # or their are neighbors
            next_to(c1, c2, nbs) || next_to(c2, c1, nbs)
    }
    i <- 1L
    while (i < length(clusters)) {
        joined <- FALSE
        j <- i + 1L
        while (j <= length(clusters)) {
            if (neighbors(clusters[[i]], clusters[[j]], nbs)) {
                clusters[[i]] <- unique(c(clusters[[i]], clusters[[j]]))
                clusters[[j]] <- NULL
                joined <- TRUE
            } else {
                j <- j + 1L
            }
        }
        if (!joined)
            i <- i + 1L
    }
    clusters
}


# compute_cluster_tibble() find all cluster of hotspots in one district
#
# inputs:
# - lixesl ... (sf tibble) lixels with densities
# - nb ... (spdep's nb list) a list with one component for each row in lixels
#   (in their order); each component tells which lixels in lixels are neighbors
# - start ... (integer scalar) starting lixel---where the cluster is positioned;
#   e.g., a lixel with the highest density
# - threshold ... (numeric scalar) minimal density necessary in the first step
#   for the lixel to be included into cluster
# - steps ... (integer scalar) how many times the cluster is recursively
#   enlarged
#
# value:
#   tibble with two colums:
#   - cluster ... (integer) the id of cluster
#   - lineID ... (integer) the id of underlying lixels
#
# notes:
# - for each cluster, this function 1) finds all consecutive lixels starting
#   from start lixel which have density >= threshold, and then 2) recursively
#   adds all neighboring lixels in steps steps
# compute_cluster_tibble <- function(lixels, nb, threshold, steps) {
#     lst <- make_clusters(lixels, nb, threshold, steps) |>
#         join_clusters()
#     if (length(lst) == 0)
#         return(tibble::tibble(cluster = integer(0), lixel_id = integer(0)))
#     lst |>
#         purrr::map(~tibble(lixel_id = unlist(.))) |>
#         dplyr::bind_rows(.id = "cluster") |>
#         dplyr::mutate(cluster = as.integer(cluster))
# }
compute_cluster_tibble <- function(lixels, nb, threshold, steps) {
    lst <- make_clusters(lixels, nb, threshold, steps) |>
        join_clusters(nb)
    if (length(lst) == 0)
        return(tibble::tibble(cluster = integer(0), lixel_id = integer(0)))
    lid <- lixels |> st_drop_geometry() |> pull(lixel_id)
    lst |>
        purrr::map(~tibble(lixel_id = unlist(.))) |>
        dplyr::bind_rows(.id = "cluster") |>
        # the last step translates lixel_id from its order in vector to the true
        # lixel_id; however, they shoud be the same
        dplyr::mutate(cluster = as.integer(cluster),
                      lixel_id = lid[lixel_id])
}



# cluster to lixels/accidents --------------------------------------------------

# add_clusters_to_lixels(lixels, clusters) adds column cluster to lixels; it
# indicates to which cluster lixels belong
#
# inputs:
# - lixels ... (sf tibble) lixelized roads or lizelized roads with densities
# - clusters ... (tibble) table created by compute_cluster_tibble()
#
# value:
#   sf tibble lixels with added column cluster (NA for lixels that are part of
#   no cluster)
add_clusters_to_lixels <- function(lixels, clusters) {
    dplyr::left_join(lixels, clusters, by = "lixel_id")
}


# add_clusters_to_accidents(accidents, clusters) adds column cluster to
# accidents table; it indicates to which cluster lixels belong
#
# inputs:
# - accidents ... (sf tibble) accidents snapped to roads
# - clusters ... (tibble) table created by compute_cluster_tibble()
#
# value:
#   sf tibble lixels with added column cluster (NA for lixels that are part of
#   no cluster)
add_clusters_to_accidents <- function(accidents, clusters) {
    dplyr::left_join(accidents, clusters, by = "lixel_id")
}



# compute cluster costs --------------------------------------------------------

# cluster_cost(accidents) computes cost of each cluster
#
# inputs:
# - accidents ... (sf tibble) table of accidents snapped to roads in particular
#   district
#
# value:
cluster_cost <- function(accidents) {
    accidents |>
        dplyr::filter(!is.na(cluster)) |>
        dplyr::group_by(cluster) |>
        dplyr::summarise(cost = sum(accident_cost), .groups = "drop")
}


# cluster_statistics() computes tibble with basic statistics for each cluster
#
# inputs:
# - lixels
# - accidents
# - clusters
#
# value:
#   tibble with four columns:
#   - cluster ... (integer) cluster id
#   - total_length ... (double) clusters' total lengths in meters
#   - total_density ... (double) sum of densities of lixels within clusters
#   - cost ... (double) sum of cost of accidents lying on lixels within clusters
#       in mil. CZK
#   - cost_per_meter (double) cost / total_length
cluster_statistics <- function(lixels, accidents, clusters) {
    clstrs <- lixels |>
        sf::st_drop_geometry() |>
        add_clusters_to_lixels(clusters) |>
        dplyr::filter(!is.na(cluster)) |>
        dplyr::group_by(cluster) |>
        dplyr::summarise(total_length = as.numeric(sum(len)),
                         total_density = sum(density),
                         .groups = "drop")
    accdnts <- accidents |>
        sf::st_drop_geometry() |>
        add_clusters_to_accidents(clusters) |>
        cluster_cost()
    dplyr::left_join(clstrs, accdnts, by = "cluster") |>
        dplyr::mutate(cost_per_meter = cost / total_length)
}


# graphic_clusters() just for fast visualization
graphic_clusters <- function(lixels, clusters, cluster_statistics) {
    dplyr::left_join(lixels, clusters, by = "lixel_id") |>
        dplyr::filter(!is.na(cluster)) |>
    dplyr::left_join(cluster_statistics, by = "cluster")
}



# optimize cluster parameters --------------------------------------------------

# cluster_pai() computes PAI of clusters
#
# inputs:
# - cluster ... ((sf) tibble) with columns total_density and cost as returned by
#   graphic_clusters()
# - accidents ... (sf tibble) of accidents cropped to the particular district
#   and snapped to its roads; it must contain column accident_cost
# - lixels ... (sf tibble) road lixels; it must contain column len
#
# value:
#   PAI (numeric scalar)---the higher, the better
cluster_pai <- function(cluster, accidents, lixels) {
    # in cluster_cost, NA means there are clusters that include no accident,
    # i.e, that the number of steps it too low for the particular threshold;
    # the question is whether it should be ignored (na.rm = TRUE), or strictly
    # forbidden (na.rm = FALSE)
    cluster_cost <- sum(cluster$cost, na.rm = TRUE)
    cluster_length <- as.numeric(sum(cluster$total_length))
    total_cost <- sum(accidents$accident_cost)
    total_length <- as.numeric(sum(lixels$len))
    (cluster_cost / total_cost) / (cluster_length / total_length)

}


# optimize_cluster_parameters() finds threshold and no_of_steps used in
# compute_cluster_tibble() to maximize PAI based on accident cost
#
# WARNING:
# - this function probably returns too high quantile and too low no_of_steps
optimize_cluster_parameters <- function(lixels, nb, accidents,
                                        threshold_range = seq(from = 0.975,
                                                              to = 0.999,
                                                              by = 0.001),
                                        step_range = 1:30) {
    f <- function(lixels, nb, accidents, threshold, no_of_steps) {
        cls <- compute_cluster_tibble(lixels, nb, threshold, no_of_steps)
        clstrs <- cluster_statistics(lixels, accidents, cls)
        cluster_pai(clstrs, accidents, lixels)
    }
    grid <- tidyr::expand_grid(
        threshold_quantile = threshold_range,
        no_of_steps = step_range) |>
        dplyr::mutate(threshold = quantile(lixels$density, threshold_quantile))
    grid$pai <- purrr::map_dbl(seq_len(nrow(grid)),
                               ~f(lixels, nb, accidents,
                                  threshold = grid$threshold[.],
                                  no_of_steps = grid$no_of_steps[.]))
    dplyr::filter(grid, !is.na(pai)) |>
        dplyr::arrange(desc(pai))
}



# cluster preparation ----------------------------------------------------------

# TODO: přidat dokumentaci funkce
# compute_clusters_for_parameters()
#
# inputs:
# - quantile ... (numeric scalar) ?
# - threshold ... (numeric scalar) ?
# - cluster_steps ... (numeric scalar) ?
# - lixels
# - accidents
# - nb
# - geometry
#
# value:
compute_clusters_for_parameters <- function(quantile, threshold,
                                            cluster_steps,
                                            lixels, accidents, nb, geometry) {
    cls <- compute_cluster_tibble(lixels, nb, threshold, cluster_steps)
    clss <- cluster_statistics(lixels, accidents, cls)

    lixels <- lixels |>
        add_clusters_to_lixels(cls) |>
        dplyr::select(lixel_id, density, cluster)
    accidents <- accidents |>
        add_clusters_to_accidents(cls) |>
        sf::st_drop_geometry() |>
        dplyr::filter(!is.na(cluster)) |>
        dplyr::select(accident_id, cluster, accident_cost)
    join_network <- function(geo) {
        # this function visually simplifies the clusters (so that they are
        # rendered faster); the simplifications fails in rare cases, probably
        # due to a bug in to_spatial_smooth(); the cluster isn't simplified in
        # such a case, i.e., it still consists of individual lixels
        tryCatch({
            geo <- geo |>
                sfnetworks::as_sfnetwork(directed = FALSE) |>
                tidygraph::convert(sfnetworks::to_spatial_smooth) |>
                sfnetworks::activate("edges") |>
                sf::st_as_sf() |>
                sf::st_union()
        },
        error = function(e)
            logging::logwarn(
                stringr::str_c(
                    "clusters prep: cluster visual simplification failed; ",
                    "I'm leaving this cluster not simplified"))
        )
        geo
    }
    cluster_statistics <- lixels |>
        dplyr::filter(!is.na(cluster)) |>
        dplyr::group_by(cluster) |>
        dplyr::summarise(geometry = join_network(geometry),
                         .groups = "drop") |>
        dplyr::left_join(clss, by = "cluster")
    # crop all to true district boarder
    crop_to_district <- function(x, district) {
        x[sf::st_intersects(x, district, sparse = FALSE), ]
    }
    # lixels <- lixels |>
    #     crop_to_district(geometry) |>
    #     sf::st_transform(crs = WGS84)
    cluster_statistics <- cluster_statistics |>
        filter(!is.na(cost)) |>  # removes clusters that include no accident
        crop_to_district(geometry)
    # cluster centroids
    centroids <- cluster_statistics |>
        sf::st_geometry() |>
        sf::st_centroid() |>
        sf::st_transform(crs = WGS84) |>
        sf::st_coordinates()
    cluster_statistics <- dplyr::bind_cols(
        cluster_statistics |> sf::st_transform(crs = WGS84),
        centroids)
    accidents <- accidents |>
        filter(cluster %in% unique(cluster_statistics$cluster))
    tibble::tibble(severity = as.integer(round((1 - quantile) * 1000)),
                   # quantile = quantile,
                   # threshold = threshold,
                   additional_lixels = cluster_steps,
                   accidents = list(accidents),
                   clusters = list(cluster_statistics))
}


# compute_one_time_clusters() computes hotspot clusters for one time period
# given by parameters from_date and to_date
#
# inputs:
# - districts ... (sf tibble) districts table
# - densities_dir ... (character scalar) path to folder where densities are
#   stored
# - lixel_maps_dir ... (character scalar) path to folder where where nbs for
#   lixelized roads in individual districts are stored
# - accidents_dir ... (character scalar) path to folder where accidents snapped
#   to roads in individual districts are stored
# - cluster_dir ... (character scalar) path to folder where the clusters will be
#   stored
# - profiles ... (tibble) of profiles
#
# value:
#   none; data are written to disk
#
# notes:
# - clusters are found in two steps:
#   1. all consecutive lixels with densities above or at cluster_min_quantile
#       constitute clusters
#   2. cluster_steps lixels are recursively added to all clusters
compute_clusters <- function(districts,
                             densities_dir,
                             lixel_maps_dir,
                             accidents_dir,
                             cluster_dir,
                             profiles) {
    one_district <- function(district_id,
                             geometry,
                             densities_file,
                             lixel_nb_file,
                             accident_file,
                             output_file,
                             from_date, to_date,
                             cluster_min_quantile,
                             cluster_steps,
                             visual_min_quantile,
                             unit_cost_dead,
                             unit_cost_serious_injury,
                             unit_cost_light_injury,
                             unit_cost_material,
                             unit_cost_const,
                             const_cost_dead,
                             const_cost_serious_injury,
                             const_cost_light_injury,
                             const_cost_material,
                             cluster_severity_limit,
                             cluster_severity_step,
                             cluster_step_limit) {
        start_logging(log_dir())
        logging::loginfo("clusters prep: creating %s", output_file)
        lixels <- readr::read_rds(densities_file)
        nb <- readr::read_rds(lixel_nb_file)
        accidents <- readr::read_rds(accident_file) |>
            dplyr::filter(accident_date >= from_date,
                          accident_date <= to_date) |>
            add_damage_cost(unit_cost_dead, unit_cost_serious_injury,
                            unit_cost_light_injury, unit_cost_material,
                            unit_cost_const, const_cost_dead,
                            const_cost_serious_injury, const_cost_light_injury,
                            const_cost_material)

        severity <- seq(from = cluster_severity_limit, to = 1,
                        by = -cluster_severity_step) / 1000
        pars <- tidyr::expand_grid(
            quantile = 1 - severity,
            cluster_steps = 1:cluster_step_limit
        ) |>
            dplyr::mutate(threshold = quantile(lixels$density, quantile)) |>
            dplyr::select(quantile, threshold, cluster_steps)
        output <- purrr::pmap_dfr(pars, compute_clusters_for_parameters,
                                  lixels = lixels, accidents = accidents,
                                  nb = nb, geometry = geometry)

        # write to file
        write_dir_rds(output, file = output_file)
        logging::loginfo("clusters prep: %s has been created", output_file)
    }

    start_logging(log_dir())
    logging::loginfo("clusters prep: checking for updates")
    tryCatch({
        districts <- districts |>
            dplyr::bind_cols(profiles |>
                                 dplyr::select(PROFILE_NAME, TIME_WINDOW,
                                               CLUSTER_SEVERITY_LIMIT,
                                               CLUSTER_SEVERITY_STEP,
                                               CLUSTER_STEP_LIMIT,
                                               starts_with("UNIT_COST_"),
                                               starts_with("CONST_COST_")) |>
                                 tidyr::unnest(TIME_WINDOW) |>
                                 tidyr::nest(data = everything())
            ) |>
            tidyr::unnest(data)
        districts <- districts |>
            districts_behind(target_fun = shiny_file_name,
                             source_fun = list(lixel_nb_file_name,
                                               accidents_file_name),
                             target_folder = cluster_dir,
                             source_folder = list(lixel_maps_dir,
                                                  accidents_dir),
                             other_files = path_to_districts(),from_date = districts$from_date,
                             to_date = districts$to_date,
                             profile_name = districts$PROFILE_NAME)
        txt <- dplyr::if_else(nrow(districts) == 0,
                              "---skipping", " in parallel")
        logging::loginfo(
            "clusters prep: %d districts x times will be uppdated%s",
            nrow(districts), txt)
        tab <- districts |>
            dplyr::select(district_id) |>
            dplyr::mutate(
                densities_file = densities_file_name(districts, densities_dir,
                                                     from_date = districts$from_date,
                                                     to_date = districts$to_date,
                                                     profile_name = districts$PROFILE_NAME),
                lixel_nb_file = lixel_nb_file_name(districts, lixel_maps_dir),
                accident_file = accidents_file_name(districts, accidents_dir),
                output_file = shiny_file_name(districts, cluster_dir,
                                              from_date = districts$from_date,
                                              to_date = districts$to_date,
                                              profile_name = districts$PROFILE_NAME),
                from_date = districts$from_date,
                to_date = districts$to_date,
                cluster_severity_limit = districts$CLUSTER_SEVERITY_LIMIT,
                cluster_severity_step = districts$CLUSTER_SEVERITY_STEP,
                cluster_step_limit = districts$CLUSTER_STEP_LIMIT,
                unit_cost_dead = districts$UNIT_COST_DEAD,
                unit_cost_serious_injury = districts$UNIT_COST_SERIOUS_INJURY,
                unit_cost_light_injury = districts$UNIT_COST_LIGHT_INJURY,
                unit_cost_material = districts$UNIT_COST_MATERIAL,
                unit_cost_const = districts$UNIT_COST_CONST,
                const_cost_dead = districts$CONST_COST_DEAD,
                const_cost_serious_injury = districts$CONST_COST_SERIOUS_INJURY,
                const_cost_light_injury = districts$CONST_COST_LIGHT_INJURY,
                const_cost_material = districts$CONST_COST_MATERIAL
            )
        PWALK(tab, one_district,
              workers = profiles$NO_OF_WORKERS[1],
              ram_needed = profiles$RAM_PER_CORE_GENERAL[1])
        logging::loginfo("clusters prep: clusters have been updated")
    },
    error = function(e) {
        logging::logerror("clusters prep failed: %s", e)
        stop("clusters prep failed---stopping evaluation")})
}
