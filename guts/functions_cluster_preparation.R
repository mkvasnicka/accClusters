# -------------------------------------
# Script:   functions_cluster_preparation.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

require(spdep)
require(dplyr)


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
    repeat {
        non_idx <- unique(unlist(clusters))
        tab <- tibble::tibble(
            id = seq_len(nrow(lixels)),
            dens = lixels$density
        ) |>
            dplyr::filter(dens >= threshold, !(id %in% non_idx))
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
join_clusters <- function(clusters) {
    i <- 1L
    joined <- FALSE
    while (i <= length(clusters)) {
        j <- i + 1L
        while (j <= length(clusters)) {
            if (length(base::intersect(clusters[[i]], clusters[[j]])) > 0) {
                clusters[[i]] <- unique(c(clusters[[i]], clusters[[j]]))
                clusters[[j]] <- NULL
                joined <- TRUE
            } else {
                j <- j + 1L
            }
        }
        if (!joined)
            i <- i + 1L
        joined <- FALSE
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
compute_cluster_tibble <- function(lixels, nb, threshold, steps) {
    lst <- make_clusters(lixels, nb, threshold, steps) |>
        join_clusters()
    if (length(lst) == 0)
        return(tibble::tibble(cluster = integer(0), lixel_id = integer(0)))
    lst |>
        purrr::map(~tibble(lixel_id = unlist(.))) |>
        dplyr::bind_rows(.id = "cluster") |>
        dplyr::mutate(cluster = as.integer(cluster))
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

# cluster_cost(accidents, accident_cost) computes cost of each cluster
#
# inputs:
# - accidents ... (sf tibble) table of accidents snapped to roads in particular
#   district
# - accident_cost ... (character scalar) name of column in accidents that
#   includes the cost of individual accidents
#
# value:
cluster_cost <- function(accidents, accident_cost = "accident_cost") {
    accidents$.acccost <- accidents[["accident_cost"]]
    accidents |>
        dplyr::filter(!is.na(cluster)) |>
        dplyr::group_by(cluster) |>
        dplyr::summarise(cost = sum(.acccost), .groups = "drop")
}


# cluster_statistics() computes tibble with basic statistics for each cluster
#
# inputs:
# - lixels
# - accidents
# - clusters
# - accident_cost
#
# value:
#   tibble with four columns:
#   - cluster ... (integer) cluster id
#   - total_length ... (double) clusters' total lengths in meters
#   - total_density ... (double) sum of densities of lixels within clusters
#   - cost ... (double) sum of cost of accidents lying on lixels within clusters
#       in mil. CZK
#   - cost_per_meter (double) cost / total_length
cluster_statistics <- function(lixels, accidents, clusters,
                               accident_cost = "accident_cost") {
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
        cluster_cost(accident_cost)
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
    cluster_cost <- sum(cluster$cost)
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
# - from_date, to_date ... (Date scalars) describe date range of accidents that
#   are used
# - cluster_min_quantile ... (numeric scalar in (0, 1)) threshold for lixels to
#   constitute clusters in step 1, see notes
# - cluster_steps ... (non-negative integer scalar) how many lixels are
#   recursively added to clusters
# - visual_min_quantile ... (numeric scalar in (0, 1)) threshold for lixel
#   density for lixels that will be stored in lixel table
# - workers ... (NULL or integer scalar) number of cores used in parallel
# - other_files ... (character vector) pathes to other files that can determine
#   whether existing files are up-to-date
#
# value:
#   none; data are written to disk
#
# notes:
# - clusters are found in two steps:
#   1. all consecutive lixels with densities above or at cluster_min_quantile
#       constitute clusters
#   2. cluster_steps lixels are recursively added to all clusters
compute_one_time_clusters <- function(districts,
                                      densities_dir,
                                      lixel_maps_dir,
                                      accidents_dir,
                                      cluster_dir,
                                      from_date, to_date,
                                      cluster_min_quantile, cluster_steps,
                                      visual_min_quantile,
                                      workers = NULL,
                                      other_files) {
    one_district <- function(densities_file, lixel_nb_file, accident_file,
                             shiny_file,
                             cluster_min_quantile, cluster_steps,
                             visual_min_quantile) {
        lixels <- readr::read_rds(densities_file)
        nb <- readr::read_rds(lixel_nb_file)
        accidents <- readr::read_rds(accident_file)
        threshold <- quantile(lixels$density, cluster_min_quantile)
        cls <- compute_cluster_tibble(lixels, nb, threshold, cluster_steps)
        clss <- cluster_statistics(lixels, accidents, cls)
        visual_threshold <- quantile(lixels$density, visual_min_quantile)

        lixels <- lixels |>
            add_clusters_to_lixels(cls) |>
            dplyr::filter(density >= visual_threshold | !is.na(cluster)) |>
            dplyr::select(lixel_id, density, cluster)
        accidents <- accidents |>
            add_clusters_to_accidents(cls) |>
            sf::st_drop_geometry() |>
            dplyr::filter(!is.na(cluster)) |>
            dplyr::select(p1, cluster, accident_cost)
        cluster_statistics <- clss
        write_dir_rds(
            list(lixels = lixels, accidents = accidents,
                 cluster_statistics = cluster_statistics),
            file = shiny_file)
    }

    from_date <- as.Date(from_date)
    to_date <- as.Date(to_date)
    stopifnot(from_date <= to_date)

    profile_name <- NULL
    if (exists("PROFILE_NAME"))
        profile_name <- PROFILE_NAME

    workers <- get_number_of_workers(workers)
    districts <- districts_behind(districts,
                                  target_fun = shiny_file_name,
                                  source_fun = list(lixel_nb_file_name,
                                                    accidents_file_name),
                                  target_folder = densities_dir,
                                  source_folder = list(lixel_maps_dir,
                                                       accidents_dir),
                                  other_files = other_files,
                                  from_date = from_date, to_date = to_date,
                                  profile_name = profile_name)
    tab <- tibble::tibble(
        densities_file = densities_file_name(districts, densities_dir,
                                             from_date = "2019-01-01",
                                             to_date = "2021-12-31",
                                             profile_name = profile_name),
        lixel_nb_file = lixel_nb_file_name(districts, lixel_maps_dir),
        accident_file = accidents_file_name(districts, accidents_dir),
        shiny_file = shiny_file_name(districts, cluster_dir,
                                     from_date = from_date,
                                     to_date = to_date,
                                     profile_name = profile_name)
    )
    PWALK(tab, one_district,
          # from_date, to_date,
          cluster_min_quantile = cluster_min_quantile,
          cluster_steps = cluster_steps,
          visual_min_quantile = visual_min_quantile)
}


# compute_clusters() computes hotspot clusters for all periods in specified time
# windows
#
# inputs:
# - districts ... (sf tibble) districts table
# - densities_dir ... (character scalar) path to folder where densities are
#   stored
# - lixel_maps_dir ... (character scalar) path to folder where where nbs for
#   lixelized roads in individual districts are stored
# - accidents_dir ... (character scalar) path to folder where accidents snapped
#   to roads in individual districts are stored
# - path_to_time_window_file ... (character scalars) path to file where time
#   window table is stored
# - cluster_min_quantile ... (numeric scalar in (0, 1)) threshold for lixels to
#   constitute clusters in step 1, see notes
# - cluster_steps ... (non-negative integer scalar) how many lixels are
#   recursively added to clusters
# - visual_min_quantile ... (numeric scalar in (0, 1)) threshold for lixel
#   density for lixels that will be stored in lixel table
# - workers ... (NULL or integer scalar) number of cores used in parallel
# - other_files ... (character vector) pathes to other files that can determine
#   whether existing files are up-to-date
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
                             path_to_time_window_file,
                             cluster_min_quantile, cluster_steps,
                             visual_min_quantile,
                             workers = NULL,
                             other_files) {
    time_window <- read_time_window_file(path_to_time_window_file)
    purrr::walk(seq_len(nrow(time_window)),
                ~compute_one_time_clusters(districts = districts,
                                           densities_dir = densities_dir,
                                           lixel_maps_dir = lixel_maps_dir,
                                           accidents_dir = accidents_dir,
                                           cluster_dir = cluster_dir,
                                           from_date = time_window$from_date[.],
                                           to_date = time_window$to_date[.],
                                           cluster_min_quantile = cluster_min_quantile,
                                           cluster_steps = cluster_steps,
                                           visual_min_quantile = visual_min_quantile,
                                           workers = workers,
                                           other_files = other_files)
    )
}
