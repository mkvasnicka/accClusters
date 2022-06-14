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
    make_clusters(lixels, nb, threshold, steps) |>
        join_clusters() |>
        purrr::map(~tibble(lixel_id = unlist(.))) |>
        dplyr::bind_rows(.id = "cluster") |>
        dplyr::mutate(cluster = as.integer(cluster))
}



# cluster to lixels/accidents --------------------------------------------------

add_clusters_to_lixels <- function(lixels, clusters) {
    dplyr::left_join(lixels, clusters, by = "lixel_id")
}


add_clusters_to_accidents <- function(accidents, clusters) {
    dplyr::left_join(accidents, clusters, by = "lixel_id")
}



# compute cluster costs --------------------------------------------------------

cluster_cost <- function(accidents, unit_costs) {
    accidents |>
        sf::st_drop_geometry() |>
        dplyr::filter(!is.na(cluster)) |>
        dplyr::group_by(cluster) |>
        dplyr::summarise(cost = sum(accident_cost), .groups = "drop")
}



graphic_clusters <- function(lixels, accidents, clusters, unit_costs) {
    clstrs <- add_clusters_to_lixels(lixels, clusters) |>
        dplyr::filter(!is.na(cluster)) |>
        dplyr::group_by(cluster) |>
        dplyr::summarise(total_length = sum(len),
                         total_density = sum(density),
                         geometry = st_union(geometry),
                         .groups = "drop")
    accdnts <- add_clusters_to_accidents(accidents, clusters)
    costs <- cluster_cost(accdnts, unit_costs)
    dplyr::left_join(clstrs, costs, by = "cluster") |>
        dplyr::mutate(cost_per_meter = cost / total_length)
}


# faster than graphic_clusters, no graphical representation
gr_clusters <- function(lixels, accidents, clusters) {
    clstrs <- lixels |>
        st_drop_geometry() |>
        add_clusters_to_lixels(clusters) |>
        dplyr::filter(!is.na(cluster)) |>
        dplyr::group_by(cluster) |>
        dplyr::summarise(total_length = sum(len),
                         total_density = sum(density),
                         .groups = "drop")
    accdnts <- accidents |>
        st_drop_geometry() |>
        add_clusters_to_accidents(clusters) |>
        dplyr::filter(!is.na(cluster)) |>
        dplyr::group_by(cluster) |>
        dplyr::summarise(cost = sum(accident_cost), .groups = "drop")
    dplyr::left_join(clstrs, accdnts, by = "cluster") |>
        dplyr::mutate(cost_per_meter = cost / total_length)
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


optimize_cluster_parameters <- function(lixels, nb, accidents) {
    f <- function(x) {
        threshold <- x[1]
        no_of_steps <- round(x[2])
        cls <- compute_cluster_tibble(lixels, nb, threshold, no_of_steps)
        clstrs <- gr_clusters(lixels, accidents, cls)
        pai <- cluster_pai(clstrs, accidents, lixels)
        ifelse(is.na(pai), Inf, -pai)
    }
    optim(par = c(0.995, 10),
          fn = f,
          lower = c(0.95, 1), upper = c(1, 60),
          lixels = lixels, nb = nb, accidents = accidents
    )
}
