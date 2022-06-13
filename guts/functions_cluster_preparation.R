# -------------------------------------
# Script:   functions_cluster_preparation.R
# Author:
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Corporation Name
# -------------------------------------

require(spdep)
require(dplyr)


# make_cluster <- function(lixels, nb, core, treshold) {
#     get_neighbors <- function(x, nb) {
#         unique(unlist(nb[x]))
#     }
#     fulfills_conditions <- function(x, lixels, treshold){
#         lixels$density[x] >= treshold
#     }
#     cluster <- core
#     last_items <- core
#     repeat {
#         neighbors <- get_neighbors(last_items, nb)
#         neighbors <- setdiff(neighbors, cluster)
#         neighbors <- neighbors[fulfills_conditions(neighbors, lixels, treshold)]
#         if (length(neighbors) == 0)
#             break
#         cluster <- unique(c(cluster, neighbors))
#         last_items <- neighbors
#     }
#     cluster
# }




cluster_condition_density <- function(x, lixels, treshold){
    lixels$density[x] >= treshold
}

produce_cluster_condition_steps <- function() {
    done <- 0L
    function(x, lixels, steps) {
        done <<- done + 1L
        (done <= steps)
    }
}

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


make_one_cluster <- function(lixels, nb, start, treshold, steps) {
    cluster_condition_steps <- produce_cluster_condition_steps()

    cluster <- get_one_cluster(lixels, nb, start,
                               cluster_condition_density, treshold)
    cluster <- get_one_cluster(lixels, nb, cluster,
                               cluster_condition_steps, steps)
    cluster
}


make_clusters <- function(lixels, nb, treshold, steps) {
    clusters <- list()
    repeat {
        non_idx <- unique(unlist(clusters))
        tab <- tibble::tibble(
            id = seq_len(nrow(lixels)),
            dens = lixels$density
        ) |>
            dplyr::filter(dens >= treshold, !(id %in% non_idx))
        if (nrow(tab) == 0)
            break
        start <- tab$id[which(tab$dens == max(tab$dens))][1]
        clusters[[length(clusters) + 1]] <-
            make_one_cluster(lixels, nb, start, treshold, steps)
    }
    clusters
}


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
