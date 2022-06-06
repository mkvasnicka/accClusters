# -------------------------------------
# Script:
# Author:
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Corporation Name
# -------------------------------------

# https://stackoverflow.com/questions/62119516/generate-neighbour-list-object-for-spatial-lines-in-r

library(tidyverse)
library(sfnetworks)
library(sf)
library(spdep)
library(igraph)

net <- as_sfnetwork(roxel, directed = FALSE) %>%
    activate("edges") %>%
    mutate(road_id = row_number() + 1000)

# 1) origina approach

system.time({
# Make adjacency matrix
B_net <- igraph::as_adjacency_matrix(net, edges = TRUE, attr = names)

# Make neighbour list
nb_net <- mat2listw(B_net)$neighbours
# Can't use row.names in mat2listw because how do I set row.names in igraph::as_adjacency_matrix
})

# 2) new approach

net_sf <- st_as_sf(net)  # zbytečné, jde dělat na net -- ale na na net_sf nepatrně rychlejší

system.time({
net_neigh <- st_touches(net_sf)
# net_neigh <- st_touches(net)

# define ad-hoc function to translate sgbp into nb (as documented in
# https://r-spatial.github.io/spdep/articles/nb_sf.html#creating-neighbours-using-sf-objects)
as.nb.sgbp <- function(x) {
    attrs <- attributes(x)
    x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
    attributes(x) <- attrs
    class(x) <- "nb"
    x
}

net_nb <- as.nb.sgbp(net_neigh)
})

net_lw <- nb2listw(net_nb)


brno <- read_rds("guts/data/maps/district_40711.rds")
system.time(brno_neigh <- brno |> st_touches() |> as.nb.sgbp())

ii <- map_lgl(brno_neigh, ~(length(.) == 1 & 0L %in% .)) |> which()

library(tmap)
tmap_mode("view")
tm_shape(brno) + tm_lines() + tm_shape(brno[ii, ]) + tm_lines(col = "red", lwd = 5)


brno_simp <- simplify_sf(brno)
system.time(brno_neigh_simp <- brno_simp |> st_touches() |> as.nb.sgbp())
ii_simp <- map_lgl(brno_neigh_simp, ~(length(.) == 1 & 0L %in% .)) |> which()
tm_shape(brno_simp) + tm_lines() + tm_shape(brno_simp[ii_simp, ]) + tm_lines(col = "red", lwd = 5)


tm_shape(brno) + tm_lines() +
    tm_shape(brno_simp) + tm_lines(col = "green") +
    tm_shape(brno[ii, ]) + tm_lines(col = "blue", lwd = 5) +
    tm_shape(brno_simp[ii_simp, ]) + tm_lines(col = "red", lwd = 5)
