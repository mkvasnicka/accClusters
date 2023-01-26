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

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(sf)

library(tmap)
# tmap_mode("view")



PLANARY_PROJECTION <- 5514  # Křovák
WGS84 <- 4326  # WGS84
CR_PATH <- "data/rawdata/cuzk/"
GPS_PATH <- "newData/2022_databaze_GPS.ori.csv"
EXPORT_PATH <- "newData/2022_databaze_GPS.csv"

gps_map <- function(gps, cr) {
    gps <- gps |>
        drop_na(p1, starts_with("coord")) |>
        st_as_sf(coords = c("coord_x", "coord_y"),
                 crs = PLANARY_PROJECTION)
    gps$in_cr = st_intersects(gps, cr, sparse = FALSE)
    gps
}

major_nos <- function(n) {
    str_length(as.character(round(abs(as.double(n)))))
}

plot_map <- function(gps, cr) {
    inside <- gps |> filter(in_cr)
    outside <- gps |> filter(!in_cr)
    p <- tm_shape(cr) + tm_polygons(alpha = 0, border.col = "blue")
    if (nrow(inside) > 0)
        p <- p + tm_shape(inside) + tm_dots(col = "green")
    if (nrow(outside) > 0)
        p <- p + tm_shape(outside) + tm_dots(col = "red")
    p
}


correct <- function(gps_num, cr, x, y, skip_ids, switch = FALSE) {
    # message("x is ", x, ", y is ", y)
    gps_cor <- gps_num |>
        filter(!(p1 %in% skip_ids))
    if (switch) {
        gps_cor <- gps_cor |>
            mutate(tmp = coord_y,
                   coord_y = coord_x,
                   coord_x = tmp) |>
            select(-tmp)
    }
    gps_cor <- gps_cor |>
        mutate(coord_x = coord_x / 10^x,
               coord_y = coord_y / 10^y)
    map_cor <- gps_map(gps_cor, cr)
    map_cor |> filter(in_cr) |> pull(p1)
}



cr <- st_read(CR_PATH, layer = "SPH_STAT")
st_crs(cr) <- PLANARY_PROJECTION


gps <- readr::read_csv(GPS_PATH,
                       col_types = readr::cols(.default = col_character())
)



# detection of bugs ------------------------------------------------------------

# valid range d: about -888336 -441924 -- 6 nedesetinnych cislic
# valid range e: about -1211174.6  -947687.6 -- 6 nebo 7 nedesetinnych cislic

gps_num <- gps |>
    mutate(coord_x = str_replace_all(d, ",", ".") |> as.double(),
           coord_y = str_replace_all(e, "," ,".") |> as.double())
map_num <- gps_map(gps_num, cr)
# ids of rows with correct coordinates
ids_correct <- map_num |> filter(in_cr) |> pull(p1)

# ids of rows where both coordinates must be divided by 1000
idscor3 <- correct(gps_num, cr, 3, 3, ids_correct)

# ids of rows where x coordinate must be divided by 1000
idscor30 <- correct(gps_num, cr, 3, 0, c(ids_correct, idscor3))

# ids of rows where y coordinate must be divided by 1000
idscor03 <- correct(gps_num, cr, 0, 3, c(ids_correct, idscor3, idscor30))

# ids of rows that must be corrected so far
idscor <- c(ids_correct, idscor3, idscor30, idscor03)

# try other divisors---nothing is found
grid <- expand_grid(x = -2:5, y = -2:5) |>
    filter(!((x == 0 & y == 0) | (x == 3 & y == 3) | (x == 0 & y == 3) |
                 (x == 3 & y == 0)))
res <- pmap(grid, ~correct(gps_num, cr, x = .x, y = .y, idscor))
map_int(res, length)

# ids of rows where x and y must be switched
idsswitch <- correct(gps_num, cr, 0, 0, idscor, switch = TRUE)

# ids of rows where x and y must be switched and divided by 1000
idsswitch3 <- correct(gps_num, cr, 3, 3, c(idscor, idsswitch), switch = TRUE)


# bugs correction --------------------------------------------------------------

gps_cor <- gps_num |>
    mutate(
        # switching
        tmp = coord_x,
        coord_x = if_else(p1 %in% c(idsswitch, idsswitch3), coord_y, coord_x),
        coord_y = if_else(p1 %in% c(idsswitch, idsswitch3), tmp, coord_y),
        # dividing
        coord_x = if_else(p1 %in% c(idscor3, idscor30, idsswitch3),
                          coord_x / 1e3, coord_x),
        coord_y = if_else(p1 %in% c(idscor3, idscor03, idsswitch3),
                          coord_y / 1e3, coord_y),
        # zero coordinates
        coord_x = if_else(coord_x == 0, NA_real_, coord_x),
        coord_y = if_else(coord_y == 0, NA_real_, coord_y)
    ) |>
    select(-tmp)


# test
map_corrected <- gps_map(gps_cor, cr) |>
    drop_na(starts_with("coord_"))
plot_map(map_corrected, cr)



# export -----------------------------------------------------------------------

coord <- function(n) {
    round(n, 2) |>
        as.character() |>
        str_replace("\\.", ",")
}

gps_export <- gps_cor |>
    mutate(d = coord(coord_x),
           e = coord(coord_y)) |>
    select(-coord_x, -coord_y)

write_csv(gps_export, path = EXPORT_PATH)
