# -------------------------------------
# Script:   auxiliary_functions.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:   none
# Outputs:  functions
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# necessary packages
require(readr)
require(memuse)



# paths to files ---------------------------------------------------------------

# general template for paths to individual district files
#
# inputs:
# - folder ... (character scalar) folder where the files are stored
# - districts ... (sf tibble) the districts table
# - txt ... (character scalar) general template in glue format
#   supported parts:
#   - id ... id numbers of districts
#   - name ... names of districts
#
# value:
#   character vector of paths to specific files;
#   if is.null(folder), only file name is returned
basic_file_name <- function(folder, districts, txt) {
    fname <- glue::glue(txt,
                      id = districts$district_id,
                      name = districts$district_name) |>
        as.character()
    if (is.null(folder))
        return(fname)
    file.path(folder, fname)
}

# these functions return paths to individual district files of various types
#
# inputs:
# - districts ... (sf tibble) the districts table
# - folder ... (character scalar) folder where the files are stored;
#   if is.null(folder) (implicit value) then only file name is returned
#
# value:
#   character vector of paths to specific files; see folder input

# paths to geojson files describing individual districts
geojson_file_name <- function(districts, folder = NULL) {
    basic_file_name(folder, districts, "district_{id}.geojson")
}

# paths to osm files including selected roads in particual districts
osm_file_name <- function(districts, folder = NULL) {
    basic_file_name(folder, districts, "district_{id}.osm")
}

# paths to sfnetwork files including selected roads in particual districts
sf_file_name <- function(districts, folder = NULL) {
    basic_file_name(folder, districts, "district_{id}.rds")
}

# paths to files including lixelated selected roads in particual districts
lixel_file_name <- function(districts, folder = NULL) {
    basic_file_name(folder, districts, "lixel_{id}.rds")
}

# paths to files including mid-points of lixelated selected roads in particual
# districts
lixel_sample_file_name <- function(districts, folder = NULL) {
    basic_file_name(folder, districts, "lixel_sample_{id}.rds")
}

# paths to files including neighbors' lists of lixelated selected roads in
# particual districts
lixel_nb_file_name <- function(districts, folder = NULL) {
    basic_file_name(folder, districts, "lixel_nb_{id}.rds")
}

# paths to files including accidents cropped to particular districts snapped to
# selected roads there
accidents_file_name <- function(districts, folder = NULL) {
    basic_file_name(folder, districts, "accidents_{id}.rds")
}

# paths to files including lixelated selected roads in particual districts with
# added NKDE densities
densities_file_name <- function(districts, folder = NULL) {
    basic_file_name(folder, districts, "densities_{id}.rds")
}



# reading/writing files --------------------------------------------------------

# write_dir_rds(object, file) writes one object to a path; it doesn't compress
# the file; it creates all necessary (sub)folders in the path
#
# inputs:
# - object ... any kind of object
# - file ... (character scalar) a paht to the object
#
# value:
#   none
#
# usage:
#   write_dir_rds(districts, "/tmp/boo.rds")
#   districts <- readr::read_rds("/tmp/boo.rds")
write_dir_rds <- function(object, file) {
    dir <- dirname(file)
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    readr::write_rds(object, file = file, compress = "none")
}



# updating files ---------------------------------------------------------------

# is_behind(target, source) returns TRUE if any target files is older than any
# source files or if it doesn't exist; it returns FALSE otherwise
#
# inputs:
# - target ... (character vector) paths to files depending on source
# - source ... (character vector) paths to files used to create target
#
# values:
#   logical, see above
#
# usage:
#   if (is_behind(PATH_TO_DISTRICTS, PATH_TO_RAW_DISTRICTS)) {...}
is_behind <- function(target, source) {
    mtarget <- file.mtime(target)
    msource <- file.mtime(source)

    if (any(is.na(msource)))
        stop("Some sources don't exist:",
             source[is.na(msource)])

    if (any(is.na(mtarget)))
        return(TRUE)

    max(msource) > min(mtarget)
}



# parallel processing ----------------------------------------------------------

get_number_of_workers <- function(workers) {
    if (is.null(workers))
        workers <- ifelse(exists("NO_OF_WORKERS"), NO_OF_WORKERS, 1)
    if (workers == "auto") {
        ram <- as.numeric(memuse::Sys.meminfo()$freeram) / 1024 ^ 3
        no_of_cores <- future::availableCores()
        workers <- min(no_of_cores, floor(ram / 2.5))}
    workers
}


PWALK <- function(.l, .f, workers = 1, ...) {
    workers <- get_number_of_workers(workers)

    if (workers == 1) {
        purrr::pwalk(.l, .f, ...)
    } else {
        oplan <- future::plan()
        future::plan("multisession", workers = workers)
        furrr::future_pwalk(.l, .f, ...,
                            .options = furrr::furrr_options(seed = TRUE))
        future::plan(oplan)
    }
}



# damage costs -----------------------------------------------------------------

# accident_damage_cost() computes each accident's damage cost
#
# inputs:
# - dead ... (integer vector) number of casualties in accidents (P13A)
# - serious_injury ... (integer vector) number of seriously injured in accidents
#   (P13B)
# - light_injury ... (integer vector) number of light injured in accidents
#   (P13C)
# - material_cost ... (numeric vector) total material cost in 100 CZK (P14)
# - unit_costs ... list three named slots:
#   - dead ... (numeric scalar) casualty cost in millions CZK, i.e., value of
#       statistical life
#   - serious_injury ... (numeric scalar) cost of one serious injury in millions
#       CZK
#   - light_injury ... (numeric scalar) cost of one light injury in millions CZK
#
# value:
#   numeric vector of damage costs in millions of CZK; each number is the total
#   damage of the corresponding accident
accident_damage_cost <- function(dead, serious_injury, light_injury,
                                 material_cost, unit_costs) {
    dead * unit_costs$dead +
        serious_injury * unit_costs$serious_injury +
        light_injury * unit_costs$light_injury +
        material_cost * 1e2 / 1e6
}
