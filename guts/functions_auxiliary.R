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
        workers <- if_else(exists("NO_OF_WORKERS"), NO_OF_WORKERS, 1)
    workers
}


PWALK <- function(.l, .f, workers = 1, ...) {
    workers <- get_number_of_workers(workers)

    if (workers == 1) {
        purrr::pwalk(.l, .f, ...)
    } else {
        oplan <- future::plan()
        future::plan("multisession", workers = workers)
        furrr::future_pwalk(.l, .f, ...)
        future::plan(oplan)
    }
}
