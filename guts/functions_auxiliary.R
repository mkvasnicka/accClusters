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
require(dplyr)
require(readr)
require(stringr)
require(memuse)
require(logging)



# parsing command-line parameters ----------------------------------------------

# process_command_line_arguments() processes optional command-line parameters;
# only --profile=filename.R is handled now
#
# presently, the function finds all --profile==filename.R paramters and sources
# them---in their order in the command line
#
# inputs:
#   none
#
# value:
#   none
process_command_line_arguments <- function(rdir) {
    get_parameter <- function(params, key) {
        stringr::str_subset(params, stringr::str_c("--", key, "=.*")) |>
            stringr::str_remove(stringr::str_c("--", key, "="))
    }

    cl_pars <- commandArgs()
    profile <- get_parameter(cl_pars, "profile")
    purrr::walk(profile, ~source(file.path(rdir, .)))
}



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
basic_file_name <- function(folder, districts, txt, ...) {
    pars <- list(...)
    pars$id <- districts$district_id
    pars$name <- districts$district_name
    fname <- glue::glue(txt, .envir = pars) |>
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
geojson_file_name <- function(districts, folder = NULL, ...) {
    basic_file_name(folder, districts, "district_{id}.geojson")
}

# paths to osm files including selected roads in particual districts
osm_file_name <- function(districts, folder = NULL, ...) {
    basic_file_name(folder, districts, "district_{id}.osm")
}

# paths to sfnetwork files including selected roads in particual districts
sf_file_name <- function(districts, folder = NULL, ...) {
    basic_file_name(folder, districts, "district_{id}.rds")
}

# paths to files including lixelated selected roads in particual districts
lixel_file_name <- function(districts, folder = NULL, ...) {
    basic_file_name(folder, districts, "lixel_{id}.rds")
}

# paths to files including mid-points of lixelated selected roads in particual
# districts
lixel_sample_file_name <- function(districts, folder = NULL, ...) {
    basic_file_name(folder, districts, "lixel_sample_{id}.rds")
}

# paths to files including neighbors' lists of lixelated selected roads in
# particual districts
lixel_nb_file_name <- function(districts, folder = NULL, ...) {
    basic_file_name(folder, districts, "lixel_nb_{id}.rds")
}

# paths to files including accidents cropped to particular districts snapped to
# selected roads there
accidents_file_name <- function(districts, folder = NULL, ...) {
    basic_file_name(folder, districts, "accidents_{id}.rds")
}

# paths to files including lixelated selected roads in particual districts with
# added NKDE densities
densities_file_name <- function(districts, folder = NULL, ...) {
    pars <- list(...)
    stopifnot(all(c("from_date", "to_date") %in% names(pars)))
    txt <- stringr::str_c(
        "densities_{id}_",
        if ("profile_name" %in% names(pars) && !is.null(pars$profile_name))
            "{profile_name}_"
        else
            "",
        "{as.character(from_date)}_{as.character(to_date)}.rds")
    basic_file_name(folder, districts, txt, ...)
}

# paths to files including the final product used in Shiny
shiny_file_name <- function(districts, folder = NULL, ...) {
    pars <- list(...)
    stopifnot(all(c("from_date", "to_date") %in% names(pars)))
    txt <- stringr::str_c(
        "shiny_{id}_",
        if ("profile_name" %in% names(pars) && !is.null(pars$profile_name))
            "{profile_name}_"
        else
            "",
        "{as.character(from_date)}_{as.character(to_date)}.rds")
    basic_file_name(folder, districts, txt, ...)
}



# reading/writing files --------------------------------------------------------

# create_dir_for_file(file) recursively creates folders needed to save file
#
# inputs:
# - file ... (character scalar) path to file
#
# value:
#   none; it creates folders if needed
create_dir_for_file <- function(file) {
    dir <- dirname(file)
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
}


# write_dir_rds(object, file) writes one object to a path; it doesn't compress
# the file; it creates all necessary (sub)folders in the path
#
# inputs:
# - object ... any kind of object
# - file ... (character scalar) a path to the object
#
# value:
#   none
#
# usage:
#   write_dir_rds(districts, "/tmp/boo.rds")
#   districts <- readr::read_rds("/tmp/boo.rds")
write_dir_rds <- function(object, file) {
    create_dir_for_file(file)
    readr::write_rds(object, file = file, compress = "none")
}


# write_dir_rdata(..., file) writes all objects in ... to a path; it doesn't
# compress the file; it creates all necessary (sub)folders in the path
#
# inputs:
# - ... ... any kind of objects
# - file ... (character scalar) a path to the object
#
# value:
#   none
#
# WARNING: this function may not work when used deep in functions
write_dir_rdata <- function(..., file) {
    create_dir_for_file(file)
    save(..., file = file, compress = FALSE)
}


# read_time_window_file(path) reads in table with time windows for which
# accidnets cluster should be computed
#
# inputs:
# - path ... (character scalar) path to the file defining time windows;
#   - file must be TSV, i.e., TAB separated textual file
#   - its header must be from-date and to-date
#   - dates must be in format YYYY-MM-DD
#
# value:
#   tibble with two Date columns (from_date and to_date)
read_time_window_file <- function(path) {
    read_tsv(path, col_types = cols(.default = col_date(format = "%Y-%m-%d")))
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
        stop("Some sources don't exist: ",
             str_c(source[is.na(msource)], collapse = ", "))

    if (any(is.na(mtarget)))
        return(TRUE)

    max(msource) > min(mtarget)
}


# districts_behind() returns subset of districts for which the computation is
# behind
#
# inputs:
# - districts ... (sf tibble) tibble of districts
# - target_fun ... (function) name function of the target, such as
#   lixel_file_name
# - source_fun ... (function) name function of the target, such as sf_file_name
# - target_folder ... (character scalar) path to folder where target files
#   should be stored
# - source_folder ... (character scalar) path to folder where source files
#   are be stored
# - other_files ... (optional character vector) paths to other files that
#   condition target creation
#
# value:
#   subset of (rows of) districts table; it includes only its rows for which
#   targets must be created because they either don't exist or are behind
districts_behind <- function(districts, target_fun, source_fun,
                             target_folder, source_folder,
                             other_files = NULL,
                             ...) {
    target_files <- target_fun(districts, target_folder, ...)
    mtarget <- file.mtime(target_files)

    if (is.list(source_fun)) {
        if (!is.list(source_folder) ||
            length(source_fun) != length(source_folder))
            stop("If source_fun is a list, source_folder must ",
                 "be a list of the same length.")
        msource <- -Inf
        for (k in seq_along(source_fun)) {
            source_files <- source_fun[[k]](districts, source_folder[[k]], ...)
            msource <- pmax(msource, file.mtime(source_files))
        }
    } else {
        source_files <- source_fun(districts, source_folder, ...)
        msource <- file.mtime(source_files)
    }

    if (is.null(other_files)) {
        mother <- -Inf
    } else{
        mother <- max(file.mtime(other_files))
    }

    if (any(is.na(msource)))
        stop("Some sources don't exist: ",
             str_c(source_files[is.na(msource)], collapse = ", "))

    ids <- is.na(mtarget) | (mtarget < msource) | (mtarget < mother)

    districts[ids, ]
}



# parallel processing ----------------------------------------------------------

get_number_of_workers <- function(workers, ram_needed = RAM_PER_CORE_GENERAL) {
    if (is.null(workers))
        workers <- ifelse(exists("NO_OF_WORKERS"), NO_OF_WORKERS, 1)
    if (workers == "auto") {
        ram <- as.numeric(memuse::Sys.meminfo()$freeram) / 1024 ^ 3
        no_of_cores <- future::availableCores()
        workers <- min(no_of_cores, floor(ram / ram_needed))}
    workers
}


PWALK <- function(.l, .f, workers = 1, ...) {
    workers <- get_number_of_workers(workers)
    .f <- purrr::possibly(.f, otherwise = NULL)
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



# logging ----------------------------------------------------------------------

# create_log_file(log_folder) creates a new log file in log_folder; it creates
# the folder, too, if necessary
#
# inputs:
# - log_folder (character scalar) path to a folder where logs should be stored
#
# value:
#   none; it only creates the file (and the folder, if necessary)
#
# notes:
# - the reason for this approach is following: I want individual steps in data
#   preparation to run in vanilla fresh R each time, but I want all logging into
#   one file; therefore, I have to start logging into the newest existing file;
#   therefore, there must be a way to create a new log file
create_log_file <- function(log_folder) {
    dir.create(log_folder, showWarnings = FALSE, recursive = TRUE)
    time <- as.character(Sys.time())
    log_file <- stringr::str_c(
        stringr::str_replace_all(time, "[\\s:]", "-"),
        ".log")
    readr::write_lines(
        stringr::str_c(time, " CREATED log file"),
        file.path(log_folder, log_file)
    )
}


# start_logging(log_folder) starts logging into the newest .log file in
# log_folder
#
# inputs:
# - log_folder (character scalar) path to a folder where logs arestored
#
# value:
#   none; it starts logging into the newest .log file in log_folder
#
# notes:
# - for reason why it is done this way, see notes to create_log_file()
start_logging <- function(log_folder, console = FALSE) {
    logging::basicConfig()
    log_file <- list.files(log_folder, pattern = "\\.log", full.names = TRUE) |>
        file.info() |>
        dplyr::arrange(dplyr::desc(ctime)) |>
        dplyr::slice(1) |>
        rownames()
    logging::addHandler(writeToFile, file = log_file)
    if (console)
        logging::removeHandler("basic.stdout")
}


