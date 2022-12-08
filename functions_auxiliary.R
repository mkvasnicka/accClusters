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
library(dplyr, verbose = FALSE, warn.conflicts = FALSE)
library(readr, verbose = FALSE, warn.conflicts = FALSE)
library(stringr, verbose = FALSE, warn.conflicts = FALSE)
library(memuse, verbose = FALSE, warn.conflicts = FALSE)
library(logging, verbose = FALSE, warn.conflicts = FALSE)



# parsing command-line parameters ----------------------------------------------

# process_command_line_arguments() processes optional command-line parameters
#
# presently, the function handles:
# - --profile=filename.R parameters---it sources them---in their order in the
#   command line
# - --workers=# where # is either positive integer scalar or "auto"
#
# inputs:
#   none
#
# value:
#   none; the function reads global (config) variables into the R working space
#
# WARNINGS:
# - there must be no space in names, etc., as any white space is supposed to
#   separate parameters
process_command_line_arguments <- function(rdir) {
    get_parameter <- function(params, key) {
        stringr::str_subset(params, stringr::str_c("--", key, "=.*")) |>
            stringr::str_remove(stringr::str_c("--", key, "="))
    }

    cl_pars <- commandArgs() |>
        stringr::str_split("\\s+", simplify = TRUE) |>
        t() |>
        as.vector() |>
        str_subset("^\\-\\-.+$")

    profile <- get_parameter(cl_pars, "profile")
    purrr::walk(profile, ~source(file.path(rdir, .)))

    workers <- get_parameter(cl_pars, "workers")
    if (length(workers) > 0) {
        stopifnot(length(workers) == 1)
        if (workers != "auto") {
            workers <- as.numeric(workers)
            stopifnot(!is.na(workers) && workers == round(workers))
        }
        NO_OF_WORKERS <<- workers
    }
}



# paths to folders -------------------------------------------------------------


# path to raw data
raw_data_dir <- function() {
    file.path(DIR_ORIGIN, "rawdata")
}

# path to districts shape file folder
path_to_raw_districts <- function() {
    file.path(raw_data_dir(),
              # "arccr", "arccr_4_1.gdb")
              "cuzk")
}

# path to OSM maps
path_to_raw_roads_osm <- function() {
    file.path(raw_data_dir(), "osm", "czech-republic-latest.osm.pbf")
}


# path to original accidents file
path_to_raw_accidents_dir <- function() {
    file.path(raw_data_dir(), "accidents")
}

# path to file with all processed accidents
path_to_raw_accidents <- function() {
    file.path(path_to_accidents_dir(), "all_accidents.rds")
}

# path to created data folder
data_dir <- function() {
    file.path(DIR_ORIGIN, "tmp")
}

# path to districts
path_to_districts <- function() {
    file.path(data_dir(), "districts", "districts.rds")
}

# path to filtered/converted maps folder
path_to_map_dir <- function() {
    file.path(data_dir(), "maps")
}

# path to lixels' folder
path_to_lixels_maps_dir <- function() {
    file.path(data_dir(), "lixels")
}

# paths to accidents data by districts
path_to_accidents_dir <- function() {
    file.path(data_dir(), "accidents")
}

# path to NKDEs by districts
path_to_densities_dir <- function() {
    file.path(data_dir(), "densities")
}


# outputs
output_dir <- function() {
    file.path(DIR_ORIGIN, "output")
}

# path to districts for shiny
path_to_shiny_districts <- function() {
    file.path(output_dir(), "districts", "districts.rds")
}

# path to folder where accidents in individual districts for shiny are stored
path_to_shiny_accidents_dir <- function() {
    file.path(output_dir(), "accidents")
}

# path to folder where the final product used in shiny in stored
shiny_dir <- function() {
    file.path(output_dir(), "clusters")
}

# path to folder where finald product for GIS (shape file) is stored
gis_dir <- function() {
    file.path(output_dir(), "gis")
}


# logging
log_dir <- function() {
    file.path(DIR_ORIGIN, "log")
}


# path to source profiles and config
path_to_source_configs <- function() {
    file.path(DIR_ORIGIN, "config")
}

# path to file where profiles are stored
path_to_configs <- function() {
    file.path(path_to_source_configs(), "profiles.rds")
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
        "clusters_{id}_",
        if ("profile_name" %in% names(pars) && !is.null(pars$profile_name))
            "{profile_name}_"
        else
            "",
        "{as.character(from_date)}_{as.character(to_date)}.rds")
    basic_file_name(folder, districts, txt, ...)
}



# accidents files regexes ------------------------------------------------------

# regex for major accident-data file
accidents_file_name_pattern <- function() {
    "\\d{4}_databaze_nehody.csv"
}

# regex for major gps file of accident data
accidents_gps_name_pattern <- function() {
    "\\d{4}_databaze_GPS.csv"
}

# regex for outcomes file of accident data
accidents_outcomes_name_pattern <- function() {
    "\\d{4}_databaze_nasledky.csv"
}

# regex for pedestrians file of accident data
accidents_pedestrians_name_pattern <- function() {
    "\\d{4}_databaze_chodci.csv"
}

# regex for vehicles file of accident data
accidents_vehicles_name_pattern <- function() {
    "\\d{4}_databaze_vozidla.csv"
}

# how many first rows should be skipped in each CSV file
ACCIDENTS_FILES_SKIP <- 6L



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



# handling time windows --------------------------------------------------------

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


# handle_time_window(time_window) checks and prepares time windows
#
# inputs:
# - time_window ... (either character scalar or tibble with two columns)
#   - if time_window is character scalar, it is a path to TSV file where time
#       window is stored; see help for read_time_window_file()
#   - if time_window is tibble, it must have two columns (from_date and to_date)
#       which includes Dates or character vectors in YYYY-MM-DD format
#       convertible to Dates
#
# value:
#   tibble with two Dates columns (from_date, to_date)
handle_time_window <- function(time_window) {
    if (is.character(time_window))
        time_window <- read_time_window_file(time_window)
    stopifnot(tibble::is_tibble(time_window),
              nrow(time_window) > 0,
              all(c("from_date", "to_date") %in% names(time_window)))
    time_window <- time_window |>
        dplyr::mutate(from_date = as.Date(from_date),
                      to_date = as.Date(to_date)) |>
        dplyr::distinct()
    stopifnot(all(!is.na(time_window$from_date)),
              all(!is.na(time_window$to_date)),
              all(time_window$from_date <= time_window$to_date))
    time_window
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

    if (any(is.na(msource))) {
        logging::logerror("Some sources don't exist: %s.",
                          str_c(source[is.na(msource)], collapse = ", "))
        stop("Some sources don't exist: ",
             str_c(source[is.na(msource)], collapse = ", "))
    }

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
            stop("When source_fun is a list, source_folder must ",
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

# docker_mem_limit() returns amount of memory available to the docker container
# in which it's running
#
# inputs:
#   none
#
# value:
#   either available memory in GBs or Inf if unconstrained
#
# notes:
# - it may be necessary to add a new file in mem_limit() in a new version
#   of Docker
docker_mem_limit <- function() {
    mem_limit <- function(file) {
        ml <- NA
        if (file.exists(file)) {
            ml <- base::readLines(file) |>
                as.numeric() / 1024^3
        }
        ml
    }

    suppressWarnings(
        c(
            mem_limit("/sys/fs/cgroup/memory/memory.limit_in_bytes"),
            mem_limit("/sys/fs/cgroup/memory.max")
        ) |>
            min(na.rm = TRUE)
    )
}


# available_memory() returns memory available to the process at the moment in
# GBs
#
# inputs:
#   none
#
# value:
#   available memory in GBs
#
# notes:
# - if run from inside a docker container, it returns the minimum of free memory
#   at the moment and the size of the memory available to the container
# - it returns the amount of free memory at the momenet otherwise
available_memory <- function() {
    min(
        docker_mem_limit(),
        as.numeric(memuse::Sys.meminfo()$freeram) / 1024 ^ 3
    )
}


# docker_cpu_limit() returns number of cpus available to the docker container
# in which it's running
#
# inputs:
#   none
#
# value:
#   either number of cpus or Inf if unconstrained
#
# notes:
# -
# - it may be necessary to add a new file in cpu_limit() in a new version
#   of Docker
docker_cpu_limit <- function() {
    cpu_limit <- function(quota_file, period_file = "") {
        quota <- NA
        period <- 1e5
        if (file.exists(quota_file)) {
            if (file.exists(period_file)) {
                quota <- as.numeric(base::readLines(quota_file))[1]
                period <- as.numeric(base::readLines(period_file))[1]
            } else {
                cl <- base::readLines(quota_file) |>
                    base::strsplit(split = "\\s+")
                quota <- as.numeric(cl[[1]][1])
                period <- as.numeric(cl[[1]][2])
            }
        }
        max(quota %/% period, 1)
    }

    suppressWarnings(
        c(
            cpu_limit("/sys/fs/cgroup/cpu/cpu.cfs_quota_us",
                      "/sys/fs/cgroup/cpu/cpu.cfs_period_us"),
            cpu_limit("/sys/fs/cgroup/cpu.max")
        ) |>
            min(na.rm = TRUE) |>
            floor()
    )
}


# available_cores() returns the number of cores available at the moment
#
# inputs:
#   none
#
# value:
#   (integer scalar) number of available cpu cores
#
# notes:
# - if run from inside a docker container, it returns the minimum of the number
#   of cores dedicated to the docker container and the number of cores available
#   at the machine
# - it returns the number of cores available at the machine otherwise
available_cores <- function() {
    min(
        docker_cpu_limit(),
        parallelly::availableCores(methods = "system", logical = FALSE)
    ) |> as.integer()
}


# get_number_of_workers() returns the number of workers/cores that should be
# used in parallel
#
# inputs:
# - workers ... (integer scalar or "auto")
# - ram_needed ... (numeric scalar) RAM in GB needed per one core
#
# value:
#   integer scalar
#
# notes:
# - if workers == "auto", optimal number of workers is estimated from available
#   cores, RAM needed per core, and amount of memory available when
#   get_number_of_workers() is run
# - it is supposed that this system is the only software running at the machine
#
# WARNINGS:
# - if any other process allocates memory after get_number_of_workers() done its
#   magic, memory may be insufficient for this system
get_number_of_workers <- function(workers, ram_needed) {
    if (workers == "auto") {
        ram <- available_memory()
        no_of_cores <- available_cores()
        workers <- min(no_of_cores, floor(ram / ram_needed))}
    workers
}


# # silently(.f) wrappes function .f in such a way that runs and returns TRUE when
# # it succeeds and FALSE when it fails; it should be used with functions that
# # return no value but are run for their side effects; it is used in PWALK()
# #
# # inputs:
# # - .f ... (clusure) a function that returns nothing and is run for its side
# #   effects
# #
# # value:
# #   logical: FALSE when .f fails and TRUE otherwise
# silently <- function(.f) {
#     .f <- purrr::as_mapper(.f)
#     function(...) {
#         val <- TRUE
#         tryCatch(.f(...), error = function(e) {
#             val <<- FALSE
#         }, interrupt = function(e) {
#             stop("Terminated by user", call. = FALSE)
#         })
#         val
#     }
# }


# silently(.f) wrappes function .f in such a way that runs and returns TRUE when
# it succeeds and simpleError when it fails; it should be used with functions
# that return no value but are run for their side effects; it is used in PWALK()
#
# inputs:
# - .f ... (clusure) a function that returns nothing and is run for its side
#   effects
#
# value:
#   simpleError when .f fails and TRUE otherwise
silently <- function(.f) {
    .f <- purrr::as_mapper(.f)
    function(...) {
        tryCatch({.f(...); TRUE},
                 error = function(e) e)
    }
}


# # PWALK() is the same as purrr::walk() with these differences:
# # - if workers > 1, furrr::future_walk() is run
# # - function .f is protected, i.e., it never fails
# # - .l must be a tibble, i.e., it cannot be a list
# #
# # PWALK <- function(.l, .f, workers = 1, ...) {
# #     workers <- get_number_of_workers(workers)
# #     .f <- purrr::possibly(.f, otherwise = NULL)
# #     if (workers == 1) {
# #         purrr::pwalk(.l, .f, ...)
# #     } else {
# #         oplan <- future::plan()
# #         future::plan("multisession", workers = workers)
# #         furrr::future_pwalk(.l, .f, ...,
# #                             .options = furrr::furrr_options(seed = TRUE))
# #         future::plan(oplan)
# #     }
# # }
# #
# # TODO: když to spadne, mělo by to throw error via stop()
# PWALK <- function(.l, .f, workers = 1, ram_needed = NULL, ...) {
#     if (nrow(.l) > 0) {
#         workers <- get_number_of_workers(workers, ram_needed)
#         .f <- silently(.f)
#         if (workers == 1) {
#             success <- purrr::pmap_lgl(.l, .f, ...)
#         } else {
#             oplan <- future::plan()
#             future::plan("multisession", workers = workers)
#             success <- furrr::future_pmap_lgl(.l, .f, ...,
#                                               .options = furrr::furrr_options(seed = TRUE))
#             future::plan(oplan)
#         }
#         tab <- .l[!success, ]
#         if (nrow(tab) > 0) {
#             logging::logerror("production failed in the following output files: %s",
#                               str_c(tab$output_file, collapse = ", "))
#         }
#     }
# }

# PWALK() is the same as purrr::walk() with these differences:
# - if workers > 1, furrr::future_walk() is run
# - function .f is protected, i.e., it doesn't fail, and the all paralleled
#   computations are carried out; PWALK() fails only then
# - .l must be a tibble, i.e., it cannot be a list
# - when run in parallel, it uses "dynamic" scheduling to balance the load;
#   this assumes that the data .l sent to individual processes are small while
#   the computation itself takes a lot of time; see
#   https://furrr.futureverse.org/articles/chunking.html
#
PWALK <- function(.l, .f, workers = 1, ram_needed = NULL, ...) {
    if (nrow(.l) > 0) {
        workers <- get_number_of_workers(workers, ram_needed)
        .f <- silently(.f)
        if (workers == 1) {
            success <- purrr::pmap(.l, .f, ...)
        } else {
            oplan <- future::plan()
            on.exit(future::plan(oplan))
            future::plan("multisession", workers = workers)
            success <- furrr::future_pmap(
                .l, .f, ...,
                .options = furrr::furrr_options(seed = TRUE,
                                                scheduling = Inf)
            )
        }
        failed <- seq_len(nrow(.l))[!purrr::map_lgl(success, isTRUE)]
        if (length(failed) > 0) {
            purrr::walk(failed,
                        ~logging::logerror("output file %s throws error %s",
                                           .l$output_file[.],
                                           as.character(success[[.]])))
            stop("production failed in the following output files: ",
                 str_c(.l$output_file[failed], collapse = ", "),
                 "; see the log")
        }
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
    tryCatch({
        dir.create(log_folder, showWarnings = FALSE, recursive = TRUE)
        time <- as.character(Sys.time())
        log_file <- file.path(
            log_folder,
            stringr::str_c(stringr::str_replace_all(time, "[\\s:]", "-"),".log")
        )
        logging::addHandler(writeToFile, file = log_file)
        logging::loginfo("log file created")
    },
    error = function(e) {
        stop("creating new log file failed---stopping evaluation---see the log",
             call. = NA)}
    )
}


# start_logging(log_folder) starts logging into the newest .log file in
# log_folder
#
# inputs:
# - log_folder (character scalar) path to a folder where logs are stored
#
# value:
#   none; it starts logging into the newest .log file in log_folder
#
# notes:
# - for reason why it is done this way, see notes to create_log_file()
start_logging <- function(log_folder) {
    # if (is.null(log_folder))
    #     return(invisible(NULL))
    logging::basicConfig()
    log_file <- list.files(log_folder, pattern = "\\.log", full.names = TRUE) |>
        file.info() |>
        dplyr::arrange(dplyr::desc(ctime)) |>
        dplyr::slice(1) |>
        rownames()
    logging::addHandler(writeToFile, file = log_file)
}



# automatic time windows -------------------------------------------------------

# auto_time_window(length, number) produces an automatic time window for one
# period length and one period number
#
# inputs:
# - length ... (integer scalar) length of a time window in years
# - number ... (integer scalar) how many windows of length length should be
#   created
#
# value:
#   tibble with two columns:
#   - from_date ... (character in format YYYY-MM-DD) beginning of the period
#   - to_date ... (character in format YYYY-MM-DD) end of the period
#   the first period end in December 31 of the last finished year; it starts on
#   January 1 of the year length years before that; i.e., the length of the
#   period is length
#
# usage:
#   auto_time_window(3, 2)  # two periods of 3 years
auto_time_window <- function(length, number) {
    last_complete_year <- lubridate::year(lubridate::today()) - 1
    f <- function(end_year, length, shift) {
        tibble::tibble(
            from_date = stringr::str_c(end_year - (shift + 1) * length + 1,
                                       "-01-01"),
            to_date = stringr::str_c(end_year - shift * length, "-12-31")
        )
    }
    purrr::map_dfr(seq_len(number) - 1,
                   ~f(last_complete_year, length, .)) #|>
    # dplyr::mutate(across(everything(), as.Date))
}


# auto_time_windows(lengths, numbers) produces an automatic time window for many
# period lengths and possibly many period numbers
#
# inputs:
# - lengths ... (integer vector) length of a time window in years
# - number ... (integer scalar or integer vector of the same length as lengths)
#   how many windows of length length should be created
#
# value:
#   tibble with two columns:
#   - from_date ... (character in format YYYY-MM-DD) beginning of the period
#   - to_date ... (character in format YYYY-MM-DD) end of the period
#   the first period end in December 31 of the last finished year; it starts on
#   January 1 of the year length years before that; i.e., the length of the
#   period is length
#
# usage:
#   auto_time_windows(3, 2)  # two periods of 3 years
#   auto_time_windows(c(1, 3), 2)  # two periods of 1 year and two of 3 years
#   auto_time_windows(c(1, 3), 1:2)  # one 1-year long period and two
#                                    # 3-years-long periods
auto_time_windows <- function(lengths, numbers) {
    lengths <- unlist(lengths)
    numbers <- unlist(numbers)
    if (length(numbers) == 1)
        numbers <- rlang::rep_along(lengths, numbers)
    purrr::pmap_dfr(list(lengths, numbers), auto_time_window)
}


# compact_time_window(profile) processes the automatic time windows and joins
# them with the manual time windows; this function does the inner job;
# see help for compact_all_time_windows()
#
# inputs:
# - profile ... (tibble) one-row tibble, a row subset from profiles
#
# output:
#   one-row tibble
compact_time_window <- function(profile) {
    empty_date <- character(0)
    empty_window <- tibble::tibble(from_date = empty_date, to_date = empty_date)
    auto_window <- manual_window <- empty_window
    if (profile$TIME_WINDOW_AUTO)
        auto_window <- auto_time_windows(profile$TIME_WINDOW_LENGHT,
                                         profile$TIME_WINDOW_NUMBER)
    if ("TIME_WINDOW" %in% names(profile))
        manual_window <- profile$TIME_WINDOW
    time_window <- dplyr::bind_rows(auto_window, manual_window) |>
        dplyr::distinct()
    if (nrow(time_window) == 0) {
        logging::logerror("config prep: profile %s has empty time windows",
                          profile$PROFILE_NAME)
        stop("config prep: profile has empty time windows")
    }
    profile$TIME_WINDOW <- list(time_window)
    profile |>
        dplyr::select(-c(TIME_WINDOW_AUTO, TIME_WINDOW_LENGHT,
                         TIME_WINDOW_NUMBER))
}


# compact_all_time_windows(profiles) processes the automatic time windows and
# joins them with the manual time windows
#
# inputs:
# - profiles ... (tibble) profiles tibble created by prepare_profiles script
#
# value:
#   the same profiles tibble with few differences:
#   - automatic time slots are removed
#   - TIME_WINDOW slot is appended from the automatic time windows
#
# notes:
# - automatic time windows are ... automatic; this means the each time this
#   function is run, it may give different values as the automatic time windows
#   depend on the year when it is run; example: if it is run on December 31, it
#   gives older dates then when it's run on January 1 of the next year
compact_all_time_windows <- function(profiles) {
    profiles |>
        dplyr::rowwise() |>
        dplyr::group_split() |>
        purrr::map(compact_time_window) |>
        dplyr::bind_rows() |>
        dplyr::distinct()
}



# reading various files --------------------------------------------------------

# read_profiles() returns list of profiles
#
# inputs:
#   none
#
# output:
#   profiles (tibble) created by prepare_profiles script
#
# notes:
# - it uses automatic time windows, i.e., it may give different values based on
#   when it is used; see compact_all_time_windows()
read_profiles <- function() {
    readr::read_rds(path_to_configs()) |>
        compact_all_time_windows()
}


# read_districts returns districts table
read_districts <- function() {
    readr::read_rds(path_to_districts())
}



# damage cost ------------------------------------------------------------------

# add_damage_cost(accidents) takes a tibble of accidents and adds a new column,
# accident_cost
#
# inputs:
# - accidents ... (tibble) accidents table
# - accident_dead ... (integer) number of the dead in the accident
# - accident_serious_injury ... (integer) number of the seriously injured
# - accident_light_injury ... (integer) number of the light injured
# - accident_material_cost ... (double) material cost in mil. CZK
# - na_zero ... (logical scalar) if TRUE (default), NAs in costs are replaced
#   with 0s; if FALSE, all rows in accidents that have any NA cost are removed
#
# value:
#   the same tibble as accidents but new column, accident cost, is added
add_damage_cost <- function(accidents,
                            unit_cost_dead,
                            unit_cost_serious_injury,
                            unit_cost_light_injury,
                            unit_cost_material,
                            unit_cost_const, na_zero = TRUE) {
    zero_na <- function(x, na_zero)
        ifelse(is.na(x) & na_zero, 0, x)

    accidents <- accidents |>
        mutate(accident_cost =
                   zero_na(accident_dead, na_zero) * unit_cost_dead +
                   zero_na(accident_serious_injury, na_zero) *
                   unit_cost_serious_injury +
                   zero_na(accident_light_injury, na_zero) *
                   unit_cost_light_injury +
                   zero_na(accident_material_cost, na_zero) *
                   unit_cost_material +
                   unit_cost_const
        )

    if (!na_zero)
        accidents <- accidents |> filter(!is.na(accident_cost))
    accidents
}
