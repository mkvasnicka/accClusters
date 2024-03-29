# -------------------------------------
# Script:   functions_profiles_preparation.R
# Author:   Michal Kvasnička
# Purpose:  This scripts defines various functions used for config/profile
#           reading, checking, and storing.
# Inputs:   none
# Outputs:  functions
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

library(tibble, verbose = FALSE, warn.conflicts = FALSE)
library(purrr, verbose = FALSE, warn.conflicts = FALSE)
library(stringr, verbose = FALSE, warn.conflicts = FALSE)
library(readr, verbose = FALSE, warn.conflicts = FALSE)
library(rlang, verbose = FALSE, warn.conflicts = FALSE)


# constants
DEFAULT_PROFILE_COMMENT <- "základní nastavení"



# variable checks --------------------------------------------------------------

# These functions are used to define/check that variables supplied by the user
# in config.R/profiles have (in principle) valid values.

# the vector has lenght equal to one
is_scalar <- function(x) {
    is.atomic(x) && length(x) == 1
}

is_not_empty <- function(x) {
    is.atomic(x) && length(x) >= 1
}

# all slots in vector are known
is_known <- function(x) {
    !all(is.na(x))
}

# all slots in vector are known and are numeric
is_known_numeric <- function(x) {
    is.numeric(x) && is_known(x)
}

# all slots in vector x are known round numbers
is_known_round_numeric <- function(x) {
    is_known_numeric(x) && all(round(x) == x)
}

# all slots in vector x are known characters
is_known_character <- function(x) {
    is.character(x) && is_known(x)
}


# is known numeric scalar
check_slot_number <- function(x) {
    is_known_numeric(x) && is_scalar(x)
}

# is known character scalar
check_slot_word <- function(x) {
    is_known_character(x) && is_scalar(x)
}

# is valid path
check_slot_path <- function(x) {
    check_slot_word(x)
}

# is not-empty known character vector
check_slot_character_vector <- function(x) {
    is_known_character(x) && is_not_empty(x)
}

# is known positive integer scalar
check_slot_positive_integer <- function(x) {
    is_known_round_numeric(x) && is_scalar(x) && x > 0
}

# is known postive numeric scalar
check_slot_positive_number <- function(x) {
    check_slot_number(x) && x > 0
}

# is known non-negative numeric scalar
check_slot_nonnegative_number <- function(x) {
    check_slot_number(x) && x >= 0
}

check_slot_quantile_vector <- function(x) {
    is_known_numeric(x) && is_not_empty(x) && all(x >= 0) && all(x <= 1)
}

check_slot_positive_integer_vector <- function(x) {
    is_known_round_numeric(x) && is_not_empty(x) && all(x > 0)
}

# is known numeric scalar in interval [0, 1]
check_slot_quantile <- function(x) {
    check_slot_number(x) && x >= 0 && x <= 1
}

# is known logical scalar
check_slot_true_false <- function(x) {
    is.logical(x) && is_scalar(x) && is_known(x)
}

# number of workers: either "autor" or known positive integer
check_slot_worker <- function(x) {
    (check_slot_word(x) && x == "auto") || check_slot_positive_integer(x)
}

# is valid NKDE method: "continuous" or "discontinuous"
check_slot_method <- function(x) {
    check_slot_word(x) && x %in% c("continuous", "discontinuous")
}

# is valid NKDE weight constant: "cost" or "equal"
check_slot_weight <- function(x) {
    check_slot_word(x) && x %in% c("cost", "equal")
}

# is valid profile name
check_slot_profile_name <- function(x) {
    check_slot_word(x) &&
        stringr::str_length(x) > 0 &&
        stringr::str_length(str_remove_all(x, "[A-Za-z0-9]")) == 0 &&
        stringr::str_detect(x, "^[A-Za-z]")
}

# is valid time window tibble
check_slot_time_window <- function(x) {
    # test formal structure
    p1 <- tibble::is_tibble(x) &&
        ncol(x) == 2 && nrow(x) >= 1 &&
        all(c("from_date", "to_date") %in% names(x)) &&
        is.character(x$from_date) && is.character(x$to_date)
    if (!p1)
        return(FALSE)
    # test that contents are real dates
    p2 <- try(purrr::map_int(x, ~any(is.na(as.Date(.)))) |> sum(),
              silent = TRUE)
    if (!identical(p2, 0L))
        return(FALSE)
    # test that from_date <= to_date
    if (!all(x$from_date <= x$to_date))
        return(FALSE)
    # test for duplicities
    nrow(x) == nrow(distinct(x))
}



# expected variables -----------------------------------------------------------

# config_necessary_slots() returns list of all variables that must be present in
# a config file and their types; only variables defined by
# config_necessary_slots() and config_supported_slots() may be present
#
# inputs:
#   none
#
# value:
#   named list; names are name of variables, values are functions used to check
#   its value validity
config_necessary_slots <- function() {
    list(# paths -- fixed in code
        # RAW_DATA_DIR = check_slot_path,
        # DATA_DIR = check_slot_path,
        # OUTPUT_DIR = check_slot_path,
        # LOG_DIR = check_slot_path,
        # parallel processing
        NO_OF_WORKERS = check_slot_worker,
        NO_OF_WORKERS_ACCIDENTS = check_slot_worker,
        RAM_PER_CORE_GENERAL = check_slot_positive_number,
        RAM_PER_CORE_ACCIDENTS = check_slot_positive_number,
        OSMIUM_DISTRICTS_IN_ONE_GO = check_slot_positive_integer,
        # map preparation
        DISTRICT_BUFFER_SIZE = check_slot_positive_number,
        LIXEL_SIZE = check_slot_positive_integer,
        LIXEL_MIN_DIST = check_slot_positive_integer,
        ACCIDENT_TO_ROAD_MAX_DISTANCE = check_slot_positive_number,
        SUPPORTED_ROAD_CLASSES = check_slot_character_vector,
        # accident file masks -- fixed in code
        # ACCIDENTS_FILE_NAME_PATTERN = check_slot_word,
        # ACCIDENTS_GPS_FILE_NAME_PATTERN = check_slot_word,
        # ACCIDENTS_OUTCOMES_FILE_NAME_PATTERN = check_slot_word,
        # ACCIDENTS_PEDESTRIANS_FILE_NAME_PATTERN = check_slot_word,
        # ACCIDENTS_VEHICLES_FILE_NAME_PATTERN = check_slot_word,
        # ACCIDENTS_FILES_SKIP = check_slot_positive_integer,
        # accident costs
        UNIT_COST_DEAD = check_slot_nonnegative_number,
        UNIT_COST_SERIOUS_INJURY = check_slot_nonnegative_number,
        UNIT_COST_LIGHT_INJURY = check_slot_nonnegative_number,
        UNIT_COST_MATERIAL = check_slot_nonnegative_number,
        UNIT_COST_CONST = check_slot_nonnegative_number,
        CONST_COST_DEAD = check_slot_nonnegative_number,
        CONST_COST_SERIOUS_INJURY = check_slot_nonnegative_number,
        CONST_COST_LIGHT_INJURY = check_slot_nonnegative_number,
        CONST_COST_MATERIAL = check_slot_nonnegative_number,
        # nkde
        NKDE_WEIGHTS = check_slot_weight,
        NKDE_BW = check_slot_positive_number,
        NKDE_ADAPTIVE = check_slot_true_false,
        NKDE_TRIM_BW = check_slot_positive_number,
        NKDE_METHOD = check_slot_method,
        NKDE_AGG = check_slot_nonnegative_number,
        # cluster creation
        # CLUSTER_MIN_QUANTILE = check_slot_quantile,
        # CLUSTER_ADDITIONAL_STEPS = check_slot_positive_integer,
        # VISUAL_MIN_QUANTILE = check_slot_quantile,
        # CLUSTER_SEVERITY_LIMIT = check_slot_positive_integer,
        # CLUSTER_SEVERITY_STEP = check_slot_positive_integer,
        # CLUSTER_STEP_LIMIT = check_slot_positive_integer,
        # time windows
        TIME_WINDOW_AUTO = check_slot_true_false,
        TIME_WINDOW_LENGTH = check_slot_positive_integer_vector,
        TIME_WINDOW_NUMBER = check_slot_positive_integer_vector
    )
}


# config_supported_slots() returns list of all variables that may be present in
# a config file and their types; only variables defined by
# config_necessary_slots() and config_supported_slots() may be present
#
# inputs:
#   none
#
# value:
#   named list; names are name of variables, values are functions used to check
#   its value validity
config_supported_slots <- function() {
    list(
        # districts
        DISTRICTS = check_slot_character_vector,
        # time window
        TIME_WINDOW = check_slot_time_window
    )
}


# profile_necessary_slots() returns list of all variables that must be present
# in a profile file and their types; only variables defined by
# profile_necessary_slots() and profile_supported_slots() may be present
#
# see help for config_necessary_slots()
profile_necessary_slots <- function() {
    list(PROFILE_NAME = check_slot_profile_name,
         PROFILE_COMMENT = check_slot_word)
}


# profile_supported_slots() returns list of all variables that may be present in
# a profile file and their types; only variables defined by
# profile_necessary_slots() and profile_supported_slots() may be present
#
# see help for config_necessary_slots()
profile_supported_slots <- function() {
    c(
        config_necessary_slots()[c("NKDE_WEIGHTS", "NKDE_BW", "NKDE_ADAPTIVE",
                               "NKDE_TRIM_BW", "NKDE_METHOD", "NKDE_AGG",
                               "UNIT_COST_DEAD", "UNIT_COST_SERIOUS_INJURY",
                               "UNIT_COST_LIGHT_INJURY", "UNIT_COST_MATERIAL",
                               "UNIT_COST_CONST", "CONST_COST_DEAD",
                               "CONST_COST_SERIOUS_INJURY",
                               "CONST_COST_LIGHT_INJURY", "CONST_COST_MATERIAL",
                               # "CLUSTER_SEVERITY_LIMIT",
                               # "CLUSTER_SEVERITY_STEP",
                               # "CLUSTER_STEP_LIMIT",
                               "TIME_WINDOW_AUTO",
                               "TIME_WINDOW_LENGTH",
                               "TIME_WINDOW_NUMBER")],
        config_necessary_slots()[c("TIME_WINDOW")]
    )
}



# adding parameters user shouldn't change --------------------------------------

# add_cluster_parameters() adds to profiles parameters needed
# in functions_cluster_preparation.R that the user should not change because the
# shiny would fail in such a case
#
# the values here are expected by the shiny app
# they are added only when the corresponding columns do not exist
add_cluster_parameters <- function(profiles) {
    if (!("CLUSTER_SEVERITY_LIMIT" %in% names(profiles)))
        profiles$CLUSTER_SEVERITY_LIMIT <- 25
    if (!("CLUSTER_SEVERITY_STEP" %in% names(profiles)))
        profiles$CLUSTER_SEVERITY_STEP <- 2
    if (!("CLUSTER_STEP_LIMIT" %in% names(profiles)))
        profiles$CLUSTER_STEP_LIMIT <- 10
    profiles
}



# reading profiles -------------------------------------------------------------

# check_profile() checks that all necessary variables are present in a profile,
# no unsupported variable is present in the profile, and all variables have
# right type and length
#
# inputs:
# - profile ... (environment)
# - file_name ... (character scalar) path to the profile (just for messaging)
# - necessary_slots, supported_slots ... (named lists) the first one names the
#   variables that must be present in the profile, the second one names
#   the variables that are allowed; names in the lists are
#   the necessary/supported variable names, the their values define class and
#   length expected
#
# value: none
check_profile <- function(profile, file_name, necessary_slots, supported_slots) {
    # check that profile is environment
    stopifnot(is.environment(profile))

    all_slots <- c(necessary_slots, supported_slots)

    necessary_names <- names(necessary_slots)
    supported_names <- c(names(supported_slots), necessary_names)
    profile_names <- names(profile)

    # check all necessary names are present
    if (!all(necessary_names %in% profile_names))
        stop("Necessary slots ",
             stringr::str_flatten(setdiff(necessary_names, profile_names),
                                  collapse = ", "),
             " are missing in profile ", file_name, ".",
             call. = NA)
    # check no unsupported name is present
    if (!(length(setdiff(profile_names, supported_names)) == 0))
        stop("Profile ", file_name, " includes unsupported slots: ",
             stringr::str_flatten(setdiff(profile_names, supported_names),
                                  collapse = ", "),
             call. = NA)

    correct_slots <- purrr::map_lgl(profile_names,
                                    ~all_slots[[.]](profile[[.]]))
    if (!all(correct_slots))
        stop("Variable(s) ",
             stringr::str_flatten(profile_names[!correct_slots],
                                  collapse = ", "),
             " have incorrect type, value, or length.",
             call. = NA)
}


# read_config_or_profile() reads one R script that should include a profile; it
# evaluates it in a new environment
#
# inputs:
# - profile ... (character scalar) path to profile R script
# - e ... (environment) a profile on which this one is based-upon
#
# value: environment; it is a profile
read_config_or_profile <- function(profile, based_on = new.env()) {
    stopifnot(is.environment(based_on))
    e <- rlang::env_clone(based_on)
    source(profile, local = e)
    e
}


read_config <- function(config) {
    e <- read_config_or_profile(config)
    check_profile(e, config, config_necessary_slots(), config_supported_slots())
    e
}


read_profile <- function(config, profile) {
    e <- read_config_or_profile(profile)
    check_profile(e, profile,
                  profile_necessary_slots(), profile_supported_slots())
    read_config_or_profile(profile, config)
}


# read_all_profiles() reads all R script in a folder and evaluates them in new
# environments and check them (see help for check_profile()); it returns a list
# of such environments (profiles)
#
# inputs:
# - folder ... (character scalar) path to profile folders
#
# value: list of environments; each environment is one profile
read_all_profiles <- function(folder) {
    config <- read_config(file.path(folder, "config.R"))
    profiles <- list.files(folder, pattern = "profile_.*\\.R",
                           full.names = TRUE)
    profiles <- purrr::map(profiles, ~read_profile(config, .))
    if (length(profiles) > 0) {
        profile_names <- profiles |> purrr::map_chr("PROFILE_NAME")
        if (any(duplicated(profile_names)))
            stop("There are duplicities in profile names: ",
                 stringr::str_flatten(profile_names[duplicated(profile_names)],
                                      collapse = ", "),
                 call. = NA)
        names(profiles) <- profile_names
        profile_comments <- purrr::map_chr(profiles, "PROFILE_COMMENT")
        if (any(duplicated(profile_comments)))
            stop("There are duplicities in profile comments: ",
                 stringr::str_flatten(profile_comments[duplicated(profile_comments)],
                                      collapse = ", "),
                 call. = NA)
    } else {
        config$PROFILE_NAME <- "default"
        config$PROFILE_COMMENT <- DEFAULT_PROFILE_COMMENT
        profiles <- list(default = config)
    }
    profiles
}


# profiles_to_tibble() converts list of profiles into a tibble
profiles_to_tibble <- function(p) {
    list_vars <- c("DISTRICTS", "SUPPORTED_ROAD_CLASSES",
                   "TIME_WINDOW_LENGTH", "TIME_WINDOW_NUMBER",
                   "TIME_WINDOW")
    p <- p |>
        purrr::map(as.list) |>
        purrr::transpose()
    list_idx <- which(names(p) %in% list_vars)
    p <- purrr::map_at(p, -list_idx, unlist) |>
        tibble::as_tibble() |>
        dplyr::select(PROFILE_NAME, everything()) |>
        dplyr::mutate(across(everything(), ~setNames(., NULL)))
    p
}


# check_auto_time_windows(profiles)
check_auto_time_windows <- function(profiles) {
    lens <- map_int(profiles$TIME_WINDOW_LENGTH, length)
    nums <- map_int(profiles$TIME_WINDOW_NUMBER, length)
    ids <- !(nums == lens | nums == 1)
    if (any(ids)) {
        stop("config prep: TIME_WINDOW_NUMBER must either be a scalar or must ",
             "have the same length as TIME_WINDOW_LENGTH: it's violated in ",
             "profiles with names ",
             str_c(profiles$PROFILE_NAME[ids], collapse = ", "))
    }
}


# create_profiles() reads config and all profiles from path_to_source_configs
# folder and writes it to path_to_configs RDS file
#
# inputs:
# - path_to_configs ... (character scalar) path to a file where the profiles
#   will be written
# - path_to_source_configs ... (character scalar) path to a folder where the
#   config and profiles (if any) are stored
#
# value:
#   none; it writes the profiles to disk
#
# notes:
# - there must be just one config.R file; it must include all necessary
#   variables, and they must of a given type
# - there may be one or more profiles; they must include PROFILE_NAME variable
#   and any number of other variables supported in profiles
# - any variables missing in a profile are taken from config.R
# - if there is no profile, default profile is created from config
# - all variables that may not be present in profiles have the same values in
#   all profiles
#
# warnings:
# - checking for updates is very rudimentary (it only checks modification dates
#   of existing files); hence, when you change the configuration, you should
#   remove the config/profiles.rds file
create_profiles <- function(path_to_configs = path_to_configs(),
                            path_to_source_configs = path_to_source_configs()) {
    start_logging(log_dir())
    tryCatch({
        logging::loginfo("config prep: checking for changes in configuration")
        # check for config
        if (!file.exists(file.path(path_to_source_configs(), "config.R"))) {
            logging::logerror("config prep: config.R is missing in %s",
                              path_to_source_configs())
            stop()
        }
        source_files <- list.files(path_to_source_configs(),
                                   pattern = "config|profile_.+.R",
                                   full.names = TRUE)
        if (is_behind(path_to_configs(), source_files)) {
            logging::loginfo("config prep: configuration is behind---updating")
            profiles <- read_all_profiles(path_to_source_configs) |>
                profiles_to_tibble() |>
                add_cluster_parameters()
            check_auto_time_windows(profiles)
            write_dir_rds(profiles, path_to_configs, compress = TRUE)
            logging::loginfo("config prep: profiles created")
        } else {
            logging::loginfo("config prep: configuration is uptodate---skipping")
        }
    },
    error = function(e) {
        logging::logerror("config prep failed: %s", e)
        stop("config prep---stopping evaluation---see the log", call. = NA)}
    )
}
