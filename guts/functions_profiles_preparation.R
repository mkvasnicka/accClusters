# -------------------------------------
# Script:   functions_profiles.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:   none
# Outputs:  functions
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

library(purrr)
library(stringr)
library(rlang)


# expected variables -----------------------------------------------------------

# config_necessary_slots() returns list of all variables that must be present in
# a config file and their types; no other variables may be present
#
# inputs:
#   none
#
# value:
#   named list; names are name of variables, define classes and lenghts of the
#   variables (except zero lenght mean any lenght)
config_necessary_slots <- function() {
    list(# paths
        RAW_DATA_DIR = character(1),
        DATA_DIR = character(1),
        OUTPUT_DIR = character(1),
        LOG_DIR = character(1),
        # parallel processing
        NO_OF_WORKERS = "auto",
        NO_OF_WORKERS_ACCIDENTS = "auto",
        RAM_PER_CORE_GENERAL = double(1),
        RAM_PER_CORE_ACCIDENTS = double(1),
        # map preparation
        DISTRICT_BUFFER_SIZE = double(1),
        LIXEL_SIZE = double(1),
        LIXEL_MIN_DIST = double(1),
        ACCIDENT_TO_ROAD_MAX_DISTANCE = double(1),
        SUPPORTED_ROAD_CLASSES = character(0),
        # accident file masks
        ACCIDENTS_FILE_NAME_PATTERN = character(1),
        ACCIDENTS_GPS_FILE_NAME_PATTERN = character(1),
        # accident costs
        UNIT_COSTS_DEAD = double(1),
        UNIT_COSTS_SERIOUS_INJURY = double(1),
        UNIT_COSTS_LIGHT_INJURY = double(1),
        UNIT_COSTS_MATERIAL = double(1),
        UNIT_COST_CONST = double(1),
        # nkde
        NKDE_WEIGHTS = character(1),
        NKDE_BW = double(1),
        NKDE_ADAPTIVE = logical(1),
        NKDE_TRIM_BW = double(1),
        NKDE_METHOD = character(1),
        NKDE_AGG = double(1),
        # cluster creation
        CLUSTER_MIN_QUANTILE = double(1),
        CLUSTER_ADDITIONAL_STEPS = double(1),
        VISUAL_MIN_QUANTILE = double(1),
        # time windows
        TIME_WINDOW = tibble::tibble(
            from_date = as.Date(integer(0)),
            to_date = as.Date(integer(0)))
    )
}


# profile_necessary_slots() returns names of all variables that must be in a
# profile file, and their types
# see help for config_necessary_slots()
profile_necessary_slots <- function() {
    list(PROFILE_NAME = character(1))
}


# profile_supported_slots() returns names of all variables that may be in a
# profile file, and their types, other than those returned by
# profile_necessary_slots()
# see help for config_necessary_slots()
profile_supported_slots <- function() {
    config_necessary_slots()[c("NKDE_WEIGHTS", "NKDE_BW", "NKDE_ADAPTIVE",
                               "NKDE_TRIM_BW", "NKDE_METHOD", "NKDE_AGG",
                               "UNIT_COSTS_DEAD", "UNIT_COSTS_SERIOUS_INJURY",
                               "UNIT_COSTS_LIGHT_INJURY", "UNIT_COSTS_MATERIAL",
                               "UNIT_COST_CONST",
                               "CLUSTER_MIN_QUANTILE",
                               "CLUSTER_ADDITIONAL_STEPS",
                               "VISUAL_MIN_QUANTILE",
                               "TIME_WINDOW")]
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

    # check that present slots have correct type and length
    f <- function(key) {
        all(class(all_slots[[key]]) == class(profile[[key]])) &&
            (length(all_slots[[key]]) == 0 ||
                 all(length(all_slots[[key]]) == length(profile[[key]])))
    }
    correct_slots <- purrr::map_lgl(profile_names, f)
    if (!all(correct_slots))
        stop("Variable(s) ",
             stringr::str_flatten(profile_names[!correct_slots],
                                  collapse = ", "),
             " have either incorrect type or lenght.",
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
    check_profile(e, config, config_necessary_slots(), list())
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
    profiles <- list.files(folder, pattern = "profile.*\\.R", full.names = TRUE)
    profiles <- purrr::map(profiles, ~read_profile(config, .))
    if (length(profiles) > 0) {
        profile_names <- profiles |> purrr::map_chr("PROFILE_NAME")
        if (any(duplicated(profile_names)))
            stop("There are duplicities in profile names: ",
                 stringr::str_flatten(profile_names[duplicated(profile_names)],
                                      collapse = ", "),
                 call. = NA)
        names(profiles) <- profile_names
    } else {
        config$PROFILE_NAME <- "default"
        profiles <- list(default = config)
    }
    profiles
}
