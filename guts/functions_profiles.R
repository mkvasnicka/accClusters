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
             " are missing in profile ", file_name, ".")
    # check no unsupported name is present
    if (!(length(setdiff(profile_names, supported_names)) == 0))
        stop("Profile ", file_name, " includes unsupported slots: ",
             stringr::str_flatten(setdiff(profile_names, supported_names),
                                  collapse = ", "))

    # check that present slots have correct type and length
    f <- function(key) {
        (class(all_slots[[key]]) == class(profile[[key]])) &&
            (length(all_slots[[key]]) == length(profile[[key]]))
    }
    correct_slots <- purrr::map_lgl(profile_names, f)
    if (!all(correct_slots))
        stop("Slots ",
             stringr::str_flatten(profile_names[!correct_slots],
                                  collapse = ", "),
             " have either incorrect type or lenght.")
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
}


read_profile <- function(config, profile) {
    # definition of all supported slots
    necessary_slots <- list(PROFILE_NAME = character(1))
    supported_slots <- list(NKDE_WEIGHTS = character(1),
                            NKDE_BW = double(1),
                            NKDE_ADAPTIVE = logical(1),
                            NKDE_TRIM_BW = double(1),
                            NKDE_METHOD = character(1),
                            NKDE_AGG = double(1))

    e <- read_config_or_profile(profile)
    check_profile(e, profile, necessary_slots, supported_slots)
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
                                      collapse = ", "))
        names(profiles) <- profile_names
    } else {
        config$PROFILE_NAME <- "default"
        profiles <- list(default = config)
    }
    profiles
}
