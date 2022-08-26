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


# check_profile() checks that all necessary variables are present in a profile,
# no unsupported variable is present in the profile, and all variables have
# right type and length
#
# inputs:
# - profile ... (environment)
#
# value: none
check_profile <- function(profile, file_name) {
    # check that profile is environment
    stopifnot(is.environment(profile))

    # definition of all supported slots
    necessary_slots <- list(PROFILE_NAME = character(1))
    supported_slots <- list(NKDE_WEIGHTS = character(1),
                            NKDE_BW = double(1),
                            NKDE_ADAPTIVE = logical(1),
                            NKDE_TRIM_BW = double(1),
                            NKDE_METHOD = character(1),
                            NKDE_AGG = double(1))
    all_slots <- c(necessary_slots, supported_slots)

    necessary_names <- names(necessary_slots)
    supported_names <- c(names(supported_slots), supported_names)
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


# read_profile() reads one R script that should include a profile; it evaluates
# it in a new environment; then it checks it (see help for check_profile())
#
# inputs:
# - profile ... (character scalar) path to profile R script
#
# value: environment; it is a profile
read_profile <- function(profile) {
    e <- new.env()
    source(profile, local = e)
    check_profile(e, file_name = profile)
    e
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
    profiles <- list.files(folder, pattern = "\\.R", full.names = TRUE)
    purrr::map(profiles, read_profile)
}
