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
# cat("RSCRIPTDIR: ", RSCRIPTDIR, "\n\n")
#
# cat(commandArgs(), sep = "\n")
#
# process_command_line_arguments <- function() {
#     get_parameter <- function(params, key) {
#         stringr::str_subset(params, stringr::str_c("--", key, "=.*")) |>
#             stringr::str_remove(stringr::str_c("--", key, "="))
#     }
#
#     cl_pars <- commandArgs()
#     profile <- get_parameter(cl_pars, "profile")
#     profile
# }
#
# cat("profile: ", process_command_line_arguments(), "\n")

cat(commandArgs(), sep = "\n")

source("guts/functions_auxiliary.R")

NO_OF_WORKERS <- 7

process_command_line_arguments("guts")

cat("NO_OF_WORKERS:", NO_OF_WORKERS, "\n")

cat("LIXEL_SIZE:", LIXEL_SIZE, "\n")
