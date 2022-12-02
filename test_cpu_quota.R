#!/usr/local/bin/Rscript
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

source("functions_auxiliary.R")

#NO_OF_WORKERS <- 7

#process_command_line_arguments("guts")

#cat("NO_OF_WORKERS:", NO_OF_WORKERS, "\n")

#cat("LIXEL_SIZE:", LIXEL_SIZE, "\n")


workers <- get_number_of_workers("auto")
cat("workers:",workers,"\n")

ram <- available_memory()
cat("ram:",ram,"\n")

cat("Sys.meminfo",as.numeric(memuse::Sys.meminfo()$freeram) / 1024 ^ 3,"\n")

#no_of_cores <- available_cores()

cat("future cores:",future::availableCores(),"\n")



