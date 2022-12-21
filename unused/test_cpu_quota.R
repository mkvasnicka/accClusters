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
library('parallel')

#process_command_line_arguments("guts")

cat("future availableCores methods=empty:",future::availableCores(),"\n")
cat("future availableCores methods=system:",future::availableCores(methods="system"),"\n")
cat("future availableCores methods=mc.cores:",future::availableCores(methods="mc.cores"),"\n")
cat("future availableCores methods=_R_CHECK_LIMIT_CORES_:",future::availableCores(methods="_R_CHECK_LIMIT_CORES_"),"\n")
cat("future availableCores methods=PBS:",future::availableCores(methods="PBS"),"\n")
cat("future availableCores methods=SGE:",future::availableCores(methods="SGE"),"\n")
cat("future availableCores methods=Slurm:",future::availableCores(methods="Slurm"),"\n")
cat("future availableCores methods=fallback:",future::availableCores(methods="fallback"),"\n")

cat("detectCores logical=FALSE:",parallel::detectCores(all.tests = TRUE,logical = FALSE),"\n")
cat("detectCores logical=TRUE:",parallel::detectCores(all.tests = TRUE,logical = TRUE),"\n")

cat("docker_cpu_limit:",docker_cpu_limit(),"\n")
cat("available_cores:",available_cores(),"\n")

cat("available_memory:",available_memory(),"\n")

cat("get_number_of_workers:",get_number_of_workers("auto",4.5),"\n")

