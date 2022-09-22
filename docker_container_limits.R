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
    cpu_limit <- function(file) {
        cl <- NA
        if (file.exists(file)) {
            cl <- base::readLines(file) |>
                base::strsplit("\\s+")
            cl <- cl[[1]] |>
                as.numeric() / 1e5
            if (cl[2] != 1)
                stop("I can't work with fractional cpu-periods.\n",
                     "    Run docker with --cpu-period=100000!")
        }
        cl[1]
    }

    suppressWarnings(
        c(
            cpu_limit("/sys/fs/cgroup/cpu/cpu.cfs_quota_us"),
            cpu_limit("/sys/fs/cgroup/cpu.max")
        ) |>
            min(na.rm = TRUE) |>
            floor()
    )
}
