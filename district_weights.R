# -------------------------------------
# Script:   district_sizes.R
# Author:   Michal Kvasnička
# Purpose:  this script defines a function that that returns districts weights
#           used to balance load in parallel computation
# Inputs:   none
# Outputs:  it defines the only function: district_sizes()
# Notes:    sorry for this nasty solution
#
# Copyright(c) Michal Kvasnička
# -------------------------------------


# district_sizes(ids) must return for each district id in vector ids a numeric
# weight that is used to balance load in parallel processing
#
# inputs:
# - ids ... (character vector) vector of vector ids
#
# value: numeric vector of the weights
#
# notes:
# - this is a nasty solution that would work for the Czech Republic only; for
#   any other country it would return an agnostic weights (equal for each
#   district)
# - if you want to use it for any other country, you may use it "as such",
#   return the same constant for each each district (agnostic weights), or
#   provide your own weights
# - if you want to provide your own weights, run the system once; then use
#   the number of lixels as the weights
# - agnostic weights mean that the districts would be processed in a random
#   order; this would need more memory per core
district_sizes <- function(ids) {
    sizes <- tibble::tribble(
        ~ids, ~sizes,
        "CZ0100", 1444912,
        "CZ0201", 805789,
        "CZ0202", 471841,
        "CZ0203", 540093,
        "CZ0204", 562161,
        "CZ0205", 536458,
        "CZ0206", 476404,
        "CZ0207", 600537,
        "CZ0208", 498123,
        "CZ0209", 974168,
        "CZ020A", 921490,
        "CZ020B", 756925,
        "CZ020C", 368878,
        "CZ0311", 806657,
        "CZ0312", 420986,
        "CZ0313", 654258,
        "CZ0314", 528774,
        "CZ0315", 439864,
        "CZ0316", 526654,
        "CZ0317", 608844,
        "CZ0321", 383552,
        "CZ0322", 638302,
        "CZ0323", 421031,
        "CZ0324", 540355,
        "CZ0325", 631431,
        "CZ0326", 334316,
        "CZ0327", 421192,
        "CZ0411", 434866,
        "CZ0412", 524581,
        "CZ0413", 349759,
        "CZ0421", 437334,
        "CZ0422", 416090,
        "CZ0423", 506856,
        "CZ0424", 496031,
        "CZ0425", 288739,
        "CZ0426", 353604,
        "CZ0427", 328424,
        "CZ0511", 452050,
        "CZ0512", 393509,
        "CZ0513", 612944,
        "CZ0514", 476756,
        "CZ0521", 567974,
        "CZ0522", 475558,
        "CZ0523", 460295,
        "CZ0524", 483801,
        "CZ0525", 468093,
        "CZ0531", 593093,
        "CZ0532", 655751,
        "CZ0533", 588103,
        "CZ0534", 676376,
        "CZ0631", 597199,
        "CZ0632", 603208,
        "CZ0633", 569933,
        "CZ0634", 603150,
        "CZ0635", 662235,
        "CZ0641", 460052,
        "CZ0642", 520758,
        "CZ0643", 1025467,
        "CZ0644", 443445,
        "CZ0645", 475735,
        "CZ0646", 381733,
        "CZ0647", 490070,
        "CZ0711", 183324,
        "CZ0712", 773914,
        "CZ0713", 450827,
        "CZ0714", 554354,
        "CZ0715", 474238,
        "CZ0721", 430724,
        "CZ0722", 457489,
        "CZ0723", 485920,
        "CZ0724", 562262,
        "CZ0801", 422010,
        "CZ0802", 898470,
        "CZ0803", 551365,
        "CZ0804", 617588,
        "CZ0805", 624314,
        "CZ0806", 691712
    )

    tibble::tibble(ids = ids) |>
        dplyr::left_join(sizes, by = "ids") |>
        dplyr::mutate(sizes = ifelse(is.na(sizes), mean(sizes$sizes), sizes)) |>
        dplyr::pull(sizes)
}
