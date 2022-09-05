# -------------------------------------
# Script:   functions_damage_cost.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:   none
# Outputs:  functions
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------


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
                   zero_na(accident_serious_injury, na_zero) * unit_cost_serious_injury +
                   zero_na(accident_light_injury, na_zero) * unit_cost_light_injury +
                   zero_na(accident_material_cost, na_zero) * unit_cost_material +
                   unit_cost_const
        )

    if (!na_zero)
        accidents <- accidents |> filter(!is.na(accident_cost))
    accidents
}
