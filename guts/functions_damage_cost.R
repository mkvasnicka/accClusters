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


# unit_cost_function() reads in config variables and returns them as a double
# vector
#
# inputs:
#   none
#
# value:
#   named double vector of slots "dead", "serious_injury", "light_injury",
#   "material_cost", and "const"; the last two should probably be 1 and zero
#   respectively
# unit_cost_function <- function() {
#     c(dead = UNIT_COSTS_DEAD,
#       serious_injury = UNIT_COSTS_SERIOUS_INJURY,
#       light_injury = UNIT_COSTS_LIGHT_INJURY,
#       material_cost = UNIT_COSTS_MATERIAL,
#       const = UNIT_COST_CONST)
# }


# add_damage_cost(accidents) takes a tibble of accidents and adds a new column,
# accident_cost
#
# inputs:
# - accidents ... (tibble) accidents table
# - accident_dead ... (integer) number of the dead in the accident
# - accident_serious_injury ... (integer) number of the seriously injured
# - accident_light_injury ... (integer) number of the light injured
# - accident_material_cost ... (double) material cost in mil. CZK
#
# value:
#   the same tibble as accidents but new column, accident cost, is added
add_damage_cost <- function(accidents,
                            unit_cost_dead, unit_cost_serious_injury,
                            unit_cost_light_injury, unit_cost_material,
                            unit_cost_const) {
    accidents |>
        mutate(accident_cost =
                   accident_dead * unit_cost_dead +
                   accident_serious_injury * unit_cost_serious_injury +
                   accident_light_injury * unit_cost_light_injury +
                   accident_material_cost * unit_cost_material +
                   unit_cost_const
        )
}
