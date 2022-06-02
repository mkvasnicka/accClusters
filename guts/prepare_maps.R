# -------------------------------------
# Script:   prepare_maps.R
# Author:   Michal Kvasnička
# Purpose:
# Inputs:
# Outputs:
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# source necessary scripts
RSCRIPTDIR <- "guts"
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "auxiliary_functions.R"))
source(file.path(RSCRIPTDIR, "districts_preparation_functions.R"))
source(file.path(RSCRIPTDIR, "map_preparation_functions.R"))


#' Načtou se distrikty (tady okresy) a převedou se do stejné planární
#' transformace jako silnice. To, co potřebujeme, jsou polygony ploch distriktů.
#' Navíc přidáme naše vlastní jména distriktů a jejich id. To umožní abstrakci
#' nad tím, co jsou distrikty (nemusí být nutně okresy) a jaké mapy se použijí,
#' tj. jak to mají pojmenované.
districts <- read_arccr_districts(PATH_TO_RAW_DISTRICTS)
write_dir_rds(districts, file = PATH_TO_DISTRICTS)
