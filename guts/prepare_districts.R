# -------------------------------------
# Script:   prepare_maps.R
# Author:   Michal Kvasnička
# Purpose:  This script creates/updates districts (okresy now). It reads them
#           from ARCCR maps and tranforms to Křovák CRS. These polygons are used
#           to crop roads to districts.
# Inputs:   ARCCR maps of political districts in the Czech Republic.
# Outputs:  sf tibble districts (saved on disk)
# Notes:
#
# Copyright(c) Michal Kvasnička
# -------------------------------------

# source necessary scripts
RSCRIPTDIR <- "guts"
source(file.path(RSCRIPTDIR, "guts_config.R"))
source(file.path(RSCRIPTDIR, "functions_auxiliary.R"))
source(file.path(RSCRIPTDIR, "functions_districts_preparation.R"))


#' Načtou se distrikty (tady okresy) a převedou se do stejné planární
#' transformace jako silnice. To, co potřebujeme, jsou polygony ploch distriktů.
#' Navíc přidáme naše vlastní jména distriktů a jejich id. To umožní abstrakci
#' nad tím, co jsou distrikty (nemusí být nutně okresy) a jaké mapy se použijí,
#' tj. jak to mají pojmenované.
if (is_behind(PATH_TO_DISTRICTS, PATH_TO_RAW_DISTRICTS)) {
    districts <- read_arccr_districts(PATH_TO_RAW_DISTRICTS)
    write_dir_rds(districts, file = PATH_TO_DISTRICTS)
}
