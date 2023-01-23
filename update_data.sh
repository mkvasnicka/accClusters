#!/usr/bin/bash

# this script updates data used in shiny visualization app
# it works in waterfall way: it prepares districts, then it prepares maps for
# all districts, and so on

# stop on error
set -e

# folder where R scripts are stored---unless replaced with -r option
RSCRIPTDIR=.

# folder where all data are stored---unless replaced with -b option
DIRORIGIN=data

# help
usage() {
	echo "Použití:"
	echo "start.sh [-h] [-i <image>] [-n <container name>] [-d <data folder>] [-m] [-c] [-k]"
	echo "  -h ... vypíše nápovědu"
	echo "  -i <image> ... volitelný název použitého docker image; implicitně acc-clusters:latest"
	echo "  -n <container name> ... volitelný název kontejneru"
	echo "  -p <docker run parameters> ... volitelné parametry předané dockeru při spuštění kontejneru"
	echo "  -d <data folder> ... volitelná cesta ke složce s daty; implicitně aktuální adresář"
	echo "  -m ... volitelně uloží na disk PDF soubor s dokumentací -- pokud neexistuje"
	echo "  -c ... volitelně uloží na disk počáteční nastavení konfigurace -- pokud neexistuje"
	echo "  -k ... volitelně aktualizuje klastry dopravních nehod"
	echo "Pokud není zadán žádný z parametrů -m, -c, ani -k, aktualizuje běžná data o nehodách."
	exit 2
}

# function to run R scripts; it sets RSCRIPTDIR variable and handles other
# parameters, too
runRscript () {
    echo ""
    echo "Running $1.R"
    echo "------------------------------------------------"
    Rscript -e "RSCRIPTDIR=\"$RSCRIPTDIR\"" \
        -e "DIR_ORIGIN=\"$DIRORIGIN\"" \
        -e "source(\"$RSCRIPTDIR/$1.R\")"
}

# process flag parameters; use the implicit values it they aren't set
WHATTODO="update"
while getopts 'hmckr:b:' opt; do
    case "$opt" in
        m)
            WHATTODO="nothing"
            if [ -f "$DIRORIGIN/man/dokumentace.pdf" ];
            then
                echo "ERROR: The manual already exists at "$DIRORIGIN/man/dokumentace.pdf"; skipping."
                echo "       If you want to recreate it, remove this file."
            else
                echo "Writing the manual to $DIRORIGIN/man."
                mkdir -p "$DIRORIGIN/man"
                cp man/dokumentace.pdf "$DIRORIGIN/man"
            fi
            ;;
        c)
            WHATTODO="nothing"
            if [ -d "$DIRORIGIN/config" ];
            then
                echo "ERROR: $DIRORIGIN/config already exists. I can't overwrite it."
                echo "       If you want to create it from the scratch, remove this folder."
            else
                echo "Writing basic config to $DIRORIGIN/config."
                mkdir "$DIRORIGIN/config"
                cp config/* "$DIRORIGIN/config"
            fi
            ;;
        k)
            if [ $WHATTODO == "nothing" ]; then
                echo "You cannot produce manual/initial config and calculate clusters at the same time!"
                exit 1
            fi
            WHATTODO="update clusters"
            ;;
        r) RSCRIPTDIR=${OPTARG};;
        b) DIRORIGIN=${OPTARG};;
        ?|h)
            usage
            exit 1
            ;;
    esac
done
shift "$(($OPTIND -1))"

# show configuration
if [[ $WHATTODO == *update* ]]; then
    NICESTRING="only"
    if [[ $WHATTODO == *clusters* ]]; then
        NICESTRING="and accidents clusters"
    fi
    echo "Updating data on traffic accidents for shiny app"
    echo "================================================"
    echo "Updating basic statistics $NICESTRING."
    echo "Using RSCRIPTDIR=\"$RSCRIPTDIR\", DIR_ORIGIN=\"$DIRORIGIN\"."
fi

# update basic statistics
if [[ $WHATTODO == *update* ]]; then
    runRscript "start_logging"
    runRscript "prepare_profiles"
    runRscript "prepare_districts"
    runRscript "prepare_maps"
    runRscript "prepare_maps_lixels"
    runRscript "prepare_accidents_raw"
    runRscript "prepare_accidents_shiny"
fi

# update clusters
if [[ $WHATTODO == *clusters* ]]; then
    runRscript "prepare_maps_lixel_samples"
    runRscript "prepare_maps_lixel_neighbors"
    runRscript "prepare_accidents"
    runRscript "prepare_densities"
    runRscript "prepare_clusters"
    runRscript "prepare_sidecars"
    runRscript "prepare_gis"
fi
