#!/usr/bin/bash

# this script updates data used in shiny visualization app
# it works in waterfall way: it prepares districts, then it prepares maps for
# all districts, and so on

# stop on error
set -e

# folder where R scripts are stored---unless replaced with -r option
RSCRIPTDIR_IMPLICIT=.

# folder where all data are stored---unless replaced with -b option
DIRORIGIN_IMPLICIT=data

# help
usage()
{
    echo "Usage $(basename $0) [-h] | [-m -c] | -k"
    echo "Syntax: "
    echo "  h     Print this help."
    echo "  m     Stores the manual in the data/man folder."
    echo "  c     Stores the initial config/profile in the data/config folder."
    echo "        (The folder must be empty or non-existent; error is thrown otherwise.)"
    echo "  k     Accidents clusters are updated."
    echo "  h     Print this help."
    echo "If not additional parameter is present, basic data are updated."
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
            echo "Writing the manual to $DIRORIGIN/man"
            WHATTODO="nothing"
            if [ -d "$DIRORIGIN/man" ];
            then
                # echo "$DIR directory exists."
            else
            	mkdir "$DIRORIGIN/man"
            fi
            if [ -f "$FILE" ];
            then
                echo "The manual already exists; skipping."
            else
                cp man/dokumentace.pdf "$DIRORIGIN/man"
            fi
            ;;
        c)
            echo "Writing basic config to $DIRORIGIN/config"
            WHATTODO="nothing"
            if [ -d "$DIRORIGIN/config" ];
            then
                echo "$DIRORIGIN/config already exists. I can't overwrite it."
                echo "If you want to create it from the scratch, remove the folder."
            else
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
if [ -z $RSCRIPTDIR ]; then
    RSCRIPTDIR=$RSCRIPTDIR_IMPLICIT
fi
if [ -z $DIRORIGIN ]; then
    DIRORIGIN=$DIRORIGIN_IMPLICIT
fi

# #TODO remove
# echo "Arguments:"
# echo $*

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
    runRscript "prepare_accidents"
fi

# update clusters
if [[ $WHATTODO == *clusters* ]]; then
    runRscript "prepare_densities"
    runRscript "prepare_clusters"
    runRscript "prepare_sidecars"
    runRscript "prepare_gis"
fi
