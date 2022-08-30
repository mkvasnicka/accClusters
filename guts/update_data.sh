#!/usr/bin/bash

# this script updates data used in shiny visualization app
# it works in waterfall way: it prepares districts, then it prepares maps for
# all districts, and so on

# stop on error
set -e

# folder where R scripts are stored---unless replaced with -r option
RSCRIPTDIR_IMPLICIT=guts

# folder where all data are stored---unless replaced with -b option
DIRORIGIN_IMPLICIT=guts_data

# help
help()
{
    echo "update_data.sh updates traffic accidents data for shiny."
    echo
    echo "Syntax: update_data [-r|b|h]"
    echo "options:"
    echo "r     Sets path to the folder where R scripts are stored."
    echo "b     Sets path to the folder where data are stored."
    echo "h     Print this help."
    echo
}

# function to run R scripts; it sets RSCRIPTDIR variable and handles other
# parameters, too
runRscript () {
    echo ""
    echo "Running $1.R"
    echo "------------------------------------------------"
    Rscript --vanilla -e "RSCRIPTDIR=\"$RSCRIPTDIR\"" \
        -e "DIR_ORIGIN=\"DIRORIGIN\"" \
        -e "source(\"$RSCRIPTDIR/$1.R\")"
}

# process flag parameters; use the implicit values it they aren't set
while getopts :hr:b: flag
do
    case "${flag}" in
        h) help
           exit;;
        r) RSCRIPTDIR=${OPTARG};;
        b) DIRORIGIN=${OPTARG};;
    esac
done
if [ -z $RSCRIPTDIR ]; then
    RSCRIPTDIR=$RSCRIPTDIR_IMPLICIT
fi
if [ -z $DIRORIGIN ]; then
    DIRORIGIN=$DIRORIGIN_IMPLICIT
fi

# show configuration
echo "Updating data on traffic accidents for shiny app"
echo "================================================"
echo "Using RSCRIPTDIR=\"$RSCRIPTDIR\", DIR_ORIGIN=\"$DIRORIGIN\"."

runRscript "start_logging"
runRscript "prepare_profiles"
# runRscript "prepare_districts"
# runRscript "prepare_maps"
# runRscript "prepare_accidents"
# runRscript "prepare_densities"
# runRscript "prepare_clusters"
# runRscript "prepare_gis"

# runRscript "test"
