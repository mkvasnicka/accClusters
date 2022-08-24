#!/usr/bin/bash

# this script updates data used in shiny visualization app
# it works in waterfall way: it prepares districts, then it prepares maps for
# all districts, and so on

# you can pass parameters --profile=filename and workers=X where X is either
# positive integer or auto

# folder where R scripts are stored
BASEDIR=guts

# command-line parameters
CMLPARAMS=$@

# stop on error
set -e

# function to run R scripts; it sets RSCRIPTDIR variable and handles other
# parameters, too
runRscript () {
    Rscript --vanilla -e "RSCRIPTDIR=\"$BASEDIR\""\
        -e "source(\"$BASEDIR/$1.R\")" "$CMLPARAMS"
}

runRscript "start_logging"
runRscript "prepare_districts"
runRscript "prepare_maps"
runRscript "prepare_accidents"
runRscript "prepare_hotspots"
runRscript "prepare_clusters"
runRscript "prepare_gis"

# runRscript "test"
