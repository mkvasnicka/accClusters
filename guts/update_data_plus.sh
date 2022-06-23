#!/usr/bin/bash

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
runRscript "prepare_points"
runRscript "prepare_hotspots"
runRscript "prepare_clusters"

# runRscript "test"
