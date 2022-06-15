#!/usr/bin/bash
set -e
BASEDIR=guts
Rscript --vanilla $BASEDIR/start_logging.R
Rscript --vanilla $BASEDIR/prepare_districts.R
Rscript --vanilla $BASEDIR/prepare_maps.R
Rscript --vanilla $BASEDIR/prepare_points.R
Rscript --vanilla $BASEDIR/prepare_hotspots.R
# Rscript --vanilla $BASEDIR/prepare_clusters.R
