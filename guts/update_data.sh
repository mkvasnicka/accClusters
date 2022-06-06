#!/usr/bin/bash

BASEDIR=guts
Rscript --vanilla $BASEDIR/prepare_districts.R
Rscript --vanilla $BASEDIR/prepare_maps.R
Rscript --vanilla $BASEDIR/prepare_points.R
# Rscript --vanilla
# Rscript --vanilla
