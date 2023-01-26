#!/bin/bash

BASEDIR=guts/rawdata/accidents
PWD=$(pwd)

cd $BASEDIR
for FOLDER in */
do
    echo $FOLDER
    cd $FOLDER
    for XLS in *.xls
    do
        CSV=${XLS/xls/csv}
        if [[ ! -f $CSV ]] | [ $XLS -nt $CSV ]
        then
            unoconv -f csv $XLS
        fi
    done
    cd ..
done

cd $PWD
