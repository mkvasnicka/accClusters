#!/bin/bash

set -e

usage()
{
    echo "Usage $(basename $0) [-h] | [-m -c] | -k"
    echo "Syntax: "
    echo "h     Print this help."
    echo "m     Stores the manual in the data/man folder."
    echo "c     Stores the initial config/profile in the data/config folder."
    echo "      (The folder must be empty or non-existent; error is thrown otherwise.)"
    echo "k     Accidents clusters are updated."
    echo "h     Print this help."
    echo
    echo "If not additional parameter is present, basic data are updated."
    echo

}

WHATTODO="update"
while getopts 'hmck' opt; do
    case "$opt" in
        m)
            echo "m"
            WHATTODO="nothing"
            ;;
        c)
            echo "c"
            WHATTODO="nothing"
            ;;
        k)
            if [ $WHATTODO == "nothing" ]; then
                echo "You cannot produce manual/initial config and calculate clusters at the same time!"
                exit 1
            fi
            WHATTODO="update clusters"
            ;;
        ?|h)
            usage
            exit 1
            ;;
    esac
done
shift "$(($OPTIND -1))"

echo $WHATTODO


if [[ $WHATTODO == update* ]]; then
                echo "updating basic data..."
fi


if [[ $WHATTODO == *clusters ]]; then
                echo "updating clusters..."
fi
