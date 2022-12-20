#!/bin/bash

usage() {
	echo "Použití: basename $0 [-i <image>] <parametry pro skript update_data.sh>"
	echo "         -i <image> = volitelný název docker image pro spuštění, výchozí = acc-clusters:latest"
	echo "         - v aktuálním adresáři musí být podadresář "data", který se nemontuje dovnitř do kontejneru"
	exit 2
}

IMAGE="acc-clusters:latest"
# evaluate options
while getopts ":i:" OPTION; do
	case $OPTION in
		i)
			IMAGE="$OPTARG"	
			shift 2
			;;
	esac
done

[ -z "$1" ] && usage

docker run -it --rm --mount type=bind,source="$(pwd)"/data,target=/usr/src/accClusters/data $IMAGE $*
