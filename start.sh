#!/bin/bash

usage() {
	echo "Použití:"
	echo "start.sh [-h] [-i <image>] [-n <container name>] [-d <data folder>] [-m] [-c] [-k]"
	echo "  -h ... vypíše nápovědu"
	echo "  -i <image> ... volitelný název použitého docker image; implicitně acc-clusters:latest"
	echo "  -n <container name> ... volitelný název kontejneru"
	echo "  -d <data folder> ... volitelná cesta ke složce s daty; implicitně aktuální adresář"
	echo "  -m ... volitelně uloží na disk PDF soubor s dokumentací -- pokud neexistuje"
	echo "  -c ... volitelně uloží na disk počáteční nastavení konfigurace -- pokud neexistuje"
	echo "  -k ... volitelně aktualizuje klastry dopravních nehod"
	echo "Pokud není zadán žádný z parametrů -m, -c, ani -k, aktualizuje běžná data o nehodách."
	exit 2
}

# implicit values
IMAGE="acc-clusters:latest"
DATAFOLDER="$(pwd)"
CONTAINERNAME=""
CONTAINERARGS=""

# evaluate options
while getopts ":hi:n:d:" OPTION; do
	case $OPTION in
	    h)
	        usage
	        ;;
		i)
			IMAGE="$OPTARG"
			;;
		n)
		    CONTAINERNAME="--name $OPTARG"
		    ;;
		d)
		    DATAFOLDER="$OPTARG"
		    ;;
		\?)
		    CONTAINERARGS="$CONTAINERARGS -$OPTARG"
	esac
done
shift "$(($OPTIND -1))"

docker run -it --rm \
--mount type=bind,source="$DATAFOLDER",target=/usr/src/accClusters/data \
$CONTAINERNAME $IMAGE $CONTAINERARGS
