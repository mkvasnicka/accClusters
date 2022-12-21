#!/usr/bin/pwsh

param(
    [string] 
    $i = "acc-clusters:latest",
    [string]
    [Parameter(Position = 0, ValueFromRemainingArguments)]
    $Remaining
    )
    
function usage {
	echo "Použití: basename $0 [-i <image>] <parametry pro skript update_data.sh>"
	echo "         -i <image> = volitelný název docker image pro spuštění, výchozí = acc-clusters:latest"
	echo "         - v aktuálním adresáři musí být podadresář 'data', který se namontuje dovnitř do kontejneru"
	exit 2
}

if (! $Remaining) {
    usage
}

docker run -it --rm --mount type=bind,source="$(pwd)"/data,target=/usr/src/accClusters/data $i $Remaining