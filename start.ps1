[CmdletBinding(PositionalBinding=$False)]
param(
    [Parameter()] [switch] $h,
    [Parameter()] [string] $i = "mkvasnicka/acc-clusters:latest",
    [Parameter()] [string] $n = "",
    [Parameter()] [string] $d = "$(pwd)",
    [Parameter()] [string] $p = "",
    [Parameter()] [switch] $m,
    [Parameter()] [switch] $c,
    [Parameter()] [switch] $k
#    [Parameter(ValueFromRemainingArguments)] [string] $Remaining
)

function usage {
	echo "Použití:"
	echo "start.sh [-h] [-i <image>] [-n <container name>] [-d <data folder>] [-m] [-c] [-k]"
	echo "  -h ... vypíše nápovědu"
	echo "  -i <image> ... volitelný název použitého docker image; implicitně acc-clusters:latest"
	echo "  -n <container name> ... volitelný název kontejneru"
    echo "  -p <docker run parameters> ... volitelné parametry předané dockeru při spuštění kontejneru"
	echo "  -d <data folder> ... volitelná cesta ke složce s daty; implicitně aktuální adresář"
	echo "  -m ... volitelně uloží na disk PDF soubor s dokumentací -- pokud neexistuje"
	echo "  -c ... volitelně uloží na disk počáteční nastavení konfigurace -- pokud neexistuje"
	echo "  -k ... volitelně aktualizuje klastry dopravních nehod"
	echo "Pokud není zadán žádný z parametrů -m, -c, ani -k, aktualizuje běžná data o nehodách."
	exit 2
}

if ($h) { usage }
if ($n) { $n = "--name $n" }

$CAs += if ($m.IsPresent) {"-m "}
$CAs += if ($c.IsPresent) {"-c "}
$CAs += if ($k.IsPresent) {"-k"}

$cmd="docker run -it --rm --mount type=bind,source='$d',target=/usr/src/accClusters/data $n $p $i $CAs"

Invoke-Expression $cmd
