#!/usr/bin/pwsh
docker run -it --mount type=bind,source="$(pwd)"/data,target=/usr/src/accClusters/data --entrypoint bash acc-clusters:rc1
