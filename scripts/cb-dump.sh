#!/bin/bash
# Script to get JSON dump of data from current-bench

# NOTE: It assumes the Current bench DB is available as a Docker container
# running on the current machine.

set -euo pipefail

DOCKER_CONTAINER="current-bench_db_1"
HERE=$(dirname "$0")
CB_DIR="cb-dump"
CB_DUMP=$(realpath "${HERE}/../${CB_DIR}/")

cd "$HERE" || exit
docker cp ./cb-dump.sql "${DOCKER_CONTAINER}":cb-dump.sql
docker exec "${DOCKER_CONTAINER}" psql -U docker -f cb-dump.sql
FILES=$(docker exec current-bench_db_1 bash -c 'ls *.json')

mkdir -p "${CB_DUMP}"

for f in $FILES;
do
    docker cp "${DOCKER_CONTAINER}:$f" "${CB_DUMP}/$f"
done

cd ..
tar cvzf cb-dump.tar.gz "${CB_DIR}"
