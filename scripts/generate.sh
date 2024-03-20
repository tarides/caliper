#!/bin/bash
# Script to generate Caliper HTML from Current Bench dump directory

set -euo pipefail

HERE=$(dirname "$0")
CB_DUMP=$(realpath "${HERE}/../cb-dump/")
CACHE=$(realpath "${HERE}/../cache/")

rm -rf "${CACHE}"
mkdir -p "${CACHE}"

cd "${HERE}/.." || exit

# Download dump if it doesn't exist
if [ ! -d "${CB_DUMP}" ];
then
    wget -c https://github.com/ocurrent/current-bench/files/14662859/cb-dump.tar.gz
    tar xzf cb-dump.tar.gz
fi

# Generate HTML for the dumped data
echo "Generating cache dir ..."
for f in "${CB_DUMP}"/*.json;
do
    echo "Parsing ${f} ..."
    dune exec -- bin/main.exe parse-cb-json --cache-dir="${CACHE}" --json-dump "${f}"
done
echo "Generating HTML ..."
dune exec -- bin/main.exe generate-html --cache-dir="${CACHE}" --output-dir=/tmp/caliper
