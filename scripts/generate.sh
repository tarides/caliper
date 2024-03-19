#!/bin/bash
# Script to generate Caliper HTML from Current Bench dump directory

set -euo pipefail

HERE=$(dirname "$0")
CB_DUMP=$(realpath "${HERE}/../cb-data/")

# Generate HTML for the dumped data
CACHE=$(realpath "${HERE}/../cache/")
echo "Generating cache dir ..."
for f in "${CB_DUMP}"/*.json;
do
    dune exec -- bin/main.exe parse-cb-json --cache-dir="${CACHE}" --json-dump "${f}"
done
echo "Generating HTML ..."
dune exec -- bin/main.exe generate-html --cache-dir="${CACHE}" --output-dir=/tmp/caliper
