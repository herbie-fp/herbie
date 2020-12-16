#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: supply the directory which contains the demo json files (v10.json, ect)"
    echo "These files will be converted to fpcore and placed in bench/demosubmissions"
    exit 0
fi

echo "Converting user submitted data into benchmark suite"
if [ -d "bench/demosubmissions" ] ; then
    rm -r "bench/demosubmissions"
fi

mkdir "bench/demosubmissions"

for f in "$1"/*.json; do
    name=$(basename "$f" .json)
    echo "Converting $f"
    racket infra/convert-demo.rkt "$f" "bench/demosubmissions/$name-unsorted.fpcore"
    racket infra/sort-fpcore.rkt "bench/demosubmissions/$name-unsorted.fpcore" "bench/demosubmissions/$name.fpcore"
    rm "bench/demosubmissions/$name-unsorted.fpcore"
done
