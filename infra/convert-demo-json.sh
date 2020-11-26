#!/bin/bash

if [ "$#" -ne 1 ]; then
    echo "Usage: supply the directory which contains the demo json files (v10.json, ect)"
    echo "These files will be converted to fpcore and placed in bench/demosubmissions"
    exit 0
fi

echo "Converting user submitted data into benchmark suite"
if [ -f "bench/demosubmissions" ] ; then
    rm "bench/demosubmissions"
fi

mkdir "bench/demosubmissions"

for f in "$1"; do
  name=$(basename "$f" .json)
  racket infra/convert-demo.rkt "$1/$f" "bench/demosubmissions/$name-unsorted.fpcore"
  racket infra/sort-fpcore.rkt "bench/demosubmissions/$name-unsorted.fpcore" "bench/demo/$name.fpcore"
  rm "bench/demosubmissions/$name-unsorted.fpcore"
done
