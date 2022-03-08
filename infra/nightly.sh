#!/bin/bash

# lowered number of cores from 6 to 4 to avoid pagetable error
# caused by heavy use of FFI by eggmath.rkt
CORES=4

set -e -x
SEED=$(date "+%Y%j")

function output_error {
    NAME="$2"
    DATE=`date +%s`
    COMMIT=`git rev-parse HEAD`
    BRANCH=`git rev-parse --abbrev-ref HEAD`
    HOSTNAME=`hostname`
    cat >"$1" <<EOF
{ "date": $DATE, "commit": "$COMMIT", "branch": "$BRANCH",
  "hostname": "$HOSTNAME", "seed": "$SEED", "flags": [],
  "points": -1, "iterations": -1, "bit_width": -1,
  "note": "$NAME", "crash": true, "tests": [] }
EOF
}

function run {
  bench="$1"; shift
  name="$1"; shift
  
  echo "Running $name test with flags $@"
  rm -rf "reports/$name"
  racket "src/herbie.rkt" report \
      --note "$name" \
      --profile \
      --debug \
      --seed "$SEED" \
      --threads "$CORES" \
      "$@" \
      "$bench" "reports/$name" \
      || output_error "reports/$name/results.json" "$name"
}

DIR="$1"
shift

report=$(git rev-parse --abbrev-ref HEAD)-$(date "+%Y-%m-%d")
mkdir -p "$DIR"
rm -rf "$DIR"/* || echo "No existing reports to delete"
dirs=""
for bench in bench/*; do
  name=$(basename "$bench" .fpcore)
  run "$bench" "$name" "$@"
  if [ "$?" -eq 0 ]; then
      dirs="$dirs $name";
  fi
done


racket infra/nightly.rkt "$DIR" $dirs
bash infra/publish.sh upload "$DIR"
bash infra/publish.sh index
