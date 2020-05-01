#!/bin/bash

# lowered number of cores from 6 to 4 to avoid pagetable error
# caused by heavy use of FFI by eggmath.rkt
CORES=4

function seed {
    date "+%Y%j"
}

function output_error {
    DATE=`date +%s`
    COMMIT=`git rev-parse HEAD`
    BRANCH=`git rev-parse --abbrev-ref HEAD`
    HOSTNAME=`hostname`
    SEED=`seed`
    NOTE="$2"
    cat >"$1" <<EOF
{ "date": "$DATE", "commit": "$COMMIT", "branch": "$BRANCH",
  "hostname": "$HOSTNAME", "seed": "$SEED", "flags": [],
  "points": -1, "iterations": -1, "bit_width": -1,
  "note": "$NOTE", "crash": true, "tests": [] }
EOF
}

function run {
  bench=$1; shift
  name=$1; shift

  echo "Running $name test with flags $@"
  rm -rf "reports/$name"
  racket "src/herbie.rkt" report \
      --note "$name" \
      --profile \
      --debug \
      --seed $(seed) \
      --threads $CORES \
      "$@" \
      "$bench" "reports/$name" || output_error "reports/$name/results.json" "$name"
  bash infra/publish.sh upload "reports/$name"
}

# install package deps
raco pkg install --auto ./src
raco pkg update --auto ./src

mkdir -p reports
for bench in bench/*; do
  name=$(basename "$bench" .fpcore)
  run "$bench" "$name"  "$@"
done
