#!/bin/bash

# lowered number of cores from 6 to 4 to avoid pagetable error
# caused by heavy use of FFI by eggmath.rkt
CORES=4

function run {
  bench=$1; shift
  name=$1; shift

  echo "Running $name test with flags $@"
  racket "src/herbie.rkt" report \
      --note "$name" \
      --profile \
      --debug \
      --threads $CORES \
      "$@" \
      "$bench" "reports/$name"
  bash infra/publish.sh upload "reports/$name"
}

# install package deps
raco pkg install --auto ./src
raco pkg update --auto ./src

mkdir -p reports
for bench in bench/*; do
  name=$(basename "$bench" .fpcore)
  run "$bench" "$name"  --seed $(date "+%Y%j") "$@"
done
