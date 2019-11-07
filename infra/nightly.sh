#!/bin/bash

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

mkdir -p reports
for bench in bench/*; do
  name=$(basename "$bench" .fpcore)
  run "$bench" "$name"  --seed $(date "+%Y%j") "$@"
done
