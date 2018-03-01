#!/bin/bash

CORES=6

function run {
  bench=$1; shift
  name=$1; shift

  time xvfb-run --auto-servernum \
    racket "src/herbie.rkt" report \
      --note "$name" \
      --profile \
      --threads $CORES \
      "$@" \
      "$bench" "reports/"
}

function runEach {
  for bench in bench/*; do
    name=$(basename "$bench" .fpcore)
    # add cases to skip large or misbehaving benchmarks
    case $name in
      haskell) ;;
      random) ;;
      *) run "$bench" "$name" "$@" ;;
    esac
  done
}

# use common seed across every 4 day cycle
d=$(date "+%j")
qseed=$(racket -e " \
  (random-seed $(expr $d / 4)) \
  (pseudo-random-generator->vector \
    (current-pseudo-random-generator))")
seed="${qseed:1}" # :1 removes leading quote

runEach --seed "$seed"
