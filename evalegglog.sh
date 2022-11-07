#!/bin/bash

# exit immediately upon first error
set -e -x

# build egg-herbie to make sure updated
cargo build --release --manifest-path=egg-herbie/Cargo.toml

SEED=0
CORES=1
BENCHMARKS="2tan.fpcore"

racket src/herbie.rkt report --threads "$CORES" --no-pareto --seed "$SEED" "$BENCHMARKS" egglogreport

racket src/herbie.rkt report --threads "$CORES"  --seed "$SEED" --disable generate:egglog --no-pareto "$BENCHMARKS" vanillareport


bash egglogreport.sh
