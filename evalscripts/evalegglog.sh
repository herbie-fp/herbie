#!/bin/bash

# exit immediately upon first error
set -e -x


SEED=0
CORES=16
BENCHMARKS="bench/"
TIMEOUT=500

ls
make egglog
racket -y src/herbie.rkt report --threads "$CORES" --no-pareto --seed "$SEED" --timeout "$TIMEOUT"  "$BENCHMARKS" egglogreport

git checkout main
cargo build --release --manifest-path=egg-herbie/Cargo.toml
racket -y src/herbie.rkt report --threads "$CORES" --timeout "$TIMEOUT" --seed "$SEED" --no-pareto "$BENCHMARKS" vanillareport
git checkout oflatt-egglog-ctx


bash egglogreport.sh
