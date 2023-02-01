#!/bin/bash

# exit immediately upon first error
set -e -x


SEED=0
CORES=16
BENCHMARKS="bench/"
TIMEOUT=500


git checkout main
make install
racket -y src/herbie.rkt report --num-enodes 15000 --threads "$CORES" --timeout "$TIMEOUT" --seed "$SEED" --no-pareto "$BENCHMARKS" vanillareport
git checkout oflatt-egglog-ctx


#make egglog
#racket -y src/herbie.rkt report --threads "$CORES" --no-pareto --seed "$SEED" --timeout "$TIMEOUT"  "$BENCHMARKS" egglogreport



bash evalscripts/egglogreport.sh
