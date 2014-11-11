#!/bin/bash

set -v

THREADS=3
RACKETSEED=$(racket -e "(pseudo-random-generator->vector (current-pseudo-random-generator))")
SEED=${RACKETSEED:1}
make report FLAGS="-r $SEED"
make publish
cp graphs/results.casio.dat compile/double.casio.dat
make report FLAGS="-r $SEED -f sample:float -f precision:float"
make publish
cp graphs/results.casio.dat compile/single.casio.dat
make report FLAGS="-r $SEED -f reduce:regimes"
make publish
cp graphs/results.casio.dat compile/noregimes.casio.dat
make compile
make rcompile
make -j$THREADS paper
make report bench FLAGS="-r $SEED"
make publish


