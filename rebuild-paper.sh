#!/bin/bash

set -e

THREADS=3
RACKETSEED=$(racket -e "(pseudo-random-generator->vector (current-pseudo-random-generator))")
SEED="\"${RACKETSEED:1}\""
make report FLAGS="-r $SEED"
make publish
cp graphs/results.herbie.dat compile/double.herbie.dat
make report FLAGS="-r $SEED -f sample:double -f precision:double"
make publish
cp graphs/results.herbie.dat compile/single.herbie.dat
make report FLAGS="-r $SEED -f reduce:regimes"
make publish
cp graphs/results.herbie.dat compile/noregimes.herbie.dat
make compile
make rcompile
make -j$THREADS paper
make report BENCHDIR="bench" FLAGS="-r $SEED"
make publish


