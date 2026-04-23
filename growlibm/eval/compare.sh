#!/bin/bash

# exit immediately upon first error, log every command executed
set -e -x

# Ensure egglog is in the path
export PATH="$PATH:$HOME/.cargo/bin/"

# Seed is fixed for the whole day; this way two branches run the same seed
SEED=$(date "+%Y%j")
BENCHDIR="bench/PROJ/"
REPORTDIR="reports"

mkdir -p "$REPORTDIR"
rm -rf "$REPORTDIR"/* || echo "nothing to delete"

clang -dynamiclib -O3 -o growlibm/libaccelerators.dylib growlibm/accelerators.c

# run regular herbie
racket -y "src/main.rkt" report \
        --seed "$SEED" \
        --platform "herbie20" \
        "$BENCHDIR" \
        "$REPORTDIR/herbie20" 

racket -y "src/main.rkt" report \
        --seed "$SEED" \
        --platform "no-accelerators" \
        "$BENCHDIR" \
        "$REPORTDIR/no-accelerators" 

racket -y "src/main.rkt" report \
        --seed "$SEED" \
        --platform "growlibm" \
        "$BENCHDIR" \
        "$REPORTDIR/grow" 

# generate the html report page
python3 growlibm/generate-compare-html.py $REPORTDIR