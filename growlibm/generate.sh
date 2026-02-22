#!/bin/bash

# exit immediately upon first error, log every command executed
set -e -x

# Ensure egglog is in the path
export PATH="$HOME/.cargo/bin/:$PATH"

# Seed is fixed for the whole day; this way two branches run the same seed
SEED=$(date "+%Y%j")
REPORTDIR="reports"

rustup update
make install
BENCHDIR="bench/graphics/pbrt.fpcore"
NUM_ITERS=2
NUM_CANDIDATES=1000
NUM_ADD=20

# BENCHDIR="bench/mathematics/statistics.fpcore"
# NUM_ITERS=1
# NUM_CANDIDATES=1000
# NUM_ADD=5

cp growlibm/grow-template.rkt growlibm/grow.rkt

mkdir -p "$REPORTDIR"
rm -rf "reports"/* || echo "nothing to delete"

# run initial herbie
racket -y "src/main.rkt" report \
        --seed "$SEED" \
        --dump-exprs \
        --platform "grow" \
        --disable "generate:taylor" \
        --disable "generate:evaluate" \
        "$BENCHDIR" \
        "$REPORTDIR/start" > "$REPORTDIR/expr_dump.txt"

# generate accelerator candidates
racket -y growlibm/generate-candidates.rkt "$REPORTDIR" $NUM_CANDIDATES
 
racket -y growlibm/to-json.rkt counts 
racket -y growlibm/to-json.rkt costs 

# extend platform loop
for ((i = 0; i < $NUM_ITERS; i++)) do
    racket -y "src/main.rkt" report \
            --seed "$SEED" \
            --platform "grow" \
            --threads 4 \
            --disable "generate:taylor" \
            --disable "generate:evaluate" \
            "$REPORTDIR/candidates.txt" \
            "$REPORTDIR/iter$i" 

    racket -y "growlibm/extend-platform.rkt"  "$REPORTDIR/iter$i/results.json" $NUM_ADD $SEED
done

# run herbie again with expanded platform
racket -y "src/main.rkt" report \
        --seed "$SEED" \
        --platform "grow" \
        --threads 4 \
        --disable "generate:taylor" \
        --disable "generate:evaluate" \
        "$BENCHDIR" \
        "$REPORTDIR/end"

cat growlibm/grow.rkt

# generate the html report page
python3 growlibm/generate-html.py $NUM_ITERS $REPORTDIR