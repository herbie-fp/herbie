#!/bin/bash

# exit immediately upon first error, log every command executed
set -e -x

# Ensure egglog is in the path
export PATH="$HOME/.cargo/bin/:$PATH"

# Seed is fixed for the whole day; this way two branches run the same seed
SEED=$(date "+%Y%j")
REPORTDIR="reports"
TIMELINE_FILE="$REPORTDIR/timeline.json"

log_time() {
    python3 growlibm/timeline.py add "$TIMELINE_FILE" "$1"
}

rustup update
make install
BENCHDIR="bench/graphics/pbrt.fpcore"
NUM_ITERS=2
NUM_CANDIDATES=625
NUM_ADD=25

# BENCHDIR="bench/numerics/kahan.fpcore"
# NUM_ITERS=1
# NUM_CANDIDATES=100
# NUM_ADD=5

cp growlibm/grow-template.rkt growlibm/grow.rkt

mkdir -p "$REPORTDIR"
rm -rf "reports"/* || echo "nothing to delete"
python3 growlibm/timeline.py init "$TIMELINE_FILE"

# run initial herbie
racket -y "src/main.rkt" report \
        --seed "$SEED" \
        --platform "grow" \
        --enable "dump:intermediates" \
        --disable "generate:taylor" \
        --disable "generate:evaluate" \
        "$BENCHDIR" \
        "$REPORTDIR/start"
log_time "after_initial_compilation"

# generate accelerator candidates
racket -y growlibm/generate-candidates.rkt "$REPORTDIR" $NUM_CANDIDATES
 
racket -y growlibm/to-json.rkt counts 
racket -y growlibm/to-json.rkt costs 
log_time "after_generate"

# extend platform loop
for ((i = 0; i < $NUM_ITERS; i++)) do
    racket -y "src/main.rkt" report \
            --seed "$SEED" \
            --platform "grow" \
            --threads 4 \
            --disable "generate:taylor" \
            --disable "reduce:regimes" \
            --disable "generate:evaluate" \
            "$REPORTDIR/candidates.txt" \
            "$REPORTDIR/iter$i" 
    log_time "after_run_herbie_candidates_iter_$i"

    racket -y "growlibm/extend-platform.rkt"  "$REPORTDIR/iter$i/results.json" $NUM_ADD $SEED
    log_time "after_add_to_platform_iter_$i"
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
log_time "after_final_compilation"

cat growlibm/grow.rkt

# generate the html report page
python3 growlibm/generate-html.py $NUM_ITERS $REPORTDIR
