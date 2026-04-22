#!/bin/bash

# exit immediately upon first error, log every command executed
set -e -x

# Ensure egglog is in the path
export PATH="$HOME/.cargo/bin/:$PATH"

# Seed is fixed for the whole day; this way two branches run the same seed
SEED=$(date "+%Y%j")
REPORTDIR="reports"
TIMELINE_FILE="$REPORTDIR/timeline.tsv"
SCRIPT_START_EPOCH=$(date "+%s")
SCRIPT_START_ISO=$(date "+%Y-%m-%dT%H:%M:%S%z")

log_time() {
    local phase="$1"
    local iso_time epoch_time elapsed
    iso_time=$(date "+%Y-%m-%dT%H:%M:%S%z")
    epoch_time=$(date "+%s")
    elapsed=$((epoch_time - SCRIPT_START_EPOCH))
    printf '%s\t%s\t%s\n' "$phase" "$iso_time" "$elapsed" | tee -a "$TIMELINE_FILE"
}

# rustup update
# make install
BENCHDIR="bench/coolprop/"
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

printf 'phase\ttimestamp\telapsed_seconds\n' > "$TIMELINE_FILE"
printf 'start\t%s\t0\n' "$SCRIPT_START_ISO" | tee -a "$TIMELINE_FILE"

# run initial herbie
racket -y "src/main.rkt" report \
        --seed "$SEED" \
        --platform "grow" \
        --enable "dump:intermediates" \
        --disable "generate:taylor" \
        --disable "generate:evaluate" \
        "$BENCHDIR" \
        "$REPORTDIR/start" > "$REPORTDIR/expr_dump.txt"
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
