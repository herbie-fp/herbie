#!/bin/bash

# exit immediately upon first error, log every command executed
set -e -x

# Ensure egglog is in the path
export PATH="$PATH:$HOME/.cargo/bin/"

# Seed is fixed for the whole day; this way two branches run the same seed
SEED=$(date "+%Y%j")
BENCHDIR="bench/graphics/pbrt.fpcore"
BENCHNAME="pbrt"
REPORTDIR="reports2"
NUMITERS=2

mkdir -p "$REPORTDIR"
rm -rf "reports"/* || echo "nothing to delete"

# run initial herbie
racket -y "src/main.rkt" report \
        --seed "$SEED" \
        --dump-exprs \
        --platform "no-accelerators" \
        "$BENCHDIR" "$REPORTDIR"/"orig_$BENCHNAME" > "$REPORTDIR/expr_dump.txt"

# generate accelerator candidates
racket -y growlibm/generate-candidates.rkt "$REPORTDIR/expr_dump.txt" > "$REPORTDIR/candidates.fpcore"

# extend platform loop
for ((i = 0; i < $NUMITERS; i++)) do
    racket -y "src/main.rkt" report \
            --seed "$SEED" \
            --platform "grow" \
            "$REPORTDIR/candidates.fpcore" "$REPORTDIR"/"iter$i" 

    racket -y "growlibm/extend-platform.rkt"  "$REPORTDIR"/"iter$i/results.json" >> "src/platforms/grow.rkt"
done

# print all operators from the platform
# racket -y growlibm/print-platform-ops.rkt > "$REPORTDIR/platform_ops.txt"

# finally run herbie again with expanded platform
racket -y "src/main.rkt" report \
        --seed "$SEED" \
        --platform "grow" \
        "$BENCHDIR" "$REPORTDIR"/"final_$BENCHNAME"

# dirs=""
# for bench in "$BENCHDIR"/*; do
#   name=$(basename "$bench" .fpcore)
#   rm -rf "$REPORTDIR"/"$name"

#   racket -y "src/main.rkt" report \
#          --seed "$SEED" \
#          "$@" \
#          "$bench" "$REPORTDIR"/"$name"
  
#   dirs="$dirs $name";
# done

# merge reports
# racket -y infra/merge.rkt "$REPORTDIR" $dirs

