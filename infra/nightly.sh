#!/bin/bash

# exit immediately upon first error, log every command executed
set -e -x

# Ensure egglog is in the path
export PATH="$PATH:$HOME/.cargo/bin/"

# Seed is fixed for the whole day; this way two branches run the same seed
SEED=$(date "+%Y%j")
BENCHDIR="bench/"
REPORTDIR="reports"
NUMITERS=5

mkdir -p "$REPORTDIR"
rm -rf "reports"/* || echo "nothing to delete"

# run initial herbie
racket -y "src/main.rkt" report \
        --seed "$SEED" \
        --dump-exprs \
        --platform "no-accelerators" \
        --disable "generate:evaluate" \ 
        "$BENCHDIR" "$REPORTDIR"/"start" > "$REPORTDIR/expr_dump.txt"

# generate accelerator candidates
racket -y growlibm/generate-candidates.rkt "$REPORTDIR/expr_dump.txt" > "$REPORTDIR/candidates.fpcore"

# extend platform loop
for ((i = 0; i < $NUMITERS; i++)) do
    racket -y "src/main.rkt" report \
            --seed "$SEED" \
            --platform "grow" \
            --threads 4 \
            --disable "generate:taylor" \
            --disable "generate:evaluate" \ 
            "$REPORTDIR/candidates.fpcore" "$REPORTDIR"/"iter$i" 

    racket -y "growlibm/extend-platform.rkt"  "$REPORTDIR"/"iter$i/results.json" >> "src/platforms/grow.rkt"
done

# racket -y growlibm/print-platform-ops.rkt > "$REPORTDIR/platform_ops.txt"

# print all operators from the platform

# run herbie again with expanded platform
racket -y "src/main.rkt" report \
        --seed "$SEED" \
        --platform "grow" \
        --threads 4 \
        --disable "generate:evaluate" \ 
        "$BENCHDIR" "$REPORTDIR"/"end"

cat "src/platforms/grow.rkt" > "$REPORTDIR/grow_platform.txt"

# generate the html report page
python3 growlibm/generate-html.py

# racket -y "src/main.rkt" report \
#         --seed "$SEED" \
#         --platform "no-accelerators" \
#         --threads 4 \
#         "bench/" "$REPORTDIR"/"full_bench_no_accelerators"

# racket -y "src/main.rkt" report \
#         --seed "$SEED" \
#         --platform "grow" \
#         --threads 4 \
#         "bench/" "$REPORTDIR"/"full_bench_grow"

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

