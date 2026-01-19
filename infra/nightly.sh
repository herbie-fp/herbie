#!/bin/bash

# exit immediately upon first error, log every command executed
set -e -x

# Ensure egglog is in the path
export PATH="$HOME/.cargo/bin/:$PATH"
rustup update

make install

# Seed is fixed for the whole day; this way two branches run the same seed
SEED=$(date "+%Y%j")
BENCHDIR="bench/PROJ/"
REPORTDIR="reports"
NUMITERS=10

mkdir -p "$REPORTDIR"
rm -rf "reports"/* || echo "nothing to delete"

# run initial herbie
racket -y "src/main.rkt" report \
        --seed "$SEED" \
        --dump-exprs \
        --platform "no-accelerators" \
        --disable "generate:taylor" \
        --disable "generate:evaluate" \
        "$BENCHDIR" \
        "$REPORTDIR/start" > "$REPORTDIR/expr_dump.txt"

# generate accelerator candidates
racket -y growlibm/generate-candidates2.rkt "$REPORTDIR"
 
racket -y growlibm/to-json.rkt 

# extend platform loop
for ((i = 0; i < $NUMITERS; i++)) do
    racket -y "src/main.rkt" report \
            --seed "$SEED" \
            --platform "grow" \
            --threads 4 \
            --disable "generate:taylor" \
            --disable "generate:evaluate" \
            "$REPORTDIR/candidates.txt" \
            "$REPORTDIR/iter$i" 

    racket -y "growlibm/extend-platform.rkt"  "$REPORTDIR/iter$i/results.json" 
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

# print the new platform
cat "src/platforms/grow.rkt" > "$REPORTDIR/grow_platform.txt"

# generate the html report page
python3 growlibm/generate-html.py $NUMITERS $REPORTDIR