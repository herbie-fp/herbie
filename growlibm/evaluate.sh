#!/bin/bash

# exit immediately upon first error, log every command executed
set -e -x

# Ensure egglog is in the path
export PATH="$HOME/.cargo/bin/:$PATH"
# rustup update

# make install

# Seed is fixed for the whole day; this way two branches run the same seed
SEED=$(date "+%Y%j")
BENCHDIR="$1"
REPORTDIR="$2"
PLATFORM="$3"
NUM_ENODES="16000"

mkdir -p "$REPORTDIR"
rm -rf "$REPORTDIR"/* || echo "nothing to delete"

racket -y src/main.rkt report \
        --seed $SEED \
        --platform $PLATFORM \
        --num-enodes $NUM_ENODES \
        $BENCHDIR \
        $REPORTDIR/growlibm_base

racket -y src/main.rkt report \
        --seed "$SEED" \
        --platform herbie20 \
        --num-enodes $NUM_ENODES \
        $BENCHDIR \
        $REPORTDIR/herbie20_base

racket -y src/main.rkt report \
        --seed $SEED \
        --platform vanilla \
        --num-enodes $NUM_ENODES \
        $BENCHDIR \
        $REPORTDIR/vanilla_base

# if [[ "$(uname -s)" == "Darwin" ]]; then
#     clang -dynamiclib -O3 -o growlibm/libaccelerators.dylib \
#     growlibm/accelerators.c \
#     growlibm/e_rem_pio2.c \
#     growlibm/k_rem_pio2.c \
#     -lm
# else
#     clang -shared -fPIC -O3 -o growlibm/libaccelerators.so growlibm/accelerators.c -lm
# fi

# run regular herbie

# racket -y "src/main.rkt" report \
#         --seed "$SEED" \
#         --platform "vanilla" \
#         --disable "generate:taylor" \
#         "$BENCHDIR" \
#         "$REPORTDIR/vanilla_no_taylor"

# racket -y "src/main.rkt" report \
#         --seed "$SEED" \
#         --platform "vanilla" \
#         --disable "generate:taylor" \
#         --disable "reduce:regimes" \
#         "$BENCHDIR" \
#         "$REPORTDIR/vanilla_no_taylor_regimes"  

# racket -y "src/main.rkt" report \
#         --seed "$SEED" \
#         --platform "herbie20" \
#         --disable "generate:taylor" \
#         "$BENCHDIR" \
#         "$REPORTDIR/herbie20_no_taylor" 

# racket -y "src/main.rkt" report \
#         --seed "$SEED" \
#         --platform "herbie20" \
#         --disable "generate:taylor" \
#         --disable "reduce:regimes" \
#         "$BENCHDIR" \
#         "$REPORTDIR/herbie20_no_taylor_regimes" 

# racket -y "src/main.rkt" report \
#         --seed "$SEED" \
#         --platform "growlibm" \
#         --disable "generate:taylor" \
#         "$BENCHDIR" \
#         "$REPORTDIR/growlibm_no_taylor" 

# racket -y "src/main.rkt" report \
#         --seed "$SEED" \
#         --platform "growlibm" \
#         --disable "generate:taylor" \
#         --disable "reduce:regimes" \
#         "$BENCHDIR" \
#         "$REPORTDIR/growlibm_no_taylor_regimes" 

# racket -y "src/main.rkt" report \
#         --seed "$SEED" \
#         --platform "herbie20plus" \
#         "$BENCHDIR" \
#         "$REPORTDIR/herbie20plus_base" 

# racket -y "src/main.rkt" report \
#         --seed "$SEED" \
#         --platform "herbie20plus" \
#         --disable "generate:taylor" \
#         "$BENCHDIR" \
#         "$REPORTDIR/herbie20plus_no_taylor" 

# racket -y "src/main.rkt" report \
#         --seed "$SEED" \
#         --platform "herbie20plus" \
#         --disable "generate:taylor" \
#         --disable "reduce:regimes" \
#         "$BENCHDIR" \
#         "$REPORTDIR/herbie20plus_no_taylor_regimes" 

# racket -y "src/main.rkt" report \
#         --seed "$SEED" \
#         --platform "growlibmbest" \
#         "$BENCHDIR" \
#         "$REPORTDIR/growlibmbest_base" 

python3 growlibm/evaluate-report.py $REPORTDIR