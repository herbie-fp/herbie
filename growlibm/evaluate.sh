#!/bin/bash

# exit immediately upon first error, log every command executed
set -e -x

# Ensure egglog is in the path
export PATH="$HOME/.cargo/bin/:$PATH"
rustup update

# Keep nightly installs isolated and consistent across install/run steps.
export PLTADDONDIR="${PLTADDONDIR:-pltlibs}"
make install

# Seed is fixed for the whole day; this way two branches run the same seed
# SEED=$(date "+%Y%j")
SEED="2026068"
BENCHDIR="$1"
REPORTDIR="$2"
PLATFORM="$3"
NUM_ENODES="32000"

mkdir -p "$REPORTDIR"
mkdir -p "$REPORTDIR/$PLATFORM"

racket -y src/main.rkt report \
        --seed $SEED \
        --platform $PLATFORM \
        --num-enodes $NUM_ENODES \
        $BENCHDIR \
        $REPORTDIR/$PLATFORM/growlibm_base

# racket -y src/main.rkt report \
#         --seed "$SEED" \
#         --platform c \
#         --num-enodes $NUM_ENODES \
#         $BENCHDIR \
#         $REPORTDIR/$PLATFORM/herbie20_base

racket -y src/main.rkt report \
        --seed $SEED \
        --platform vanilla \
        --num-enodes $NUM_ENODES \
        $BENCHDIR \
        $REPORTDIR/$PLATFORM/vanilla_base

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

python3 growlibm/evaluate-report.py "$REPORTDIR/$PLATFORM" "$PLATFORM"
python3 growlibm/evaluate-frontier.py "$REPORTDIR/$PLATFORM" "$PLATFORM"
python3 growlibm/evaluate-best-alt-bars.py "$REPORTDIR/$PLATFORM" "$PLATFORM"
