#!/bin/bash

# exit immediately upon first error
set -e -x

# build egg-herbie to make sure updated
cargo build --release --manifest-path=egg-herbie/Cargo.toml

SEED=0

racket src/herbie.rkt report --seed "$SEED" bench/hamming egglogreport

racket src/herbie.rkt report --seed "$SEED" --egglog-disabled bench/hamming vanillareport


bash egglogreport.sh
