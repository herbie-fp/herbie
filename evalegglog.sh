#!/bin/bash

# exit immediately upon first error
set -e -x

# build egg-herbie to make sure updated
cargo build --release --manifest-path=egg-herbie/Cargo.toml

racket src/herbie.rkt report bench/ egglogreport

racket src/herbie.rkt report --egglog-disabled bench/ vanillareport


bash egglogreport.sh
