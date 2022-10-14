#!/bin/bash

# exit immediately upon first error
set -e -x


racket src/herbie.rkt report bench/hamming egglogreport

racket src/herbie.rkt report --egglog-disabled bench/hamming vanillareport


bash egglogreport.sh
