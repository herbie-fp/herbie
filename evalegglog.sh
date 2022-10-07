#!/bin/bash

# exit immediately upon first error
set -e -x


racket src/herbie.rkt report bench/ egglogreport

racket src/herbie.rkt report --egglog-disabled bench/ vanillareport


bash egglogreport.sh
