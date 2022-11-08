#!/bin/bash

# exit immediately upon first error
set -e -x

if [ -d "egglogdata" ]; then rm -Rf egglogdata; fi

mkdir egglogdata

python3 egglogplot.py "vanillareport/results.json" "egglogreport/results.json" "egglogdata/error.pdf" "egglogdata/macros.txt"
