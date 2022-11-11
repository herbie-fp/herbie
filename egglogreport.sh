#!/bin/bash

# exit immediately upon first error
set -e -x

if [ -d "egglogdata" ]; then rm -Rf egglogdata; fi

mkdir egglogdata

VANILLA=vanillareport
EGGLOG=egglogreport

python3 egglogplot.py "$VANILLA/results.json" "$EGGLOG/results.json" "egglogdata/error.pdf" "egglogdata/macros.tex" "egglogdata"
