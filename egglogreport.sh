#!/bin/bash

# exit immediately upon first error
set -e -x

if [ -d "egglogreport" ]; then rm -Rf egglogreport; fi

mkdir egglogreport

python3 egglogplot.py vanillareport/results.json egglogreport/results.json