#!/bin/bash
find . -name '*.rkt' | while read line; do
  OUTNAME=${line%rkt}fpcore
	racket ../src/formats/convert.rkt $line > $OUTNAME
done
