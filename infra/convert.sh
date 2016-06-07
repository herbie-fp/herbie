#!/bin/bash
set -e

DIR=$1
if [[ -z "$DIR" ]]; then
    DIR=.
fi

find "$DIR" -name '*.rkt' | while read line; do
  OUTNAME=${line%rkt}fpcore
  echo "Converting $line"
  racket infra/convert.rkt "$line" > "$OUTNAME"
done
