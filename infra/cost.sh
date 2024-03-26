#!/bin/bash

# exit immediately upon first error
set -e -x

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done

INFRA_DIR="$(cd -P "$(dirname "$src")" && pwd)"
BENCH_DIR="$INFRA_DIR"/../bench
HERBIE_THREADS=4
THREADS=4

# check arguments
if [ -z "$1" ]; then
  echo "Usage: $0 <output_dir>"
  exit 1
else
  OUTDIR="$(pwd)/$1"
fi

echo "Running platforms evaluation"
python3 $INFRA_DIR/platforms-eval.py \
  "$OUTDIR/platforms" $HERBIE_THREADS $THREADS \
  "c $BENCH_DIR/hamming 2500 default" \
  "c $BENCH_DIR/physics 2500 default" \
  "python $BENCH_DIR/physics 2500 c" \
