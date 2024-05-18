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

# Run Hamming evaluation
python3 $INFRA_DIR/platforms-eval.py \
  "$OUTDIR/platforms" \
  "$BENCH_DIR/hamming/" \
  hamming \
  $HERBIE_THREADS \
  $THREADS

# clean up cache and build files
rm -rf "$OUTDIR/platforms/cache"
rm -rf "$OUTDIR/platforms/drivers"
