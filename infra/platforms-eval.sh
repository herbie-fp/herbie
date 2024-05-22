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

function run() {
  bench=$1
  key_prefix=$2
  seed=$3

  # "unique" key
  key="$key_prefix-$seed"

  # Generate JSON
  # python3 $INFRA_DIR/platforms-eval.py \
  #  <output directory> \
  #  <benchmark path> \
  #  <unique key> \
  #  <herbie threads> \
  #  <threads>
  python3 $INFRA_DIR/platforms-eval.py \
    "$OUTDIR/platforms" \
    $bench \
    $key \
    $HERBIE_THREADS \
    $THREADS \
    $seed

  # Plot JSON data
  # python3 $INFRA_DIR/platforms/plot.py \
  #  <eval JSON path> \
  #  <output directory>
  python3 $INFRA_DIR/platforms/plot.py \
    $OUTDIR/platforms/output/$key/results.json \
    $OUTDIR/platforms/output/$key
}

# Run configs
run $BENCH_DIR/hamming hamming 100

# clean up cache and build files
if [ -n "$RM_CACHE" ]; then
  echo "removing cache and drivers"
  rm -rf "$OUTDIR/platforms/cache"
  rm -rf "$OUTDIR/platforms/drivers"
fi
