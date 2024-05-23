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
THREADS=4

# check arguments
if [ -z "$1" ]; then
  echo "Usage: $0 <output_dir>"
  exit 1
else
  OUTDIR="$(pwd)/$1"
fi

# advise user of execution plan
if [ -z "$PARALLEL_SEEDS" ]; then
  echo "Using Herbie concurrency only."
  PARALLEL_SEEDS=1
else
  # support for exporting bash environment to parallel
  echo "Using multiple concurrent Herbie runs in parallel."
  echo "Restricting to $PARALLEL_SEEDS parallel concurrent Herbie runs."
fi

echo "Running platforms evaluation"

function run() {
  bench=$1
  key=$2
  num_runs=$3

  # Generate JSON
  python3 $INFRA_DIR/platforms-eval.py \
    --key $key \
    --parallel $PARALLEL_SEEDS \
    --threads $THREADS \
    $bench \
    "$OUTDIR/platforms" \
    $num_runs

  # Plot JSON data
  # python3 $INFRA_DIR/platforms/plot.py \
  #  <eval JSON path> \
  #  <output directory>
  # python3 $INFRA_DIR/platforms/plot.py \
  #   $OUTDIR/platforms/output/$key/results.json \
  #   $OUTDIR/platforms/output/$key
}

# Run configs
run $BENCH_DIR/hamming hamming 4

# clean up cache and build files
if [ -n "$RM_CACHE" ]; then
  echo "removing cache and drivers"
  rm -rf "$OUTDIR/platforms/cache"
  rm -rf "$OUTDIR/platforms/drivers"
fi
