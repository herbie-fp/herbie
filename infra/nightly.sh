#!/bin/bash

# exit immediately upon first error
set -e -x

CORES=6
SEED=$(date "+%Y%j")

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done

INFRA_DIR="$(cd -P "$(dirname "$src")" && pwd)"
BENCH_DIR="$INFRA_DIR"/../bench

# check arguments
if [ -z "$1" ]; then
  echo "Usage: $0 <output_dir>"
  exit 1
else
  OUT_DIR="$1"; shift
  FLAGS="$@"
fi

# run
RECURSE=1 LOG=1 \
  bash "$INFRA_DIR"/run.sh \
    "$BENCH_DIR" "$OUT_DIR" \
    --profile \
    --seed "$SEED" \
    --threads "$CORES" \
    $FLAGS

# upload
if [ "$?" -eq 0 ]; then
  bash $INFRA_DIR/publish.sh upload "$OUT_DIR"
fi

