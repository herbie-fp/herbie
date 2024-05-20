#!/bin/bash

# exit immediately upon first error
set -e -x

src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done

INFRA_DIR="$(cd -P "$(dirname "$src")" && pwd)"
BENCH_DIR="$INFRA_DIR"/../bench


if [ -z "$1" ]; then
  echo "Usage: $0 <output_dir>"
  exit 1
else
  WORKING_DIR="$1";
fi

# check arguments
if [ -z "$2" ]; then
  echo "Usage: $0 <output_dir>"
  exit 1
fi

INPUT_FILE="$BENCH_DIR/$2"
# check arguments
if [ -z "$3" ]; then
  echo "Usage: $0 <output_dir>"
  exit 1
else
  OUT_DIR="$3";
fi

mkdir -p $WORKING_DIR/

mkdir -p $OUT_DIR/

cd $WORKING_DIR

echo "Cloning Baseline (Herbie 2.0)..."

git clone --depth 1 --branch v2.0 git@github.com:herbie-fp/herbie.git

echo "Building Baseline (Herbie 2.0)..."

cd herbie

make install

cd ../..

echo "Running Baseline (Herbie 2.0) on benchmarks..."

herbie report $INPUT_FILE $WORKING_DIR/graphs

mv $WORKING_DIR/graphs/results.json $OUT_DIR/

rm -rf $WORKING_DIR
