#!/usr/bin/env bash

# exit immediately upon first error
set -e

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

# Herbie path
HERBIE="$MYDIR/../.."

# handle inputs
if [ "$#" -ne 3 ]; then
    echo "Usage: $0 NUM_SEEDS BENCH_PATH RESULT_PATH"
  exit 1
else
    NSEEDS="$1"; shift
    BENCH="$1"; shift
    RESPATH="$1"; shift
fi

echo "Running on benchmarks at $HERBIE/$BENCH"
echo "Writing results to $RESPATH"

# advise user of threads
if [ -z "$THREADS" ]; then
  THREADS="yes"
  echo "Using maximum number of threads"
else
  # support for exporting bash environment to parallel
  echo "Using $THREADS threads for each run"
fi

# advise user of execution plan
if [ -z "$PARALLEL_SEEDS" ]; then
  echo "Using Herbie concurrency only."
else
  # support for exporting bash environment to parallel
  source $(which env_parallel.bash)
  env_parallel --record-env

  echo "Using multiple concurrent Herbie runs in parallel."
  echo "Restricting to $PARALLEL_SEEDS parallel concurrent Herbie runs."
fi

#
#   SAMPLE SEEDS
#

# allocate space for output
tstamp="$(date "+%Y-%m-%d_%H%M")"
output="$(pwd)/$RESPATH/$tstamp"
mkdir -p "$output"

function do_seed {
  seed="$1"

  seed_output="$output/$(printf "%03d" "$seed")"
  mkdir -p "$seed_output"

  racket "$HERBIE/src/herbie.rkt" report \
    --threads $THREADS \
    --seed "$seed" \
    $HERBIE_FLAGS \
    "$HERBIE/$BENCH" \
    "$seed_output"
}

# sample herbie behavior
if [ -z "$PARALLEL_SEEDS" ]; then
  # by default, do not use parallel
  for seed in $(seq "$NSEEDS"); do
    do_seed "$seed"
  done
else
  # conditionally use parallel
  #
  # Note that Herbie can already use up to # of benchmarks cores,
  # so this probably only makes sense if you have PARALLEL_SEEDS
  # set to something less than # of cores divided by # of benchmarks,
  # i.e., you have a lot of cores. We're not at all careful to get
  # solid timing numbers, but going higher any than that will make
  # any time measurements even less meaningful.
  seq "$NSEEDS" \
    | env_parallel \
        --env _ \
        --jobs "$PARALLEL_SEEDS" \
        --halt now,fail=1 \
        do_seed
fi


#
#   COLLECT OUTPUT
#

pushd "$output"

echo "[" > all.json
first=true

for rj in $(find . -name 'results.json' | sort); do
  if $first; then
    first=false
  else
    echo "," >> all.json
  fi

  seed="$(jq '.seed' "$rj")"
  npts="$(jq '.points' "$rj")"
  herbie_iters="$(jq '.iterations' "$rj")"

  # warn about errors and timeouts that will be filtered out

  errors="$(jq '.tests | map(select(.status == "error"))' "$rj")"
  if [ "$errors" != "[]" ]; then
    echo "WARNING: filtering out errors in $rj on seed $seed"
    echo "$errors"
    echo "$seed" >> errors.json
    echo "$errors" >> errors.json
  fi

  timeouts="$(jq '.tests | map(select(.status == "timeout"))' "$rj")"
  if [ "$timeouts" != "[]" ]; then
    echo "WARNING: filtering out timeouts in $rj on seed $seed"
    echo "$timeouts"
    echo "$seed" >> timeouts.json
    echo "$timeouts" >> timeouts.json
  fi

  cat "$rj" \
    | jq --argjson SEED "$seed" \
         --argjson NPTS "$npts" \
         --argjson HERBIE_ITERS "$herbie_iters" \
      '.tests | map(
         select(.status != "error") |
         select(.status != "timeout") |
         { "test" : .name
         , "input" : .input
         , "output" : .output
         , "output_parens" : (.output | [match("[(]"; "g")] | length)
         , "avg_bits_err_input": .start
         , "avg_bits_err_output": .end
         , "avg_bits_err_improve": (.start - .end)
         , "time_improve": .time
         , "seed": $SEED
         , "npts": $NPTS
         , "herbie_iters": $HERBIE_ITERS
         })' \
    >> all.json
done
echo "]" >> all.json

# flatten array of array of results to an array
jq 'flatten' all.json > all.json.tmp
mv all.json.tmp all.json

popd

#
#   PLOT RESULTS
#

$MYDIR/src/plot-results.sh "$output"
