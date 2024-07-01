#!/usr/bin/env bash

# determine physical directory of this script
src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"

# caller should pass path to output from sampler
cd "$1"

if [ ! -f "all.json" ]; then
  echo "ERROR: no 'all.json' in '$1'"
  exit 1
fi

# munge and plot results
jq 'group_by(.seed)' all.json > by-seed.json
jq 'group_by(.test)' all.json > by-test.json

function plot-seed-field {
  field="$1"

  cat by-seed.json \
    | jq --arg FIELD "$field" \
        'map({ "seed": .[0].seed
             , "data": map(.[$FIELD]) | add
             })' \
    > "by-seed-${field}.json"

  python3 "$MYDIR/seed-violin-plot.py" \
    "by-seed-${field}.json" \
    "$field"

  python3 "$MYDIR/seed-bar-chart.py" \
    "by-seed-${field}.json" \
    "$field"
}

function plot-test-field {
  field="$1"

  cat by-test.json \
    | jq --arg FIELD "$field" \
        'map({ "test": .[0].test
             , "data": map(.[$FIELD])
             })' \
    > "by-test-${field}.json"

  python3 "$MYDIR/test-violin-plot.py" \
    "by-test-${field}.json" \
    "$field"
}

function plot-test-versus {
  x="$1"
  y="$2"

  cat by-test.json \
    | jq --arg X "$x" --arg Y "$y" \
        'map({ "test": .[0].test
             , "x": map(.[$X])
             , "y": map(.[$Y])
             })' \
    > "by-test-${x}-versus-${y}.json"

  python3 "$MYDIR/test-versus-plot.py" \
    "by-test-${x}-versus-${y}.json" \
    "$x" "$y"
}


fields="
  output_parens
  avg_bits_err_input
  avg_bits_err_output
  avg_bits_err_improve
  time_improve"

for f in $fields; do
  plot-seed-field "$f"
done

for f in $fields; do
  plot-test-field "$f"
done

mkdir "versus-plots"
plot-test-versus "time_improve"  "avg_bits_err_output"
plot-test-versus "time_improve"  "output_parens"
plot-test-versus "output_parens" "avg_bits_err_output"
## plot-test-versus "time_improve"  "avg_bits_err_improve"
## plot-test-versus "output_parens" "avg_bits_err_improve"
