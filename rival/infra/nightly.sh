#!/bin/bash
set -e -x

function setup {
  raco pkg install --auto biginterval
  raco pkg install --auto fpbench
  rm -rf ./herbie
  git clone https://github.com/uwplse/herbie
}

function generate-points {
  racket -y "infra/generate-points.rkt" "./herbie/bench" "./infra/all-points.txt"
}

REPORTDIR="report"
MPFI_DATA="$REPORTDIR/mpfi-results.txt"
MATH_DATA="$REPORTDIR/mathematica-output.txt"
RIVAL_DATA="$REPORTDIR/rival-output.txt"

function clean {
  if [ -d "$REPORTDIR" ]; then
    rm -r "$REPORTDIR"
  fi
  mkdir -p "$REPORTDIR"
}

function run-mpfi {
  echo "running mpfi on generated points"
  racket -y "infra/run-mpfi.rkt" "infra/all-points.txt" "$1"
}

function run-mathematica {
  echo "Converting points to mathematica script"
  rm "report/mathematica-output.txt"  
  racket -y "infra/run-mathematica.rkt" "infra/all-points.txt" "$REPORTDIR/mathematica-input.txt" "$MATH_DATA" "$RIVAL_DATA"
}

function format-data {
  echo "Formatting the mpfi and mathematica data into latex table"
  racket -y "infra/report.rkt" "$MPFI_DATA" "$MATH_DATA" "$RIVAL_DATA" "$REPORTDIR/index.html" "$REPORTDIR/examples.txt" "$REPORTDIR/macros.txt"
  cp "infra/index.css" "$REPORTDIR"
}

function rerun {
  clean
  generate-points
  run-mpfi "$MPFI_DATA"
  run-mathematica
  format-data
  gzip -9 "$MPFI_DATA" "$MATH_DATA" "$RIVAL_DATA"
}

function all {
  setup
  rerun
}

function perf {
    clean
    xz -d -k -f infra/points.json.xz
    racket -y time.rkt --dir "$REPORTDIR" --profile profile.json infra/points.json
    python3 infra/ratio_plot.py -t "$REPORTDIR"/timeline.json -o "$REPORTDIR"
    python3 infra/point_graph.py -t "$REPORTDIR"/timeline.json -o "$REPORTDIR"
    python3 infra/histograms.py -t "$REPORTDIR"/timeline.json -o "$REPORTDIR"
    python3 infra/cnt_per_iters_plot.py -t "$REPORTDIR"/timeline.json -o "$REPORTDIR"
    python3 infra/repeats_plot.py -t "$REPORTDIR"/timeline.json -o "$REPORTDIR"
    python3 infra/density_plot.py -t "$REPORTDIR"/timeline.json -o "$REPORTDIR"
    cp profile.json "$REPORTDIR"/profile.json
    cp profile.js "$REPORTDIR"/profile.js
}

function bench {
    racket -y time.rkt --dir "$REPORTDIR" --profile profile.json --id $id
    python3 infra/ratio_plot.py -t "$REPORTDIR"/timeline.json -o "$REPORTDIR"
    python3 infra/point_graph.py -t "$REPORTDIR"/timeline.json -o "$REPORTDIR"
    python3 infra/histograms.py -t "$REPORTDIR"/timeline.json -o "$REPORTDIR"
    python3 infra/cnt_per_iters_plot.py -t "$REPORTDIR"/timeline.json -o "$REPORTDIR"
    python3 infra/repeats_plot.py -t "$REPORTDIR"/timeline.json -o "$REPORTDIR"
    python3 infra/density_plot.py -t "$REPORTDIR"/timeline.json -o "$REPORTDIR"
    cp profile.json "$REPORTDIR"/profile.json
    cp profile.js "$REPORTDIR"/profile.js
}

echo "Running $1"

if [ "$2" ]; then
  id=$2
fi

$1
