#!/bin/bash

# exit immediately upon first error, log every command executed
set -e -x

# Ensure egglog is in the path
export PATH="$HOME/.cargo/bin/:$PATH"
rustup update

# Keep nightly installs isolated and consistent across install/run steps.
export PLTADDONDIR="${PLTADDONDIR:-pltlibs}"
make install

# Seed is fixed for the whole day; both backends run the same seed
SEED=$(date "+%Y%j")
BRANCH=$(git rev-parse --abbrev-ref HEAD)

BENCHDIR="$1"; shift
REPORTDIR="$1"; shift

if [[ "$BRANCH" == rival2-* || "$BRANCH" == "rival2" ]]; then
  set -- "$@" --enable setup:rival2
fi

mkdir -p "$REPORTDIR"
rm -rf "reports"/* || echo "nothing to delete"

run_backend () {
  local label="$1"; shift
  local outdir="$REPORTDIR/$label"
  mkdir -p "$outdir"

  local dirs=""
  for bench in "$BENCHDIR"/*; do
    local name
    name=$(basename "$bench" .fpcore)
    rm -rf "$outdir/$name"

    racket -y "src/main.rkt" report \
           --seed "$SEED" \
           "$@" \
           "$bench" "$outdir/$name"

    dirs="$dirs $name"
  done

  racket -y infra/merge.rkt "$outdir" $dirs
}

# egg backend (baseline)
run_backend egg "$@"

# egglog backend
run_backend egglog "$@" --enable generate:egglog

# produce side-by-side comparison
mkdir -p "$REPORTDIR/compare"
racket -y infra/compare-backends.rkt "$REPORTDIR/egg" "$REPORTDIR/egglog" "$REPORTDIR/compare"
