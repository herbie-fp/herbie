#!/bin/bash

# exit immediately upon first error, log every command executed
set -e -x

# Seed is fixed for the whole day; this way two branches run the same seed
SEED=$(date "+%Y%j")
BENCHDIR="$1"; shift
REPORTDIR="$1"; shift

mkdir -p reports
rm -rf "reports"/* || echo "nothing to delete"

# run
dirs=""
for bench in bench/*; do
  name=$(basename "$bench" .fpcore)
  rm -rf "reports/$name"

  racket -y "src/main.rkt" report \
         --note "$name" \
         --seed "$SEED" \
         "$@" \
         "$bench" "reports/$name"
  
  dirs="$dirs $name";
done

# merge reports
racket -y infra/merge.rkt --name "$(basename bench .fpcore)" "reports" $dirs

