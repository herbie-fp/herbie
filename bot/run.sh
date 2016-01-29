#!/usr/bin/env bash

HERBROOT="$HOME/herbie"

# example crontab entry for nightlies
# 30 2 * * * $HOME/herbie/bot/run.sh

cd $HERBROOT
git pull --quiet

function run {
  time xvfb-run --auto-servernum \
    racket herbie/reports/run.rkt \
      --profile \
      --note "$2" \
      "$1"
  make publish
}

for b in $HERBROOT/bench/*; do
  name=$(basename "$b" .rkt)
  # skip some massive or misbehaving benchmarks
  case $name in
    haskell|mathematics|numerical-analysis|regression)
      continue
      ;;
  esac
  LOG="$HERBROOT/bot/$name-$(date +%y%m%d%H%M%S).log"
  ln -sf "$LOG" "$HERBROOT/bot/latest.log"
  run "$b" "$name" &> "$LOG"
done
