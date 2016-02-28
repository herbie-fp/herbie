#!/usr/bin/env bash

# this is where herbie lives on warfa for nightlies
# you may change HERBROOT to test locally,
#   but do not push any changes to HERBROOT!
HERBROOT="$HOME/herbie"

# example crontab entry for nightlies
# 30 2 * * * $HOME/herbie/bot/run.sh

cd "$HERBROOT"
git pull --quiet

make --quiet --directory=randTest
java -classpath randTest/ RandomTest \
  --size  5 --size-wiggle  5 \
  --nvars 1 --nvars-wiggle 3 \
  --ntests 20 \
  > "$HERBROOT/bench/random.rkt"

function run {
  bench=$1; shift
  name=$1;  shift
  time xvfb-run --auto-servernum \
    racket herbie/reports/run.rkt \
      --note "$name" \
      --profile \
      --threads 4 \
      "$@" \
      "$bench"
  make --directory="$HERBROOT/graphs" overhead
  make --directory="$HERBROOT" publish
}

function runEach {
  for b in $HERBROOT/bench/*; do
    name=$(basename "$b" .rkt)
    # add cases to skip large or misbehaving benchmarks
    case $name in
      SKIP)
        continue
        ;;
    esac
    LOG="$HERBROOT/bot/$name-$(date +%y%m%d%H%M%S).log"
    ln -sf "$LOG" "$HERBROOT/bot/latest.log"
    run "$b" "$name" "$@" &> "$LOG"
  done
}

# on some machines, this will cause Racket VM to run out of memory
function runAll {
  b="$HERBROOT/bench"
  name="all"
  LOG="$HERBROOT/bot/$name-$(date +%y%m%d%H%M%S).log"
  ln -sf "$LOG" "$HERBROOT/bot/latest.log"
  run "$b" "$name" "$@" &> "$LOG"
}

runEach
runEach --option rules:numerics
runEach --option precision:double
runEach --option rules:numerics --option precision:double
