#!/usr/bin/env bash

# this is where herbie lives on warfa for nightlies
# you may change HERBROOT to test locally,
#   but do not push any changes to HERBROOT!
HERBROOT="$HOME/herbie"

CORES=4

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
      --threads $CORES \
      "$@" \
      "$bench"
  time make \
    --directory="$HERBROOT/graphs" \
    --jobs=$CORES \
    overhead
  make --quiet --directory="$HERBROOT" publish
}

function runEach {
  for bench in $HERBROOT/bench/*; do
    name=$(basename "$bench" .rkt)
    # add cases to skip large or misbehaving benchmarks
    case $name in
      SKIP)
        continue
        ;;
    esac
    run "$bench" "$name" "$@"
  done
}

# on some machines, this will cause Racket VM to exhaust memory
function runAll {
  bench="$HERBROOT/bench"
  name="all"
  run "$bench" "$name" "$@"
}

function main {
  for prec in "" "--option precision:double"; do
    for num in "" "--option rules:numerics"; do
      runEach $prec $num
    done
  done
}

LOG="$HERBROOT/bot/$(date +%y%m%d%H%M%S).log"
ln -sf "$LOG" "$HERBROOT/bot/latest.log"
main &> "$LOG"
