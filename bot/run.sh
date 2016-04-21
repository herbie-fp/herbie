#!/usr/bin/env bash

# this is where herbie lives on warfa for nightlies
# you may change HERBROOT to test locally,
#   but do not push any changes to HERBROOT!
HERBROOT="$HOME/herbie"

# example crontab entry for nightlies
# 30 2 * * * $HOME/herbie/bot/run.sh

CORES=4

LOG="$HERBROOT/bot/$(date +%y%m%d%H%M%S).log"
ln -sf "$LOG" "$HERBROOT/bot/latest.log"

EXC="$HERBROOT/bot/exceptions-$(date +%y%m%d%H%M%S).rkt"
ln -sf "$EXC" "$HERBROOT/bot/latest-exceptions.rkt"

function main {
  cd "$HERBROOT"
  git pull --quiet

  make --quiet --directory="$HERBROOT/randTest"
  java -classpath "$HERBROOT/randTest/" RandomTest \
    --size  5 --size-wiggle  5 \
    --nvars 1 --nvars-wiggle 3 \
    --ntests 20 \
    > "$HERBROOT/bench/random.rkt"

  # choose configs based on day of year
  d=$(date "+%j")

  # use common seed across every 4 day cycle
  qseed=$(racket -e " \
    (random-seed $(expr $d / 4)) \
    (pseudo-random-generator->vector \
      (current-pseudo-random-generator))")
  seed="${qseed:1}" # :1 removes leading quote

  # toggle fuel every two days
  if [ $(expr \( $d / 2 \) % 2) -eq 0 ]; then
    fuel="--fuel 2"
  else
    fuel="--fuel 3"
  fi

  # toggle regimes every other day
  if [ $(expr $d % 2) -eq 0 ]; then
    regime=""
  else
    regime="--option reduce:regimes"
  fi

  # toggle some configs every day
  for prec in "" "--option precision:double"; do
    for num in "" "--option rules:numerics"; do
      runEach --seed "$seed" $fuel $regime $prec $num
    done
  done
}

function run {
  bench=$1; shift
  name=$1;  shift
  cat << EOF

================================================================================
                                   $name
================================================================================

run $@

EOF
  time xvfb-run --auto-servernum \
    racket "$HERBROOT/herbie/reports/run.rkt" \
      --note "$name" \
      --profile \
      --threads $CORES \
      "$@" \
      "$bench"
  cat << EOF >> "$EXC"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EOF
  cat "$HERBROOT/graphs/exceptions.rkt" >> "$EXC"
  echo
  echo "Evaluating extracted C"
  time make \
    --quiet --directory="$HERBROOT/graphs" \
    --jobs=$CORES \
    overhead
  echo
  echo "Publishing to uwplse.org"
  # ignore verbose rsync output and shell trace
  time make \
    --quiet --directory="$HERBROOT" \
    publish 2>&1 > /dev/null | grep -v '^+'
}

function runEach {
  for bench in $HERBROOT/bench/*; do
    name=$(basename "$bench" .rkt)
    # add cases to skip large or misbehaving benchmarks
    case $name in
      haskell|random)
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

main &> "$LOG"
