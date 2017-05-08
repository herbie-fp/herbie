#!/usr/bin/env bash

# This is where herbie lives on warfa for nightlies.
# You may change HERBROOT to test locally,
#   but do not push any changes to HERBROOT!
HERBROOT="$HOME/herbie"

# example crontab entry for nightlies
# 30 2 * * * $HOME/herbie/infra/run.sh

CORES=4

LOG="$HERBROOT/infra/$(date +%y%m%d%H%M%S).log"
ln -sf "$LOG" "$HERBROOT/infra/latest.log"

EXC="$HERBROOT/infra/exceptions-$(date +%y%m%d%H%M%S).rkt"
ln -sf "$EXC" "$HERBROOT/infra/latest-exceptions.rkt"

function main {
  cd "$HERBROOT"
  git pull --quiet

##  make --quiet --directory="$HERBROOT/randTest"
##  java -classpath "$HERBROOT/randTest/" RandomTest \
##    --size  5 --size-wiggle  5 \
##    --nvars 1 --nvars-wiggle 3 \
##    --ntests 20 \
##    > "$HERBROOT/bench/random.fpcore"

  # choose configs based on day of year
  d=$(date "+%j")

  # use common seed across every 4 day cycle
  qseed=$(racket -e " \
    (random-seed $(expr $d / 4)) \
    (pseudo-random-generator->vector \
      (current-pseudo-random-generator))")
  seed="${qseed:1}" # :1 removes leading quote

##  # toggle fuel every two days
##  if [ $(expr \( $d / 2 \) % 2) -eq 0 ]; then
##    fuel="--fuel 2"
##  else
##    fuel="--fuel 3"
##  fi

##  # toggle regimes every other day
##  if [ $(expr $d % 2) -eq 0 ]; then
##    regime=""
##  else
##    regime="--disable reduce:regimes"
##  fi

##  # toggle some configs every day
##  for prec in "" "--disable precision:double"; do
##    for postproc in "" "--enable reduce:post-process"; do
##      for num in "" "--enable rules:numerics"; do
##        runEach --seed "$seed" $fuel $regime $prec $postproc $num
##      done
##    done
##  done

  runEach --seed "$seed"
}

function run {
  bench=$1; shift
  name=$1; shift

  GRAPHS="$HERBROOT/infra/graphs-$(date +%y%m%d%H%M%S)"
  mkdir -p "$GRAPHS"

  cat << EOF

================================================================================
                                   $name
================================================================================

run $@

EOF
  time xvfb-run --auto-servernum \
    racket "$HERBROOT/src/herbie.rkt" report \
      --note "$name" \
      --profile \
      --threads $CORES \
      "$@" \
      "$bench" "$GRAPHS"
  cat << EOF >> "$EXC"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EOF
  cat "$GRAPHS/exceptions.rkt" >> "$EXC"

##  echo
##  echo "Evaluating extracted C"
##  time make \
##    --quiet --directory="$HERBROOT/graphs" \
##    --jobs=$CORES \
##    overhead
##  echo

  echo
  echo "Publishing to uwplse.org"
  # NOTE: the trailing slash at the end of GRAPHS is required for rsync!
  time "$HERBROOT/infra/publish.sh" upload "$GRAPHS/"

  echo
  echo "Rebuilding reports index"
	time "$HERBROOT/infra/publish.sh" index

  rm -rf "$GRAPHS"
}

function runEach {
  ##for bench in $HERBROOT/bench/*; do
  for bench in $HERBROOT/bench/hamming; do
    name=$(basename "$bench" .fpcore)
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
