#!/bin/bash
# RUN this from herbie root!
for power in `seq 2 11`; do
    points=`awk 'BEGIN{print 2^'$power'}'`
    racket reports/make-report.rkt -s $points bench/hamming/
    racket compile/results-to-csv.rkt graphs/results.herbie.dat graphs/results.herbie.csv
    awk 'BEGIN{print '$points',","} \
{ totalPointsRecovered += $5; totalTime += $6} \
END {print totalPointsRecovered, ",", totalTime}' graphs/results.herbie.csv
done > test/num-points-test-results.csv
