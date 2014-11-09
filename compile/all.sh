#!/bin/sh

line () { tail $1 -n+$2 | head -n1; }

for i in $1*.out; do
    line $i 1 | cut -c 4- | paste -d, - shortnames.csv
done > $1.names.csv
for i in $1*.out; do line $i 3; done > $1.if.csv
for i in $1*.out; do line $i 4; done > $1.id.csv
for i in $1*.out; do line $i 5; done > $1.of.csv
for i in $1*.out; do line $i 6; done > $1.od.csv
