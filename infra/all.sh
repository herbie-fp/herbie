#!/bin/sh

line () { tail $1 -n+$2 | head -n1; }

for i in $1*.out; do
    line $i 1 | paste -d, - "$2/shortnames.csv"
done > $1.names.csv
for i in $1*.out; do line $i 2; done > $1.pf.csv
for i in $1*.out; do line $i 3; done > $1.pd.csv
for i in $1*.out; do line $i 5; done > $1.if.csv
for i in $1*.out; do line $i 6; done > $1.id.csv
for i in $1*.out; do line $i 7; done > $1.of.csv
for i in $1*.out; do line $i 8; done > $1.od.csv
for i in $1*.out; do line $i 9; done > $1.df.csv
for i in $1*.out; do line $i 10; done > $1.dd.csv
