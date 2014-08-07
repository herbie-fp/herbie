#!/bin/sh

line () { tail $1 -n+$2 | head -n1; }

for i in $1*; do line $i 3; done > $1.im.csv
for i in $1*; do line $i 5; done > $1.if.csv
for i in $1*; do line $i 6; done > $1.id.csv
for i in $1*; do line $i 7; done > $1.il.csv
for i in $1*; do line $i 8; done > $1.om.csv
for i in $1*; do line $i 9; done > $1.of.csv
for i in $1*; do line $i 10; done > $1.od.csv
for i in $1*; do line $i 11; done > $1.ol.csv
