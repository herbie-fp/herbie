#!/bin/sh

for i in compile/tc*.c; do
    make ${i:0:${#i}-2}.dmax.out
    cat ${i:0:${#i}-2}.dmax.out >> dmax-all.out
done
