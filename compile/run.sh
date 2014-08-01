#!/bin/sh

echo "Compiling"

for f in tc*.c; do
    make test_${f%.c} >/dev/null
done

for f in tc*.c; do
    echo Running $f
    ./test_${f%.c} 1000000 > ${f%.c}.out
done
