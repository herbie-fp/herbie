#!/bin/bash

echo "Cloning Baseline (Herbie 2.0)..."

git clone --depth 1 --branch v2.0 git@github.com:herbie-fp/herbie.git

mkdir -p $2/

cd herbie

echo "Building Baseline (Herbie 2.0)..."
make install

echo "Running Baseline (Herbie 2.0) on benchmarks..."
herbie report bench/$1 tmp

cd ..

mv herbie/tmp/results.json $2/

rm -rf herbie
