#!/bin/bash

echo "Converting user submitted data into benchmark suite"
rm -rf "bench/demo"
mkdir "bench/demo"
racket infra/convert-demo.rkt "bench/demo" "infra/v10.json" "infra/v11.json" "infra/v12.json" "infra/v13.json"

racket infra/sort-fpcore.rkt "bench/demo/v10.fpcore" "bench/demo/v10-s.fpcore"
racket infra/sort-fpcore.rkt "bench/demo/v11.fpcore" "bench/demo/v11-s.fpcore"
racket infra/sort-fpcore.rkt "bench/demo/v12.fpcore" "bench/demo/v12-s.fpcore"
racket infra/sort-fpcore.rkt "bench/demo/v13.fpcore" "bench/demo/v13-s.fpcore"
rm bench/demo/v10.fpcore bench/demo/v11.fpcore bench/demo/v12.fpcore bench/demo/v13.fpcore