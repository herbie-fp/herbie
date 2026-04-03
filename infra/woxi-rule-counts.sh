#!/usr/bin/env bash

set -euo pipefail

src="${BASH_SOURCE[0]}"
while [ -L "$src" ]; do
  dir="$(cd -P "$(dirname "$src")" && pwd)"
  src="$(readlink "$src")"
  [[ $src != /* ]] && src="$dir/$src"
done
MYDIR="$(cd -P "$(dirname "$src")" && pwd)"
HERBIE="$MYDIR/.."

if [ "$#" -gt 1 ]; then
  echo "Usage: $0 [RULES_JSON]" >&2
  exit 1
fi

RULES_FILE="${1:-$HERBIE/rules.json}"

if [ ! -f "$RULES_FILE" ]; then
  echo "ERROR: no rules file at '$RULES_FILE'" >&2
  exit 1
fi

cat <<'EOF'
| Rule | Rewrite | Count |
| --- | --- | ---: |
EOF

jq -r '
  def emit($rule; $rewrite):
    if has($rule)
    then [$rule, $rewrite, .[$rule]] | @tsv
    else empty
    end;

  emit("erf-0"; "(erf 0) -> 0"),
  emit("erf-neg"; "(erf (neg x)) -> (neg (erf x))"),
  emit("erf-neg-rev"; "(neg (erf x)) -> (erf (neg x))"),
  emit("erfc-0"; "(erfc 0) -> 1"),
  emit("asin-0"; "(asin 0) -> 0"),
  emit("asin-1"; "(asin 1) -> (/ (PI) 2)"),
  emit("asin--1"; "(asin -1) -> (neg (/ (PI) 2))"),
  emit("asin-1/2"; "(asin 1/2) -> (/ (PI) 6)"),
  emit("asin--1/2"; "(asin -1/2) -> (neg (/ (PI) 6))"),
  emit("asin-sqrt2/2"; "(asin (/ (sqrt 2) 2)) -> (/ (PI) 4)"),
  emit("asin--sqrt2/2"; "(asin (/ (neg (sqrt 2)) 2)) -> (neg (/ (PI) 4))"),
  emit("asin-sqrt3/2"; "(asin (/ (sqrt 3) 2)) -> (/ (PI) 3)"),
  emit("asin--sqrt3/2"; "(asin (/ (neg (sqrt 3)) 2)) -> (neg (/ (PI) 3))"),
  emit("acos-1"; "(acos 1) -> 0"),
  emit("acos-0"; "(acos 0) -> (/ (PI) 2)"),
  emit("acos--1"; "(acos -1) -> (PI)"),
  emit("acos-1/2"; "(acos 1/2) -> (/ (PI) 3)"),
  emit("acos--1/2"; "(acos -1/2) -> (/ (* 2 (PI)) 3)"),
  emit("acos-sqrt2/2"; "(acos (/ (sqrt 2) 2)) -> (/ (PI) 4)"),
  emit("acos--sqrt2/2"; "(acos (/ (neg (sqrt 2)) 2)) -> (/ (* 3 (PI)) 4)"),
  emit("acos-sqrt3/2"; "(acos (/ (sqrt 3) 2)) -> (/ (PI) 6)"),
  emit("acos--sqrt3/2"; "(acos (/ (neg (sqrt 3)) 2)) -> (/ (* 5 (PI)) 6)"),
  emit("atan-0"; "(atan 0) -> 0"),
  emit("atan-1"; "(atan 1) -> (/ (PI) 4)"),
  emit("atan--1"; "(atan -1) -> (neg (/ (PI) 4))"),
  emit("sec-PI/6"; "(sec (/ (PI) 6)) -> (/ 2 (sqrt 3))"),
  emit("sec-PI/4"; "(sec (/ (PI) 4)) -> (sqrt 2)"),
  emit("sec-PI/3"; "(sec (/ (PI) 3)) -> 2"),
  emit("csc-PI/2"; "(csc (/ (PI) 2)) -> 1"),
  emit("csc-PI/6"; "(csc (/ (PI) 6)) -> 2"),
  emit("csc-PI/4"; "(csc (/ (PI) 4)) -> (sqrt 2)"),
  emit("csc-PI/3"; "(csc (/ (PI) 3)) -> (/ 2 (sqrt 3))"),
  emit("cot-PI/2"; "(cot (/ (PI) 2)) -> 0"),
  emit("cot-PI/6"; "(cot (/ (PI) 6)) -> (sqrt 3)"),
  emit("cot-PI/4"; "(cot (/ (PI) 4)) -> 1"),
  emit("cot-PI/3"; "(cot (/ (PI) 3)) -> (/ 1 (sqrt 3))"),
  emit("scaled-cos-sin-sum"; "(+ (* a (* (cos x) (cos x))) (* a (* (sin x) (sin x)))) -> a"),
  emit("asinh-0"; "(asinh 0) -> 0"),
  emit("acosh-1"; "(acosh 1) -> 0"),
  emit("atanh-0"; "(atanh 0) -> 0")
' "$RULES_FILE" | while IFS=$'\t' read -r rule rewrite count; do
  printf '| `%s` | `%s` | %s |\n' "$rule" "$rewrite" "$count"
done
