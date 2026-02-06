# Batch-Related Functions in Herbie

## Core Batch Functions (src/core/batch.rkt)

### Conversion Functions
- `progs->batch` — List<Expr> → (Batch, List<Batchref>)
- `batch->progs` — Batch → List<Batchref> → List<Expr>

### Struct and Accessors
- `batch` (struct) — nodes, index fields
- `batchref` (struct) — batch, idx fields
- `deref` — Batchref → Expr

### Batch Construction
- `batch-empty` — creates empty batch
- `batch-push!` — push term to batch (with hashcons)
- `batch-add!` — Batch → (Expr | Batchref | Expr<Batchref>) → Batchref
- `batch-copy` — Batch → Batch
- `batch-copy-only` — Batch → List<Batchref> → (Batch, List<Batchref>)
- `batch-copy-only!` — copy from one batch to another

### Batch Queries
- `batch-length` — Batch → Integer
- `batch-tree-size` — Batch → List<Batchref> → Integer
- `batch-free-vars` — Batch → (Batchref → Set<Var>)
- `batch-ref` — Batch → Idx → Node
- `batch-get-nodes` — returns nodes as vector
- `in-batch` — Batch → Sequence<Node>

### Batch Traversal
- `batch-recurse` — memoized recursive traversal
- `batch-iterate` — simpler variant without extra args
- `batch-exprs` — constructs expressions from batch nodes
- `batch-pull` — Batchref → Expr (fully expands)
- `batch-reachable` — find reachable nodes given condition

### Batch Transformation
- `batch-apply` — Batch → List<Batchref> → (Expr<Batchref> → Expr<Batchref>) → (Batch, List<Batchref>)
- `batch-apply!` — in-place modification

### Batchref Comparisons
- `batchref<?`
- `batchref<=?`
- `batchref>?`
- `batchref>=?`
- `batchref=?`

### Expression Recursion
- `expr-recurse` — defines recursive structure of expressions

---

## Batch Reduce (src/core/batch-reduce.rkt)

- `batch-reduce` — main reducer (symbolic simplification)
- `batch-eval-application` — evaluate constant expressions
- `batch-gather-multiplicative-terms` — internal helper

---

## Programs Module (src/core/programs.rkt)

- `batch-reprs` — compute representations for batch nodes
- `batch-replace-expression!` — replace expression in batch
- `batch-get-locations` — get locations of sub-batchref
- `batch-location-set` — set value at location in batch

---

## Points Module (src/core/points.rkt)

- `batch-errors` — compute errors for batch expressions
- `batchref-errors` — compute errors for single batchref

---

## Compiler Module (src/core/compiler.rkt)

- `compile-batch` — compile batch to executable
- `batch-for-compiler` — internal helper

---

## Alt-Table Module (src/core/alt-table.rkt)

- `alt-batch-costs` — compute costs for alternatives
- `make-alt-table` — creates alt-table (takes batch arg)
- `atab-eval-altns` — evaluate alternatives (takes batch arg)

---

## Platform Module (src/syntax/platform.rkt)

- `batch-to-spec!` — convert impl batch to spec batch

---

## Alternative Module (src/utils/alternative.rkt)

- `unbatchify-alts` — convert batchrefs in alts to expressions

---

## Sampling Module (src/core/sampling.rkt)

- `batch-prepare-points` — sample points for batch evaluation

---

## Taylor Module (src/core/taylor.rkt)

- `taylor-coefficients` — compute Taylor coefficients (takes batch)
- `expand-taylor!` — expand taylor expressions in batch
- `taylor` — compute Taylor series for variable in batch

---

## Mainloop Module (src/core/mainloop.rkt)

- `*global-batch*` — parameter for global batch
- `batch-score-alts` — score alternatives using batch

---

## Egg-Herbie Module (src/core/egg-herbie.rkt)

- `egraph-add-exprs` — add batch expressions to egraph
- `typed-egg-batch-extractor` — extract from egraph to batch
- `egg-nodes->batch` — convert egg nodes to batch

---

## Egglog-Herbie Module (src/core/egglog-herbie.rkt)

- `make-egglog-runner` — creates runner (takes batch arg)
- `run-egglog` — run egglog (output-batch arg)
- `egglog-add-exprs` — add batch expressions to egglog
- `egglog-runner-batch` — accessor for runner batch
