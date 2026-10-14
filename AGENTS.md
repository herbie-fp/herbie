
# Structure

- Modules in `src/` layer `utils < syntax < core < reports < api`.
  Lower layers do not import higher layers.
- `core` has key algorithms and needs to be high-quality. `syntax` has
  core concepts and needs to be clean and readable. 
- `explain.rkt` and `localize.rkt` are deprecated, as is the `rival`
  (vs `rival3`) backend. Keep them working, but they needn't block
  progress on the main path. The `egglog` backend is non-default but
  we hope to switch to it one day. Experimental code is allowed.
- The rest of `src` is, conceptually, glue code. Simple is best,
  optimize for maintainability. All of `src` goes through human code
  review and should match surrounding style.
- `infra/` is for development only and includes abandon-ware and slop.
  Don't reference for code style, don't apply heroics to keep it
  working, don't allow it to influence design of `src/`.

# Common errors

- Do not code defensively. No runtime type checks, no fallbacks.
- Use `map` over `for/list` only if it avoids a `lambda`.
- Always use `in-list` and similar with `for` variants.
- Always pass `#:length N` to `for/vector`.
  Prefer to examine all callers to establish types, or if they can
  differ prefer `match` with explicit patterns for all cases, to
  ensure that unanticipated value cause explicit errors.
- Update docs in `www/doc/2.4/` if you change user-visible options.

# Testing

- Check `git diff` and delete dead code before finishing a task.
- Format Racket code with `make fmt` at the top level.
- Use runs on benchmarks, not unit tests, as primary correctness
  check, with `racket -y src/main.rkt report <flags> <bench> <out>`.
- `bench/tutorial.rkt` is a ~5s check for obvious issues.
  `bench/hamming.rkt` is a longer ~1min check for significant
  algorithmic changes. For more intensive tests, start a nightly.
- Pass `--seed N` for reproducibility and `--timeout T` to set a
  per-benchmark timeout.
- The `<out>` will have one directory per benchmark. The directory
  contains output `graph.html` and observability `timeline.json`.
- Some files have unit tests; run them with `raco test <file>`.
- The default e-graph backend is `egg`; pass `--enable generate:egglog`
  to enable the `egglog` backend.

# Observability

- Nightlies are accessed with `uvx nightlies`; run with `--help` for
  usage. Do not run nightlies locally unless instructed.
- If you're investigating a single benchmark, copy it to a new file
  and run just that file.
- `timeline.json` has rich observability data for each benchmark. It
  is a list of phases, each a map from key to "table", each table is a
  list of fixed-length arrays, defined in `src/utils/timeline.rkt`.
- Add to the timeline with `(timeline-push! 'type val1 val2 ...)`. The
  `val`s must be JSON-compatible, so convert symbols to strings.
- Herbie runs also generate a profile in `profile.json`.
- You can also dump GC/memory data (to `dump-trace.json`) with
  `--enable dump:trace`, Rival commands with `--enable dump:rival`,
  and egglog commands with `--enable dump:egglog`. Dumps go in
  `dump-XXX` in the current directory. New runs *add files* to those
  directories, so clean up when done.
