
# Structure

- `src/core` has key algorithms and `src/syntax` key concepts; these
  are the most critical and highest value parts of the project. Should
  be fast and effective, complex algorithms could well be worth it.
- `src/core/explain.rkt` and `src/core/localize.rkt` are sorta-kinda
  deprecated. Keep them working, but can get slower or worse.
- The rest of `src` is, conceptually, glue code. Simple is best,
  optimize for maintainability. All of `src` goes through human code
  review and should match surrounding style.
- Modules layer `utils < syntax < core < reports < api`. Lower layers
  do not import higher layers.
- `infra/` is for development only. Some code in there is abandon-ware
  of AI-written and unreviewed. Don't reference for code style and not
  all features in there are actually used. Changing users often easier
  than code heroics.

# Formatting

- Use `map` over `for/list` only if it avoids a `lambda`.
- Always use `in-list` and similar with `for` variants.
- Always pass `#:length N` to `for/vector`.
- Do not code defensively. Do not do runtime type checks or fallback.
  Prefer to examine all callers to establish types, or if they can
  differ prefer `match` with explicit patterns for all cases, to
  ensure that unanticipated value cause explicit errors.
- Format Racket code with `make fmt` at the top level.
- Check `git diff` and delete dead code before finishing a task.
- Update docs in `www/doc/2.3/` if you change user-visible options.

# Testing

- If you change the Herbie core, test that your changes work with
  `racket src/main.rkt report bench/tutorial.fpcore tmp`. This should
  take about 5-10 seconds and all of the tests should pass with
  perfect accuracy. You can also use other benchmark suites if asked.
- Arguments come after the word `report` before any other arguments.
- Herbie prints a seed every time it runs; pass `--seed N` for
  reproducibility.
- Pass `--timeout T` to time out a benchmark after T seconds.
  Each has a detailed `graph.html`, including tracebacks for crashes.
- After running tests, `tmp` will have one directory per benchmark.
- For multiple reports name the folders `tmp-<whatever>`.
- The default e-graph backend is `egg`; pass `--enable generate:egglog`
  to enable the `egglog` backend.
- Some files have unit tests; run them with `raco test <file>`.

# Observability

- If you're investigating a single benchmark, copy it to a file named
  `test.fpcore` and run just that file.
- Herbie runs output a `tmp/<benchmark>/timeline.json` with rich
  observability data for each benchmark.
- This timeline is a list of phases, each a map from key to "table",
  each table is a list of fixed-length arrays.
- Timeline types are defined in `src/utils/timeline.rkt`; HTML
  generation is defined in `src/reports/timeline.rkt`.
- Add to the timeline with `(timeline-push! 'type val1 val2 ...)`. The
  `val`s must be JSON-compatible, so convert symbols to strings.
- Herbie runs generate a profile in `tmp/<benchmark>/profile.json`.
- You can also dump GC/memory data to `dump-trace.json` with `--enable
  dump:trace`, Rival commands with `--enable dump:rival`, and egglog
  commands with `--enable dump:egglog`. Dumps go in `dump-XXX` in the
  current directory. New runs *add files* to those directories, so
  clean up when done.
