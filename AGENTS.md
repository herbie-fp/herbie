
# Formatting

- If you modify any Racket run `make fmt` to format the code
- Check `git diff` and delete dead code before finishing a task.
- Update docs in `www/doc/2.3/` if you change user-visible options.

# Testing

- If you change the Herbie core, test that your changes work with
  `racket src/main.rkt report bench/tutorial.fpcore tmp`. This should
  take about 5-10 seconds and all of the tests should pass with
  perfect accuracy. You can also use other benchmark suites if asked.
- If you need to store two reports name the folders `tmp-X` for some X.
- Arguments come after the word `report` before any other arguments.
- Herbie prints a seed every time it runs; you can pass `--seed N`
  after the "report" argument to fix the seed reproducibly.
- You can pass `--timeout T` to time out a benchmark after T seconds.
- After running tests, `tmp` will have one directory per benchmark.
  Each has a detailed `graph.html`, including tracebacks for crashes.
- The default e-graph backend is `egg`; pass `--enable generate:egglog`
  to enable the `egglog` backend. You may need to add `~/.cargo/bin`
  to the `PATH`.
- Some files have unit tests; run them with `raco test <file>`.

# Observability

- Herbie runs output a `tmp/<benchmark>/timeline.json` with rich
  observability data for each benchmark.
- A timeline is a list of phases, each a map from key to "table",
  each table is a list of fixed-length arrays.
- Timeline types are defined in `src/utils/timeline.rkt`; HTML
  generation is defined in `src/reports/timeline.rkt`.
- Add to the timeline with `(timeline-push! 'type val1 val2 ...)`. The
  `val`s must be JSON-compatible, so convert symbols to strings.
- Herbie runs generates a profile in `tmp/<benchmark>/profile.json`.
- You can also dump GC/memory data to `dump-trace.json` with `--enable
  dump:trace`, Rival commands with `--enable dump:rival`, and egglog
  commands with `--enable dump:egglog`. Dumps go in `dump-XXX` in the
  current directory. Clean that directory when done.
