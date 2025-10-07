
# Testing

- Run `make fmt` to format the code before presentingcode. This is
  mandatory and PRs that don't follow the coding style are rejected.
- Run `racket src/main.rkt report bench/tutorial.fpcore tmp` to test
  that your changes work; this should take about 5-10 seconds and all
  of the tests should pass, getting basically perfect accuracy.
- You can also run the unit tests with `raco test <file>`, when unit
  tests exist. Often they don't.

# Profiling

- Herbie generates a profile by default; run the `report` command
  above and look at `tmp/<benchmark>/profile.json`.
- You can also run with `--enable dump:trace` to output
  `dump-trace.json` in chrome://tracing format
- Additionally `--enable dump:rival` outputs all Rival commands it
  executes, which can be useful for debugging Rival &
  arbitrary-precision code.
  
# Timeline

- The timeline is Herbie's observability tooling. Timeline information
  goes into `timeline.json` and `timeline.html`.
- A timeline is a list of phases, each of which is a hash table from
  key to "table", where a table is a list of rows each of which is a
  fixed-length array. Define new timeline types in
  `src/utils/timeline.rkt` and add/change HTML generation in
  `src/reports/timeline.rkt`.
- Add to the timeline with `(timeline-push! 'type val1 val2 ...)`. The
  `val`s must be JSON-compatible; this mostly means you have to
  convert symbols to strings.

# Documentation

- Documentation lives in `www/doc/2.3/` and is HTML-formatted. Update
  it if you change any user-visible options.

# PRs

- PR descriptions should be 1-3 paragraphs in length. Describe the
  current behavior and why you changed it. Avoid bullet points.
- Be explicit about the expected impact: "should be faster", "should
  be more accurate", "pure refactor, no changes", and so on.
