
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

# Documentation

- Documentation lives in `www/doc/2.3/` and is HTML-formatted. Update
  it if you change any user-visible options.

# PRs

- PR descriptions should be 1-3 paragraphs in length. Describe the
  current behavior and why you changed it. Avoid bullet points.
- Be explicit about the expected impact: "should be faster", "should
  be more accurate", "pure refactor, no changes", and so on.
