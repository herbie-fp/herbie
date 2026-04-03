# Branch Triage Report

Generated on 2026-03-23 after deleting fully represented branches and many closed-PR dead ends.

Summary lines below are best-effort and are based on each branch's unique commits relative to `main`, plus the rough shape of the branch diff. They should be read as "what this branch seems to be for" rather than as a precise spec.

## Keep

- `main` - current mainline branch.
- `debug-tools` - adds an e-graph debugging tool for checking whether specific expressions exist in the graph. `(2026-01-25, 1 unique, open PR #1481)`
- `functions-in-platform` - replaces function inlining with platform operators / add-to-platform style function handling. `(2026-01-27, 1 unique, open PR #1484)`
- `dump-nightly-trace` - adds local dumping of nightly traces for debugging nightly runs. `(2025-08-13, 1 unique, no PR)`
- `pavel-nobigfloat` - substantial recent branch centered on removing or reducing bigfloat overhead and fixing a memory leak. `(2024-12-06, 37 unique, no PR)`
- `time-nightly` - adds timing instrumentation for nightly phases and reporting around nightly runtime breakdowns. `(2025-05-03, 21 unique, no PR)`
- `typed-racket-2` - recent Typed Racket work, including typing the timeline machinery. `(2025-12-28, 7 unique, no PR)`
- `no-pruning` - disables e-graph pruning, probably to test quality/runtime tradeoffs or debug pruning behavior. `(2024-12-06, 1 unique, no PR)`
- `unsound-last` - pushes unsound rules or phases later in the pipeline, likely to preserve quality while reducing bad early effects. `(2025-05-14, 2 unique, no PR)`

## Stale Unique Work

- `backup-simplify-is-egg` - experiment around making simplify use egg more directly, with notes that it breaks Taylor. `(2021-08-31, 2 unique, no PR)`
- `ffi-optimizations` - small set of FFI-related optimization fixes, probably around the Rust or native boundary. `(2024-04-21, 3 unique, no PR)`
- `arb-support` - initial Arb support experiment adding a standalone `src/arb.rkt`; old and unique, but not obviously superseded. `(2023-08-16, 1 unique, no PR)`
- `preprocess-code` - exposes preprocessing steps as part of the shown program/code path. `(2022-06-02, 1 unique, no PR)`
- `secret-rr` - newer `secret-rr` variant with changes to make RR integration more normal. `(2021-08-13, 3 unique, no PR)`
- `ival-perf-with-search` - substantial interval-analysis performance branch that mixes search changes with extra hard examples / demos. `(2020-07-09, 28 unique, no PR)`

## Archaeology

Some tiny archaeology branches were deleted on 2026-03-30 because their remaining diffs were clearly obsolete plumbing or already absorbed by modern files: `better-rr`, `kill-old-mainloop-code`, `one-nightly`, `refactor-make-graph`, `runnable-files`, and `samplers-in-json`.

- `bar-graphs` - very old reporting / visualization branch that replaces or adds bar-graph output and still carries a huge amount of pre-modern history. `(2014-11-08, 322 unique, no PR)`
- `origin-new-simplify` - early "new simplify" / egraph implementation line with a very large historical diff from modern Herbie. `(2014-09-02, 224 unique, no PR)`
- `pavel-simplification-fixes` - another large early simplification branch with broad labels / conservative-mode plumbing changes. `(2014-09-12, 220 unique, no PR)`
- `taylor` - original large Taylor-expansion development branch with a lot of pre-mainline Taylor history. `(2014-09-09, 219 unique, no PR)`
- `git-annex` - ancient branch tied to git-annex or large-artifact workflow experiments. `(2014-11-20, 13 unique, no PR)`
- `better-compilation-to-C` - old branch for a register-machine / CSE / C backend compilation path. `(2016-04-08, 4 unique, no PR)`
- `egraph` - very early egraph / graphviz work predating current e-graph architecture. `(2014-04-30, 3 unique, no PR)`
- `gonzo-reckoned` - old branch tied to gonzo rules or reckoning behavior, now mostly historical context. `(2018-04-16, 4 unique, no PR)`
- `ignore-test` - one-off branch splitting out a biginterval library and likely tied to test-harness experimentation. `(2020-05-04, 1 unique, no PR)`
- `no-more-slocations` - strips out source locations entirely. `(2015-10-05, 1 unique, no PR)`
- `regimes-cleanup` - old failed regimes cleanup attempt. `(2016-06-06, 3 unique, no PR)`
- `remove-backup-simplify` - old Taylor / simplify cleanup that removes backup simplify paths and uses `make-*` constructors. `(2018-07-13, 1 unique, no PR)`
- `share-egraph-across-simplifications` - early egraph experiment to reuse one graph across simplify passes. `(2015-02-03, 2 unique, no PR)`
- `smarter-regimes` - early regimes experiment around max-error objective behavior. `(2015-01-26, 1 unique, no PR)`
- `taylor-fixes` - old report-polish branch fixing duplicate Taylor bullets. `(2016-11-29, 4 unique, no PR)`
- `taylor-one-bullet-point` - related old Taylor-report branch collapsing the report down to one bullet point. `(2016-11-29, 4 unique, no PR)`
- `typed-racket` - first-generation Typed Racket experiment with only a tiny amount of unique work left. `(2018-10-03, 2 unique, no PR)`
- `viz-wip` - old WIP branch for a Herbie visualizer. `(2018-09-06, 1 unique, no PR)`
