
# Testing

- Before presenting any code run `make fmt` to format the code. This
  is mandatory and PRs that don't follow the coding style are
  rejected.
- As a quick test to make sure everything is working, you can run
  `racket src/main.rkt report bench/tutorial.fpcore tmp`; this should
  take about 5-10 seconds and all of the tests should pass, getting
  basically perfect accuracy.
- You can also run the unit tests with `raco test src/`, but these
  aren't very complete; focus more on the full run from the previous
  bullet point.

# PRs

- PR descriptions should be in full sentences one to a few paragraphs
  paragraphs in length. For all but the most trivial fixes, describe
  the current behavior and why you changed it.
- Be explicit about the expected impact: "should be faster", "should
  be more accurate", "pure refactor, no changes", and so on.
