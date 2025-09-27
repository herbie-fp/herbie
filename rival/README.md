Interval Arithmetic for Real Computation
========================================

Rival is a library for evaluating real expressions to high precision.
Rival supports Racket 8.0+ and depends only on the standard `math`
library, including `math/bigfloat`.

Using Rival
-----------

Rival is easiest to use from its REPL:

    $ racket -l rival
    > (eval (sin 1e100))
    -0.3723761236612767
    > (set precision 1000)
    > (eval (sin 1e100))
    -0.3723761236612766882620866955531642957...
    > (set precision 100)
    > (define (f x) (- (sin x) (- x (/ (pow x 3) 6))))
    > (eval f 1e-100)
    8.3333333333333333333333333333237e-503

The [full
documentation](https://docs.racket-lang.org/rival/index.html)
describes the Racket API and the underlying interval library.

Features
--------

Rival provides a number of advanced features, which we believe makes
Rival the state of the art real evaluation library:

- Support for all `math.h` functions, including arithmetic,
  trigonometric, power, remainder, rounding, and even gamma functions.
- Support for comparisons, conditionals, and boolean operations
  through boolean intervals
- Sound handling of domain errors through error intervals
- Fast and accurate mixed-precision assignments for evaluation
- Correct rounding
- Early exit using movability flags
- Well-tested soundness and robustness

Contributing
------------

Please file issues and submit patches [on
Github](https://github.com/herbie-fp/rival). All code should be
formatted using `raco fmt`; you can set this up to happen on commit
with:

    make hook

Additionally, all code must pass the automated tests, which you can
run with:

    raco test test.rkt

They also run in CI before merging. Finally, performance is a key
metric for Rival; you can run the automated performance tests with:

    racket -y time.rkt
