![Herbie](logo.png)

Herbie synthesizes floating-point programs from real-number programs,
automatically handling simple numerical instabilities.

Current Status
--------------

[![Build Status](https://travis-ci.org/uwplse/herbie.svg?branch=master)](https://travis-ci.org/uwplse/herbie)

Herbie can solve many simple and not-so-simple problems.
It can improve the accuracy of many real-world programs,
and successfully solves most problems from Richard Hamming’s
Numerical Methods for Scientists and Engineers, Chapter 3.
It has successfully been used by colleagues to improve
the qualitative results of machine learning algorithms.
It has lead to two patches
(for complex [square roots](https://github.com/josdejong/mathjs/pull/208)
and [trigonometric functions](https://github.com/josdejong/mathjs/pull/247)),
in [math.js](http://mathjs.org/) an open-source mathematics library.

Helping Out
-----------

Herbie is organized on
[Github](https://github.com/uwplse/herbie) and
[Trello](https://trello.com/b/lh7b33Dr/herbie).
We also have a
[mailing list](https://mailman.cs.washington.edu/mailman/listinfo/herbie)
where we discuss Herbie's development and announce major improvements.
Our test results are on
[uwplse.org](http://herbie.uwplse.org/reports/).

Email [Zach Tatlock](mailto:ztatlock@cs.uw.edu) to get involved.
He’ll set you up with access to these tools.

Installing
----------

Herbie requires Racket 6.1 or later, and works best on Linux.
If you're using OS X, use the
[official Racket installer](http://download.racket-lang.org/),
not the version that Homebrew installs;
Homebrew will not build several mathematics libraries.

Herbie also requires GMP, MPFR, and GCC 4.8 or later to test overhead,
since that involves compiling Herbie’s output to C.

If you are running Racket 6.3 or newer, you will also need to install the
unstable package:

    raco pkg install unstable

With these libraries installed, you are now ready to run Herbie.

Running Herbie
--------------

For details on how to run Herbie, please see the
[tutorial](http://herbie.uwplse.org/doc/tutorial.html).

The format of input files is a Scheme-based language;
you can find several examples in `bench/`.
For example,
consider this simple cancellation test

    (herbie-test (x)
      "Cancel like terms"
      (- (+ 1 x) x))

To run Herbie on this example, save the above s-expression
in the file `bench/example.rkt` and run

    racket herbie/reports/run.rkt bench/example.rkt

from the top-level directory of the repo.
You should see output similar to

    $ racket herbie/reports/run.rkt example.rkt
    Starting Herbie on 1 problems...
    Seed: #(1046809171 2544984934 1871826185 4237421819 4093186437 162666889)
      1/1   [ 1673.401ms]   (29→ 0) Cancel like terms

Running Tests
-------------

Herbie draws its test suite from open source projects, examples emailed
to the developers, and from numerical analysis textbooks. Whenever
possible, we try to *extract all possible tests* from a source, to
avoid biasing our selection of tests. To run the tests, go to the
project root and run

    racket herbie/reports/run.rkt <file>

After a some time (several minutes), you should find the file
`graphs/report.html`.

Herbie's main tests are integration tests
that test Herbie end-to-end.
They are the benchmarks that ship in the `bench/` directory.

We often test Herbie on basic but representative examples with:

    racket herbie/reports/run.rkt bench/hamming/

This takes approximately 15 minutes.
To run all of the default benchmarks, use:

    racket herbie/reports/run.rkt bench/

This can take an hour or more.

Test results are collected on
[uwplse.org](http://herbie.uwplse.org/reports/).
If you have an account on this server, you can publish your test
report with

    make publish
