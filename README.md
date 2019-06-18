![Herbie](logo.png)


[![Build Status](https://travis-ci.org/uwplse/herbie.svg?branch=master)](https://travis-ci.org/uwplse/herbie)

Herbie automatically improves the error of floating point expressions.
Visit [our website](https://herbie.uwplse.org) for tutorials,
documentation, and an online demo. Herbie has semi-regular releases
once a year, maintains backwards compatibility, and uses standardized
formats.

Installing
----------

For full details on installing Herbie, please see the
[tutorial](http://herbie.uwplse.org/doc/latest/installing.html).

Herbie requires Racket 7.0 or later, and supports Windows, OS X, and
Linux. Install it with:

    raco pkg install --auto herbie

This will install a `herbie` binary to somewhere in your home
directory. You can also run `src/herbie.rkt` directly instead of using
the `herbie` command, for example if you'd like to download the source
directly instead of through the package manager.

Running Herbie
--------------

For full details on running Herbie, please see the
[tutorial](http://herbie.uwplse.org/doc/latest/using-web.html).

Herbie's input is a Scheme-based language called [FPCore](http://fpbench.org/spec/fpcore-1.0.html);
you can several examples in `bench/`.
For example, consider this simple expression:

    (FPCore (x) (- (+ 1 x) x))

Run Herbie from the top-level directory of the repo, and enter the
cancellation test:

    $ herbie shell
    Herbie 1.3 with seed 1866513483
    Find help on https://herbie.uwplse.org/, exit with Ctrl-D
    herbie> (FPCore (x) (- (+ 1 x) x))
    (FPCore (x) ... 1)

The output is Herbie's improved, more-accurate expression, in this case
the constant `1`.

Besides the `shell`, Herbie also has a `web` interface, and can run on
files of FPCore expressions with the `improve` and `report` commands.
Consult the
[documentation](http://herbie.uwplse.org/doc/latest/tutorial.html).
for more.

Helping Out
-----------

Herbie development is organized on our
[mailing list](https://mailman.cs.washington.edu/mailman/listinfo/herbie)
where we discuss work in progress and announce major improvements.
[Email us](mailto:herbie@cs.washington.edu) to get involved!

We use [Github](https://github.com/uwplse/herbie)
and [Trello](https://trello.com/b/lh7b33Dr/herbie) to organize some
development goals.

Running Tests
-------------

Herbie contains unit tests to test basic functionality, though
coverage is far from complete. You can run the test suite with:

    raco test src/

Herbie also contains a large integration suite from open source
projects, examples emailed to the developers, and from numerical
analysis textbooks. This suite is found in `bench/`. The full test can
be run with

    herbie report bench/ graphs/

The output is an HTML report in `graphs/`. This full test can take
several hours to run. We often test Herbie on basic but representative
examples with:

    herbie report bench/hamming/ graphs/

This takes approximately 15 minutes.

Historic and nightly test results are collected on
[uwplse.org](http://herbie.uwplse.org/reports/).
