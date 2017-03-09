![Herbie](logo.png)

Herbie synthesizes floating-point programs from real-number programs,
automatically handling simple numerical instabilities.
Visit [our website](https://herbie.uwplse.org) for tutorials,
documentation, and an online demo.

Current Status
--------------

[![Build Status](https://travis-ci.org/uwplse/herbie.svg?branch=master)](https://travis-ci.org/uwplse/herbie)

Herbie can improve the accuracy of many real-world programs, and is
used by scientists in many disciplines. It has lead to two patches
(for
complex [square roots](https://github.com/josdejong/mathjs/pull/208)
and
[trigonometric functions](https://github.com/josdejong/mathjs/pull/247)),
in [math.js](http://mathjs.org/) an open-source mathematics library.
Herbie has semi-regular releases twice a year, maintains backwards
compatibility, and uses standardized formats.

Helping Out
-----------

Herbie development is organized on our
[mailing list](https://mailman.cs.washington.edu/mailman/listinfo/herbie)
where we discuss work in progress and announce major improvements.
[Email us](mailto:herbie@cs.washington.edu) to get involved!

We use [Github](https://github.com/uwplse/herbie)
and [Trello](https://trello.com/b/lh7b33Dr/herbie) to organize some
development goals Our test results
are [archived](http://herbie.uwplse.org/reports/).

Installing
----------

For full details on installing Herbie, please see the
[tutorial](http://herbie.uwplse.org/doc/latest/installing-herbie.html).

Herbie requires Racket 6.3 or later, and supports Linux and OS X.
Install it with:

    raco pkg install herbie

This will install a `herbie` binary to somewhere in your home
directory. You can also run `src/herbie.rkt` directly instead of using
the `herbie` command, for example if you'd like to download the source
directly instead of through the package manager.

Running Herbie
--------------

For full details on running Herbie, please see the
[tutorial](http://herbie.uwplse.org/doc/latest/using-herbie.html).

Herbie's input is a Scheme-based language called [FPCore](http://fpbench.org/spec/fpcore-1.0.html);
you can several examples in `bench/`.
For example, consider this simple expression:

    (FPCore (x)
      (- (+ 1 x) x))

Run Herbie from the top-level directory of the repo, and enter the
cancellation test:

    $ herbie shell
    Seed: #(1046809171 2544984934 1871826185 4237421819 4093186437 162666889)
    herbie> (FPCore (x) (- (+ 1 x) x))
    (FPCore (x) 1)

The output is Herbie's improved, more-accurate expression, in this case
the constant `1`.

Besides the `shell`, Herbie also has a `web` interface, and can run on
files of FPCore expressions with the `improve` and `report` commands.
Consult the
[documentation](http://herbie.uwplse.org/doc/latest/using-herbie.html).
for more.

Running Tests
-------------

Herbie contains unit tests to test basic functionality, though
coverage is far from complete. You can run the test suite with:

    raco test src/

Herbie also contains a large integration suite from open source
projects, examples emailed to the developers, and from numerical
analysis textbooks. This suite is found in `bench/`. The full test can
be run with

    herbie report bench/

This full test can take several hours to run. We often test Herbie on
basic but representative examples with:

    herbie report bench/hamming/

This takes approximately 15 minutes.

Test results are collected on
[uwplse.org](http://herbie.uwplse.org/reports/). If you have an
account on this server, you can publish your test results with

    make publish
