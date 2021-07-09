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
[documentation](https://herbie.uwplse.org/doc/latest/installing.html).

Herbie requires Racket 7.0 or later, and supports Windows, macOS, and
Linux. Install it with:

    raco pkg install --auto herbie

This will install a `herbie` binary to somewhere under `~/.racket`.
You can also download the source and run `src/herbie.rkt` directly.

Alternatively, Herbie and Herbie's Rust components can be built from source and installed
  using `make install` in the herbie directory.

Running Herbie
--------------

For full details on running Herbie, please see the
[tutorial](https://herbie.uwplse.org/doc/latest/using-web.html).

Herbie's input format is the Scheme-like
[FPCore](https://fpbench.org/spec/fpcore-1.2.html);
for example `(1 + x) - x` is written 

    (FPCore (x) (- (+ 1 x) x))

You can see more examples in `bench/`. To use Herbie, run `herbie
shell` and enter an FPCore expression:

    $ herbie shell
    Herbie 1.3 with seed 1866513483
    Find help on https://herbie.uwplse.org/, exit with Ctrl-D
    herbie> (FPCore (x) (- (+ 1 x) x))
    (FPCore (x) ... 1)

In this case Herbie's improved, more-accurate expression is the
constant `1`.

Besides `shell`, Herbie also has a `web` interface, and can be run in
batch mode on files with the `improve` and `report` commands. Consult
the [documentation](https://herbie.uwplse.org/doc/latest/options.html).
for more.

Helping Out
-----------

Herbie development is organized on our
[mailing list](https://mailman.cs.washington.edu/mailman/listinfo/herbie),
where we discuss work in progress and announce major improvements.
[Email us](mailto:herbie@cs.washington.edu) to get involved!

We use [Github](https://github.com/uwplse/herbie) and
[Trello](https://trello.com/b/lh7b33Dr/herbie) to organize development
goals.

Running Tests
-------------

Herbie has unit tests for basic functionality, though coverage is far
from complete. You can run the test suite by downloading the source
code, changing to the source code directory, and running:

    make install
    raco test src/

Herbie also contains a large benchmark suite drawn from open source
projects, examples from users, and numerical analysis textbooks. This
suite is found in `bench/`. The full test can be run with

    herbie report bench/ report/
    
You may see warnings; these are expected. The output is HTML files in
`report/`. This full test can take a few hours to run. We often test
Herbie on basic but representative examples with:

    herbie report bench/hamming/ graphs/

This takes approximately 10 minutes.

Historic and nightly test results are collected on
[uwplse.org](https://herbie.uwplse.org/reports/).
