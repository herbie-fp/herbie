![Herbie](logo.png)

Herbie synthesizes floating-point programs from real-number programs,
automatically handling simple numerical instabilities.

Current Status
--------------

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
[Github](https://github.com/uw-plse/casio/) and
[Trello](https://trello.com/b/lh7b33Dr/herbie).
We also hang out on the `#uwplse` channel on
[Freenode](https://freenode.net).
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
Herbie also requires GCC 4.8 or later.

Racket needs to know where the Herbie source code lives, so that each
module of Herbie can refer to other modules. To do this linking, run

    make link

from the main project directory.

Running Tests
-------------

Herbie draws its test suite from open source projects, examples emailed
to the developers, and from numerical analysis textbooks. Whenever
possible, we try to *extract all possible tests* from a source, to
avoid biasing our selection of tests. To run the tests, go to the
project root and run

    make report

After a some time (approximately 15 minutes), you should find
`report.html` in the `graphs/` folder.

Tests are collected on
[uwplse.org](http://herbie.uwplse.org/reports/).
If you have an account on this server, you can publish your test
report with

    make publish

Editing
-------

You should use Emacs to edit Racket code; Dr. Racket, which ships with
Racket, is a bit too limited an editor for the number of files and
complexity of Herbie. You’ll want to use the `quack` and `geiser` Emacs
packages to give you Racket-specific highlighting and a Racket REPL.
The easiest way to install these is to run

    (require 'package)
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/"))
    (package-initialize)
    (mapcar #'package-install '(quack geiser))

This needs to be done once. You can now open a Racket file, like
`herbie/main.rkt`, and the mode-line will read `Scheme Racket/A`,
indicating that Quack, the Scheme mode, is running.

If you hit `C-c C-a` in a Racket buffer, you’ll open up a REPL and
“enter” that module, allowing you to refer to definitions in it. The
same `C-c C-a` binding reloads the file, while `C-M-x` reloads
individual definitions and `C-c C-e` executes individual
S-expressions.
