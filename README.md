![Casio](doc/logo.png)

Just like [Giorgio Moroder](http://www.youtube.com/watch?v=gmpsBeaVrkE),
we want to unleash the beauty of synthesis.

Casio synthesizes floating-point programs from real-number programs,
automatically handling simple numerical instabilities.

Current Status
--------------

Casio can solve many simple and not-so-simple problems, but still
needs a lot of work to behave predictably or to solve not-so-simple
problems. It has successfully surprised and stymied the developers.

Helping Out
-----------

Casio is organized on [Github](https://github.com/uw-plse/casio/) and
[Trello](https://trello.com/b/lh7b33Dr/casio).  We also hang out on
the `#uwplse` channel on [Freenode](https://freenode.net). Our test
results are on [totalcrazyhack.net](http://totalcrazyhack.net/casio/reports/).

Email [Zach Tatlock](mailto:ztatlock@cs.uw.edu) to get involved.
He’ll set you up with access to these two tools.

Setting up your Environment
---------------------------

Racket needs to know where the Casio source code lives, so that each
module of Casio can refer to other modules. To do this linking, run

    make link

from the main project directory.

You should use Emacs to edit Racket code; Dr. Racket, which ships with
Racket, is a bit too limited an editor for the number of files and
complexity of Casio. You’ll want to use the `quack` and `geiser` Emacs
packages to give you Racket-specific highlighting and a Racket REPL.
The easiest way to install these is to run

    (require 'package)
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/"))
    (package-initialize)
    (mapcar #'package-install '(quack geiser))

This needs to be done once. You can now open a Racket file, like
`casio/main.rkt`, and the mode-line will read `Scheme Racket/A`,
indicating that Quack, the Scheme mode, is running.

If you hit `C-c C-a` in a Racket buffer, you’ll open up a REPL and
“enter” that module, allowing you to refer to definitions in it. The
same `C-c C-a` binding reloads the file, while `C-M-x` reloads
individual definitions and `C-c C-e` executes individual
S-expressions.

Running Tests
-------------

Casio draws its test suite from open source projects, examples emailed
to the developers, and from numerical analysis textbooks. Whenever
possible, we try to *extract all possible tests* from a source, to
avoid biasing our selection of tests. To run the tests, go to the
project root and run

    make report

After a lot of time (approximately 15 minutes), you should find
`report.html` in the `graphs/` folder.

Tests are collected on
[totalcrazyhack.net](http://totalcrazyhack.net/casio/reports/).
If you have an account on this server, you can publish your test
report with

    make publish
