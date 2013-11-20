Editing Racket
==============

Since you might not be familiar with Racket (hey, it's a niche language), here's a quick way to get set up.

= Using Dr. Racket =

Install the Racket runtime, preferably from [https://www.racket-lang.org](the website).  You should have an executable called `drracket` or, if you're on OS X, an application called something similar.  Launch it, open `casio.rkt`, and you're good to go.  To insert actual lambdas, get in the habbit of typing Control-backslash.  You can show and hide a REPL with Control-E.

= Using Emacs =

In Emacs, install the ELPA packages `quack` and `geiser`.  Perhaps the easiest way to do this is to execute this Lisp code:

    (require 'package)
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/"))
    (package-initialize)
    (mapcar #'package-install '(quack geiser))

This needs to be done only once.  You can now open `casio.rkt`; the mode-line should read `Scheme Racket/A`, indicating that Quack, the Scheme mode, is running.  You can now open a REPL with `M-x run-geiser`.  This will request the name of your Racket executable, which should be `racket`.

Now go back to `casio.rkt` (with `C-c C-z`) and type `C-c C-a`; this will "enter" your module, allowing you to refer to definitions in it.  You can use this same `C-c C-a` binding to reload your changes from file, or use `C-M-x` to load individual definitions (also, `C-c C-e` will execute individual S-expressions).

To make it easy to enter lambdas, run the following Lisp:

    (define-abbrev scheme-mode-abbrev-table "lambda" "λ")

After running this code, a lambda will appear every time you enter `lambda`.  You might want to add this bit of Lisp to your startup files.

= Running Tests =

Before running tests, you need to make the `casio` package available. You can do this from the root directory with

    raco link casio/

