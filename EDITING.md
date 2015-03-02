
Editing
=======

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