#lang scribble/manual

@(require (for-label "../main.rkt" racket/base math/bigfloat))
@(require scribble/example racket/sandbox racket/pretty)
@(define example-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string])
     (make-evaluator 'racket #:requires '(rival math/bigfloat racket/pretty))))
@(call-in-sandbox-context example-eval
             (lambda ()
               (current-print (dynamic-require 'racket/pretty 'pretty-print-handler))))

@title{Rival: Real Computation via Interval Arithmetic}
@author{Pavel Panchekha, Artem Yadrov, Oliver Flatt}
@defmodule[rival]

Rival is an advanced interval arithmetic library for
arbitrary-precision computation of complex mathematical expressions.
Its interval arithmetic is valid and attempts to be tight.
Besides the standard intervals, Rival also supports boolean intervals,
error intervals, and movability flags, as described in
@hyperlink["https://arxiv.org/abs/2107.05784"]{"An Interval Arithmetic
for Robust Error Estimation"}.

Rival is a part of the @hyperlink["https://herbie.uwplse.org"]{Herbie project},
and is developed @hyperlink["https://github.com/herbie-fp/rival"]{on Github}.

Rival provides a command-line REPL for testing its capabilities:

@verbatim|{
$ racket -l rival
> (define (f x) (- (sin x) (- x (/ (pow x 3) 6))))
> (eval f 0.5)
2.588719375363336e-4
> (eval f 1e-100)
8.3333333333333338e-503
}|

Use @code{help} at the command-line to see more the available commands

Rival can also be used programmatically:

@examples[#:eval example-eval #:label #f
(define expr '(- (sin x) (- x (/ (pow x 3) 6))))
(define machine (rival-compile (list expr) '(x) (list flonum-discretization)))
(rival-apply machine (vector (bf 0.5)))
(rival-apply machine (vector (bf 1e-100)))
]

Rival works by evaluating the expression with high-precision interval
arithmetic, repeating the evaluation with ever-higher precision until
a narrow-enough output interval is found. The @exec{explain} command
shows a visualization of this process:

@verbatim|{
> (explain f 1e-100)
Executed 7 instructions for 3 iterations:

        Bits  Time  Bits  Time  Bits  Time
------------------------------------------
adjust                     8.1         6.8
   sin    78   3.9   667  13.9  1768  28.1
     3    93   0.0                        
   pow    88   6.1              1112   5.1
     6    88   0.0                        
   div    83   3.9              1107   3.9
   add    78   2.0   667   3.2  1768   2.9
   sub    73   3.2    73   2.0    73   3.2
------------------------------------------
 Total        19.0        27.1        50.0

Total: 128.9µs
}|

In this case Rival needed three iterations with up to 1770 bits for
some of the operations in the final iteration. About a third of the
time was spent in the @code{sin} operation. The same runtime
information can be accessed programmatically using
@racket[rival-profile].

Rival also exposes the underlying interval-arithmetic library:

@examples[#:eval example-eval #:label #f
(bf-precision 20)
(define x (ival 2.bf 3.bf))
x
(ival-add x x)
(ival-sqrt x)
]

Rival is fast, accurate, and sound. We believe it to be a
state-of-the-art implementation, competitive with Sollya/MPFI,
Calcium/Arb, and Mathematica.

@include-section["eval.scrbl"]
@include-section["core.scrbl"]
@include-section["profile.scrbl"]
