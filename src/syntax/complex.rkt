#lang racket

(require (submod "syntax.rkt" internals) (submod "types.rkt" internals))
(require math/bigfloat "../bigcomplex.rkt")

(define-type complex (conjoin complex? (negate real?)) bigcomplex?)

(define-constant I complex
  [bf (λ () (bigcomplex 0.bf 1.bf))]
  [fl (const 0+1i)]
  [nonffi (const 0+1i)]
  [ival #f]
  [->tex "i"])

(define-operator (+.c complex complex) complex
  [fl +] [bf bf-complex-add] [ival #f]
  [->tex (curry format "~a + ~a")]
  [nonffi +])

(define-operator (neg.c complex) complex
  [fl -] [bf bf-complex-neg] [ival #f]
  [->tex (curry format "-~a")]
  [nonffi -])

(define-operator (-.c complex complex) complex
  [fl -] [bf bf-complex-sub] [ival #f]
  [->tex (λ (x [y #f]) (if y (format "~a - ~a" x y) (format "-~a" x)))]
  [nonffi -])

(define-operator (*.c complex complex) complex
  [fl *] [bf bf-complex-mult] [ival #f]
  [->tex (curry format "~a \\cdot ~a")]
  [nonffi *])

(define-operator (/.c complex complex) complex
  [fl /] [bf bf-complex-div] [ival #f]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi /])

(define-operator (exp.c complex) complex
  [fl exp] [bf bf-complex-exp] [ival #f]
  [->tex (curry format "e^{~a}")]
  [nonffi exp])

(define-operator (log.c complex) complex
  [fl log] [bf bf-complex-log] [ival #f]
  [->tex (curry format "\\log ~a")]
  [nonffi log])

(define-operator (pow.c complex complex) complex
  [fl expt] [bf bf-complex-pow] [ival #f]
  [->tex (curry format "{~a}^{~a}")]
  [nonffi expt])

(define-operator (sqrt.c complex) complex
  [fl sqrt] [bf bf-complex-sqrt] [ival #f]
  [->tex (curry format "\\sqrt{~a}")]
  [nonffi sqrt])

(define-operator (complex real real) complex
  ; Override number of arguments
  [fl make-rectangular] [bf bigcomplex] [ival #f]
  [->tex (curry format "~a + ~a i")]
  [nonffi make-rectangular])

(define-operator (re complex) real
  ; Override number of arguments
  [fl real-part] [bf bigcomplex-re] [ival #f]
  [->tex (curry format "\\Re(~a)")]
  [nonffi real-part])

(define-operator (im complex) real
  ; Override number of arguments
  [fl imag-part] [bf bigcomplex-im] [ival #f]
  [->tex (curry format "\\Im(~a)")]
  [nonffi imag-part])

(define-operator (conj complex) complex
  ; Override number of arguments
  [fl conjugate] [bf bf-complex-conjugate] [ival #f]
  [->tex (curry format "\\overline{~a}")]
  [nonffi conjugate])


(declare-parametric-operator! '+ '+.c '(complex complex) 'complex)
(declare-parametric-operator! '- '-.c '(complex complex) 'complex)
(declare-parametric-operator! '- 'neg.c '(complex) 'complex)
(declare-parametric-operator! '* '*.c '(complex complex) 'complex)
(declare-parametric-operator! '/ '/.c '(complex complex) 'complex)
(declare-parametric-operator! 'pow 'pow.c '(complex complex) 'complex)
(declare-parametric-operator! 'exp 'exp.c '(complex) 'complex)
(declare-parametric-operator! 'log 'log.c '(complex) 'complex)
(declare-parametric-operator! 'sqrt 'sqrt.c '(complex) 'complex)
