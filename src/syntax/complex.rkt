#lang racket

(require (submod "syntax.rkt" internals) (submod "types.rkt" internals) (submod "rules.rkt" internals))
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

(define-ruleset commutativity.c (arithmetic simplify fp-safe complex)
  #:type ([a complex] [b complex])
  [+.c-commutative     (+.c a b)               (+.c b a)]
  [*.c-commutative     (*.c a b)               (*.c b a)])

(define-ruleset associativity.c (arithmetic simplify complex)
  #:type ([a complex] [b complex] [c complex])
  [associate-+r+.c     (+.c a (+.c b c))         (+.c (+.c a b) c)]
  [associate-+l+.c     (+.c (+.c a b) c)         (+.c a (+.c b c))]
  [associate-+r-.c     (+.c a (-.c b c))         (-.c (+.c a b) c)]
  [associate-+l-.c     (+.c (-.c a b) c)         (-.c a (-.c b c))]
  [associate--r+.c     (-.c a (+.c b c))         (-.c (-.c a b) c)]
  [associate--l+.c     (-.c (+.c a b) c)         (+.c a (-.c b c))]
  [associate--l-.c     (-.c (-.c a b) c)         (-.c a (+.c b c))]
  [associate--r-.c     (-.c a (-.c b c))         (+.c (-.c a b) c)]
  [associate-*r*.c     (*.c a (*.c b c))         (*.c (*.c a b) c)]
  [associate-*l*.c     (*.c (*.c a b) c)         (*.c a (*.c b c))]
  [associate-*r/.c     (*.c a (/.c b c))         (/.c (*.c a b) c)]
  [associate-*l/.c     (*.c (/.c a b) c)         (/.c (*.c a c) b)]
  [associate-/r*.c     (/.c a (*.c b c))         (/.c (/.c a b) c)]
  [associate-/l*.c     (/.c (*.c b c) a)         (/.c b (/.c a c))]
  [associate-/r/.c     (/.c a (/.c b c))         (*.c (/.c a b) c)]
  [associate-/l/.c     (/.c (/.c b c) a)         (/.c b (*.c a c))]
  [sub-neg.c           (-.c a b)                 (+.c a (neg.c b))]
  [unsub-neg.c         (+.c a (neg.c b))           (-.c a b)])

(define-ruleset distributivity.c (arithmetic simplify complex)
  #:type ([a complex] [b complex] [c complex])
  [distribute-lft-in.c      (*.c a (+.c b c))           (+.c (*.c a b) (*.c a c))]
  [distribute-rgt-in.c      (*.c a (+.c b c))           (+.c (*.c b a) (*.c c a))]
  [distribute-lft-out.c     (+.c (*.c a b) (*.c a c))   (*.c a (+.c b c))]
  [distribute-lft-out--.c   (-.c (*.c a b) (*.c a c))   (*.c a (-.c b c))]
  [distribute-rgt-out.c     (+.c (*.c b a) (*.c c a))   (*.c a (+.c b c))]
  [distribute-rgt-out--.c   (-.c (*.c b a) (*.c c a))   (*.c a (-.c b c))]
  [distribute-lft1-in.c     (+.c (*.c b a) a)           (*.c (+.c b (complex 1 0)) a)]
  [distribute-rgt1-in.c     (+.c a (*.c c a))           (*.c (+.c c (complex 1 0)) a)])

(define-ruleset fractions-distribute.c (fractions simplify complex)
  #:type ([a complex] [b complex] [c complex] [d complex])
  [div-sub.c     (/.c (-.c a b) c)          (-.c (/.c a c) (/.c b c))]
  [times-frac.c  (/.c (*.c a b) (*.c c d))  (*.c (/.c a c) (/.c b d))])

(define-ruleset fractions-transform.c (fractions complex)
  #:type ([a complex] [b complex] [c complex] [d complex])
  [sub-div.c     (-.c (/.c a c) (/.c b c))  (/.c (-.c a b) c)]
  [frac-add.c    (+.c (/.c a b) (/.c c d))  (/.c (+.c (*.c a d) (*.c b c)) (*.c b d))]
  [frac-sub.c    (-.c (/.c a b) (/.c c d))  (/.c (-.c (*.c a d) (*.c b c)) (*.c b d))]
  [frac-times.c  (*.c (/.c a b) (/.c c d))  (/.c (*.c a c) (*.c b d))]
  [frac-2neg.c   (/.c a b)                  (/.c (neg.c a) (neg.c b))])

(define-ruleset complex-number-basics (complex simplify)
  #:type ([x real] [y real] [a real] [b real] [c real] [d real])
  [real-part        (re (complex x y))     x]
  [imag-part        (im (complex x y))     y]
  [complex-add-def  (+.c (complex a b) (complex c d)) (complex (+ a c) (+ b d))]
  [complex-def-add  (complex (+ a c) (+ b d)) (+.c (complex a b) (complex c d))]
  [complex-sub-def  (-.c (complex a b) (complex c d)) (complex (- a c) (- b d))]
  [complex-def-sub  (complex (- a c) (- b d)) (-.c (complex a b) (complex c d))]
  [complex-neg-def  (neg.c (complex a b)) (complex (neg a) (neg b))]
  [complex-def-neg  (complex (neg a) (neg b)) (neg.c (complex a b))]
  [complex-mul-def  (*.c (complex a b) (complex c d))
                    (complex (- (* a c) (* b d)) (+ (* a d) (* b c)))]
  [complex-div-def  (/.c (complex a b) (complex c d))
                    (complex (/ (+ (* a c) (* b d)) (+ (* c c) (* d d)))
                             (/ (- (* b c) (* a d)) (+ (* c c) (* d d))))]
  [complex-conj-def (conj (complex a b)) (complex a (neg b))])
