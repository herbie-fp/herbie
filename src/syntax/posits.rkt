#lang racket
(require math/flonum math/bigfloat)
(require "../common.rkt" "softposit.rkt")

(eprintf "Loading posits support...\n")

;; Defining the types

(require (submod "types.rkt" internals))

(define-type posit8 posit8? bigfloat?)
(define-type posit16 posit16? bigfloat?)
(define-type posit32 posit32? bigfloat?)
(define-type quire8 quire8? bigfloat?)
(define-type quire16 quire16? bigfloat?)
(define-type quire32 quire32? bigfloat?)

;; Defining the representations

(require (submod "../interface.rkt" internals))

(define-representation posit8
  (compose double->posit8 bigfloat->flonum)
  (compose bf posit8->double)
  ordinal->posit8
  posit8->ordinal
  8
  (list posit8-nar)
  (compose double->posit8 fl))

(define-representation posit16
  (compose double->posit16 bigfloat->flonum)
  (compose bf posit16->double)
  ordinal->posit16
  posit16->ordinal
  16
  (list posit16-nar)
  (compose double->posit16 fl))

(define-representation posit32
  (compose double->posit32 bigfloat->flonum)
  (compose bf posit32->double)
  ordinal->posit32
  posit32->ordinal
  32
  (list posit32-nar)
  (compose double->posit32 fl))

;;TODO correct functions for quire (incorrect now for testing)
(define-representation quire8
  (compose double->quire8 bigfloat->flonum)
  (compose bf quire8->double)
  (compose double->quire8 ordinal->flonum)
  (compose flonum->ordinal quire8->double)
  64
  null
  (compose double->quire8 fl))

(define-representation quire16
  (compose double->quire16 bigfloat->flonum)
  (compose bf quire16->double)
  (compose double->quire16 ordinal->flonum)
  (compose flonum->ordinal quire16->double)
  64
  null
  (compose double->quire16 fl))

(define-representation quire32
  (compose double->quire32 bigfloat->flonum)
  (compose bf quire32->double)
  (compose double->quire32 ordinal->flonum)
  (compose flonum->ordinal quire32->double)
  64
  null
  (compose double->quire32 fl))

;; Defining the operators

(require (submod "syntax.rkt" internals))

(declare-parametric-operator! '+ '+.p8 '(posit8 posit8) 'posit8)
(declare-parametric-operator! '+ '+.p16 '(posit16 posit16) 'posit16)
(declare-parametric-operator! '+ '+.p32 '(posit16 posit32) 'posit32)
(declare-parametric-operator! '- '-.p8 '(posit8 posit8) 'posit8)
(declare-parametric-operator! '- '-.p16 '(posit16 posit16) 'posit16)
(declare-parametric-operator! '- '-.p32 '(posit16 posit32) 'posit32)
(declare-parametric-operator! '- 'neg.p8 '(posit8) 'posit8)
(declare-parametric-operator! '- 'neg.p16 '(posit16) 'posit16)
(declare-parametric-operator! '- 'neg.p32 '(posit32) 'posit32)
(declare-parametric-operator! '* '*.p8 '(posit8 posit8) 'posit8)
(declare-parametric-operator! '* '*.p16 '(posit16 posit16) 'posit16)
(declare-parametric-operator! '* '*.p32 '(posit16 posit32) 'posit32)
(declare-parametric-operator! '/ '/.p8 '(posit8 posit8) 'posit8)
(declare-parametric-operator! '/ '/.p16 '(posit16 posit16) 'posit16)
(declare-parametric-operator! '/ '/.p32 '(posit16 posit32) 'posit32)
(declare-parametric-operator! 'sqrt 'sqrt.p8 '(posit8) 'posit8)
(declare-parametric-operator! 'sqrt 'sqrt.p16 '(posit16) 'posit16)
(declare-parametric-operator! 'sqrt 'sqrt.p32 '(posit32) 'posit32)
(declare-parametric-operator! '< '<.p8 '(posit8 posit8) 'bool)
(declare-parametric-operator! '< '<.p16 '(posit16 posit16) 'bool)
(declare-parametric-operator! '< '<.p32 '(posit32 posit32) 'bool)
(declare-parametric-operator! '<= '<=.p8 '(posit8 posit8) 'bool)
(declare-parametric-operator! '<= '<=.p16 '(posit16 posit16) 'bool)
(declare-parametric-operator! '<= '<=.p32 '(posit32 posit32) 'bool)
(declare-parametric-operator! '> '>.p8 '(posit8 posit8) 'bool)
(declare-parametric-operator! '> '>.p16 '(posit16 posit16) 'bool)
(declare-parametric-operator! '> '>.p32 '(posit32 posit32) 'bool)
(declare-parametric-operator! '>= '>=.p8 '(posit8 posit8) 'bool)
(declare-parametric-operator! '>= '>=.p16 '(posit16 posit16) 'bool)
(declare-parametric-operator! '>= '>=.p32 '(posit32 posit32) 'bool)
(declare-parametric-operator! '== '==.p8 '(posit8 posit8) 'bool)
(declare-parametric-operator! '== '==.p16 '(posit16 posit16) 'bool)
(declare-parametric-operator! '== '==.p32 '(posit32 posit32) 'bool)
(declare-parametric-operator! '!= '!=.p8 '(posit8 posit8) 'bool)
(declare-parametric-operator! '!= '!=.p16 '(posit16 posit16) 'bool)
(declare-parametric-operator! '!= '!=.p32 '(posit32 posit32) 'bool)

(define-operator (+.p8 posit8 posit8) posit8
  [fl posit8-add] [bf bf+] [ival #f] [cost 40]
  [->c/double (curry format "~a + ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a + ~a")]
  [nonffi posit8-add])

(define-operator (+.p16 posit16 posit16) posit16
  [fl posit16-add] [bf bf+] [ival #f] [cost 40]
  [->c/double (curry format "~a + ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a + ~a")]
  [nonffi posit16-add])

(define-operator (+.p32 posit32 posit32) posit32
  [fl posit32-add] [bf bf+] [ival #f] [cost 40]
  [->c/double (curry format "~a + ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a + ~a")]
  [nonffi posit32-add])

(define-operator (neg.p8 posit8) posit8
  [fl posit8-neg] [bf bf-] [ival #f] [cost 80]
  [->c/double (curry format "-~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "-~a")]
  [nonffi posit8-neg])

(define-operator (neg.p16 posit16) posit16
  [fl posit16-neg] [bf bf-] [ival #f] [cost 80]
  [->c/double (curry format "-~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "-~a")]
  [nonffi posit16-neg])

(define-operator (neg.p32 posit32) posit32
  [fl posit32-neg] [bf bf-] [ival #f] [cost 80]
  [->c/double (curry format "-~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "-~a")]
  [nonffi posit32-neg])

(define-operator (-.p8 posit8 posit8) posit8
  [fl posit8-sub] [bf bf-] [ival #f] [cost 80]
  [->c/double (curry format "~a - ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a - ~a")]
  [nonffi posit8-sub])

(define-operator (-.p16 posit16 posit16) posit16
  [fl posit16-sub] [bf bf-] [ival #f] [cost 80]
  [->c/double (curry format "~a - ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a - ~a")]
  [nonffi posit16-sub])

(define-operator (-.p32 posit32 posit32) posit32
  [fl posit32-sub] [bf bf-] [ival #f] [cost 80]
  [->c/double (curry format "~a - ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a - ~a")]
  [nonffi posit32-sub])

(define-operator (*.p8 posit8 posit8) posit8
  [fl posit8-mul] [bf bf*] [ival #f] [cost 320]
  [->c/double (curry format "~a * ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a \\cdot ~a")]
  [nonffi posit8-mul])

(define-operator (*.p16 posit16 posit16) posit16
  [fl posit16-mul] [bf bf*] [ival #f] [cost 320]
  [->c/double (curry format "~a * ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a \\cdot ~a")]
  [nonffi posit16-mul])

(define-operator (*.p32 posit32 posit32) posit32
  [fl posit32-mul] [bf bf*] [ival #f] [cost 320]
  [->c/double (curry format "~a * ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a \\cdot ~a")]
  [nonffi posit32-mul])

(define-operator (/.p8 posit8 posit8) posit8
  [fl posit8-div] [bf bf/] [ival #f] [cost 440]
  [->c/double (curry format "~a / ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi posit8-div])

(define-operator (/.p16 posit16 posit16) posit16
  [fl posit16-div] [bf bf/] [ival #f] [cost 440]
  [->c/double (curry format "~a / ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi posit16-div])

(define-operator (/.p32 posit32 posit32) posit32
  [fl posit32-div] [bf bf/] [ival #f] [cost 440]
  [->c/double (curry format "~a / ~a")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\frac{~a}{~a}")]
  [nonffi posit32-div])

(define-operator (sqrt.p8 posit8) posit8
  [fl posit8-sqrt] [bf bfsqrt] [ival #f] [cost 40]
  [->c/double (curry format "sqrt(~a)")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\sqrt{~a}")]
  [nonffi posit8-sqrt])

(define-operator (sqrt.p16 posit16) posit16
  [fl posit16-sqrt] [bf bfsqrt] [ival #f] [cost 40]
  [->c/double (curry format "sqrt(~a)")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\sqrt{~a}")]
  [nonffi posit16-sqrt])

(define-operator (sqrt.p32 posit32) posit32
  [fl posit32-sqrt] [bf bfsqrt] [ival #f] [cost 40]
  [->c/double (curry format "sqrt(~a)")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\sqrt{~a}")]
  [nonffi posit32-sqrt])

(define-operator (<.p8 posit8 posit8) bool
  ; Override number of arguments
  [type #hash((* . (((* posit8) bool))))] [args '(*)]
  [fl (comparator posit8<)] [bf (comparator bf<)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary <
  [->tex (infix-joiner " \\lt ")]
  [nonffi (comparator posit8<)])

(define-operator (<.p16 posit16 posit16) bool
  ; Override number of arguments
  [type #hash((* . (((* posit16) bool))))] [args '(*)]
  [fl (comparator posit16<)] [bf (comparator bf<)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary <
  [->tex (infix-joiner " \\lt ")]
  [nonffi (comparator posit16<)])

(define-operator (<.p32 posit32 posit32) bool
  ; Override number of arguments
  [type #hash((* . (((* posit32) bool))))] [args '(*)]
  [fl (comparator posit32<)] [bf (comparator bf<)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary <
  [->tex (infix-joiner " \\lt ")]
  [nonffi (comparator posit32<)])

(define-operator (>.p8 posit8 posit8) bool
  ; Override number of arguments
  [type #hash((* . (((* posit8) bool))))] [args '(*)]
  [fl (comparator posit8>)] [bf (comparator bf>)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary >
  [->tex (infix-joiner " \\gt ")]
  [nonffi (comparator posit8>)])

(define-operator (>.p16 posit16 posit16) bool
  ; Override number of arguments
  [type #hash((* . (((* posit16) bool))))] [args '(*)]
  [fl (comparator posit16>)] [bf (comparator bf>)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary >
  [->tex (infix-joiner " \\gt ")]
  [nonffi (comparator posit16>)])

(define-operator (>.p32 posit32 posit32) bool
  ; Override number of arguments
  [type #hash((* . (((* posit32) bool))))] [args '(*)]
  [fl (comparator posit32>)] [bf (comparator bf>)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary >
  [->tex (infix-joiner " \\gt ")]
  [nonffi (comparator posit32>)])

(define-operator (<=.p8 posit8 posit8) bool
  ; Override number of arguments
  [type #hash((* . (((* posit8) bool))))] [args '(*)]
  [fl (comparator posit8<=)] [bf (comparator bf<=)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary <=
  [->tex (infix-joiner " \\le ")]
  [nonffi (comparator posit8<=)])

(define-operator (<=.p16 posit16 posit16) bool
  ; Override number of arguments
  [type #hash((* . (((* posit16) bool))))] [args '(*)]
  [fl (comparator posit16<=)] [bf (comparator bf<=)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary <=
  [->tex (infix-joiner " \\le ")]
  [nonffi (comparator posit16<=)])

(define-operator (<=.p32 posit32 posit32) bool
  ; Override number of arguments
  [type #hash((* . (((* posit32) bool))))] [args '(*)]
  [fl (comparator posit32<=)] [bf (comparator bf<=)] [ival #f] [cost 65]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/*Error: no posit support in C */")] ; TODO: cannot handle variary <=
  [->tex (infix-joiner " \\le ")]
  [nonffi (comparator posit32<=)])

(define-operator (>=.p8 posit8 posit8) bool
  ; Override number of arguments
  [type #hash((* . (((* posit8) bool))))] [args '(*)]
  [fl (comparator posit8>=)] [bf (comparator bf>=)] [ival #f] [cost 65]
  [->c/double (curry format "/* Error: no posit support in C */")]
  [->c/mpfr (curry format "/* Error: no posit support in C */")] ; TODO: cannot handle variary >=
  [->tex (infix-joiner " \\ge ")]
  [nonffi (comparator posit8>=)])

(define-operator (>=.p16 posit16 posit16) bool
  ; Override number of arguments
  [type #hash((* . (((* posit16) bool))))] [args '(*)]
  [fl (comparator posit16>=)] [bf (comparator bf>=)] [ival #f] [cost 65]
  [->c/double (curry format "/* Error: no posit support in C */")]
  [->c/mpfr (curry format "/* Error: no posit support in C */")] ; TODO: cannot handle variary >=
  [->tex (infix-joiner " \\ge ")]
  [nonffi (comparator posit16>=)])

(define-operator (>=.p32 posit32 posit32) bool
  ; Override number of arguments
  [type #hash((* . (((* posit32) bool))))] [args '(*)]
  [fl (comparator posit32>=)] [bf (comparator bf>=)] [ival #f] [cost 65]
  [->c/double (curry format "/* Error: no posit support in C */")]
  [->c/mpfr (curry format "/* Error: no posit support in C */")] ; TODO: cannot handle variary >=
  [->tex (infix-joiner " \\ge ")]
  [nonffi (comparator posit32>=)])

(define-operator (real->posit8 real) posit8
  ; Override number of arguments
  [fl double->posit8] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/* ERROR: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->posit8])

(define-operator (real->posit16 real) posit16
  ; Override number of arguments
  [fl double->posit16] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/* ERROR: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->posit16])

(define-operator (real->posit32 real) posit32
  ; Override number of arguments
  [fl double->posit32] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/* ERROR: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->posit32])

(define-operator (posit8->real posit8) real
  ; Override number of arguments
  [fl posit8->double] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/* ERROR: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->posit8])

(define-operator (posit16->real posit16) real
  ; Override number of arguments
  [fl posit16->double] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/* ERROR: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->posit16])

(define-operator (posit32->real posit32) real
  ; Override number of arguments
  [fl posit32->double] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/* ERROR: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->posit32])

(define-operator (real->quire8 real) quire8
  ; Override number of arguments
  [fl double->quire8] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->quire8])

(define-operator (real->quire16 real) quire16
  ; Override number of arguments
  [fl double->quire16] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->quire16])

(define-operator (real->quire32 real) quire32
  ; Override number of arguments
  [fl double->quire32] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->quire32])

(define-operator (quire8->real quire8) real
  ; Override number of arguments
  [fl quire8->double] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->quire8])

(define-operator (quire16->real quire16) real
  ; Override number of arguments
  [fl quire16->double] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->quire16])

(define-operator (quire16->real quire16) real
  ; Override number of arguments
  [fl quire16->double] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi double->quire32])

(define (bffdp x y z) (bf+ x (bf* y z)))
(define (bffdm x y z) (bf- x (bf* y z)))

(define-operator (quire8-mul-add quire8 posit8 posit8) quire8
  ; Override number of arguments
  [fl quire8-fdp-add] [bf bffdp] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\mathsf{qma}\\left(~a, ~a, ~a\\right)")]
  [nonffi double->quire32])

(define-operator (quire16-mul-add quire16 posit16 posit16) quire16
  ; Override number of arguments
  [fl quire16-fdp-add] [bf bffdp] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\mathsf{qma}\\left(~a, ~a, ~a\\right)")]
  [nonffi double->quire32])

(define-operator (quire32-mul-add quire32 posit32 posit32) quire32
  ; Override number of arguments
  [fl quire32-fdp-add] [bf bffdp] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\mathsf{qma}\\left(~a, ~a, ~a\\right)")]
  [nonffi double->quire32])

(define-operator (quire8-mul-sub quire8 posit8 posit8) quire8
  ; Override number of arguments
  [fl quire8-fdp-sub] [bf bffdm] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\mathsf{qms}\\left(~a, ~a, ~a\\right)")]
  [nonffi double->quire8])

(define-operator (quire16-mul-sub quire16 posit16 posit16) quire16
  ; Override number of arguments
  [fl quire16-fdp-sub] [bf bffdm] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\mathsf{qms}\\left(~a, ~a, ~a\\right)")]
  [nonffi double->quire16])

(define-operator (quire32-mul-sub quire32 posit32 posit32) quire32
  ; Override number of arguments
  [fl quire32-fdp-sub] [bf bffdm] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "\\mathsf{qms}\\left(~a, ~a, ~a\\right)")]
  [nonffi double->quire32])

(define-operator (quire8->posit8 quire8) posit8
  ; Override number of arguments
  [fl quire8->posit8] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi quire8->posit8])

(define-operator (quire16->posit16 quire16) posit16
  ; Override number of arguments
  [fl quire16->posit16] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi quire16->posit16])

(define-operator (quire32->posit32 quire32) posit32
  ; Override number of arguments
  [fl quire32->posit32] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi quire32->posit32])

(define-operator (posit8->quire8 posit8) quire8
  ; Override number of arguments
  [fl posit8->quire8] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi posit8->quire8])

(define-operator (posit16->quire16 posit16) quire16
  ; Override number of arguments
  [fl posit16->quire16] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi posit16->quire16])

(define-operator (posit32->quire32 posit32) quire32
  ; Override number of arguments
  [fl posit32->quire32] [bf identity] [ival #f] [cost 0]
  [->c/double (const "/*Error: no posit support in C */")]
  [->c/mpfr (const "/* ERROR: no posit support in C */")]
  [->tex (curry format "~a")]
  [nonffi posit32->quire32])

;; Defining the rules

(require (submod "rules.rkt" internals))

(define-ruleset commutativity.p16 (arithmetic simplify posit)
  #:type ([a posit16] [b posit16])
  [+-commutative     (+.p16 a b)               (+.p16 b a)]
  [*-commutative     (*.p16 a b)               (*.p16 b a)])

; Posit conversions
(define-ruleset insert-p16 (arithmetic posit)
  #:type ([a real])
  [insert-posit16 a (posit16->real (real->posit16 a))])

(define-ruleset remove-p16 (arithmetic simplify posit)
  #:type ([a real])
  [remove-posit16 (posit16->real (real->posit16 a)) a])

(define-ruleset id-p16 (arithmetic simplify posit)
  #:type ([a posit16])
  [+p16-lft-identity-reduce    (+.p16 (real->posit16 0.0) a)               a]
  [+p16-rgt-identity-reduce    (+.p16 a (real->posit16 0.0))               a]
  [-p16-rgt-identity-reduce    (-.p16 a (real->posit16 0.0))               a]
  [*p16-lft-identity-reduce    (*.p16 (real->posit16 1.0) a)               a]
  [*p16-rgt-identity-reduce    (*.p16 a (real->posit16 1.0))               a]
  [/p16-rgt-identity-reduce    (/.p16 a (real->posit16 1.0))               a])

(define-ruleset unid-p16 (arithmetic posit)
  #:type ([a posit16])
  [+p16-lft-identity-expand    a               (+.p16 (real->posit16 0.0) a)]
  [+p16-rgt-identity-expand    a               (+.p16 a (real->posit16 0.0))]
  [-p16-rgt-identity-expand    a               (-.p16 a (real->posit16 0.0))]
  [*p16-lft-identity-expand    a               (*.p16 (real->posit16 1.0) a)]
  [*p16-rgt-identity-expand    a               (*.p16 a (real->posit16 1.0))]
  [/p16-rgt-identity-expand    a               (/.p16 a (real->posit16 1.0))])

;; TODO: Multiply add to mulAdd

;; TODO: We only cast back to posit after quire operations because herbie can't handle
;; non-double output right now (similar situtation for posits)
(define-ruleset q16-arithmetic (arithmetic posit)
  #:type ([a posit16] [b posit16] [c posit16] [q quire16])
  [introduce-quire      a               (quire16->posit16 (posit16->quire16 a))]
  [insert-quire-add     (+.p16 (quire16->posit16 q) a)
                        (quire16->posit16 (quire16-mul-add q a (real->posit16 1.0)))]
  [insert-quire-sub     (-.p16 (quire16->posit16 q) a)
                        (quire16->posit16 (quire16-mul-sub q a (real->posit16 1.0)))]
  [insert-quire-fdp-add (+.p16 (quire16->posit16 q) (*.p16 a b))
                        (quire16->posit16 (quire16-mul-add q a b))]
  [insert-quire-fdp-sub (-.p16 (quire16->posit16 q) (*.p16 a b))
                        (quire16->posit16 (quire16-mul-sub q a b))])

(define-ruleset p16-test-rules (arithmetic posit)
  #:type ([a posit16] [b posit16] [c posit16] [d posit16])
  [p16-flip--            (-.p16 a b)                            (/.p16 (-.p16 (*.p16 a a) (*.p16 b b)) (+.p16 a b))]
  [p16-*-un-lft-identity a                                      (*.p16 (real->posit16 1.0) a)]
  [p16-distribute-lft-out     (+.p16 (*.p16 a b) (*.p16 a c))   (*.p16 a (+.p16 b c))]
  [p16-times-frac  (/.p16 (*.p16 a b) (*.p16 c d))              (*.p16 (/.p16 a c) (/.p16 b d))]
  [sqrt-sqrd.p16   (*.p16 (sqrt.p16 a) (sqrt.p16 a))             a]
  [remove-negate.p16 (+.p16 a (-.p16 (real->posit16 1.0) a))    (real->posit16 1.0)])

(define-ruleset associativity.p16 (arithmetic simplify posit)
  #:type ([a posit16] [b posit16] [c posit16])
  [associate-+r+  (+.p16 a (+.p16 b c))         (+.p16 (+.p16 a b) c)]
  [associate-+l+  (+.p16 (+.p16 a b) c)         (+.p16 a (+.p16 b c))]
  [associate-+r-  (+.p16 a (-.p16 b c))         (-.p16 (+.p16 a b) c)]
  [associate-+l-  (+.p16 (-.p16 a b) c)         (-.p16 a (-.p16 b c))]
  [associate--r+  (-.p16 a (+.p16 b c))         (-.p16 (-.p16 a b) c)]
  [associate--l+  (-.p16 (+.p16 a b) c)         (+.p16 a (-.p16 b c))]
  [associate--l-  (-.p16 (-.p16 a b) c)         (-.p16 a (+.p16 b c))]
  [associate--r-  (-.p16 a (-.p16 b c))         (+.p16 (-.p16 a b) c)]
  [associate-*r*  (*.p16 a (*.p16 b c))         (*.p16 (*.p16 a b) c)]
  [associate-*l*  (*.p16 (*.p16 a b) c)         (*.p16 a (*.p16 b c))]
  [associate-*r/  (*.p16 a (/.p16 b c))         (/.p16 (*.p16 a b) c)]
  [associate-*l/  (*.p16 (/.p16 a b) c)         (/.p16 (*.p16 a c) b)]
  [associate-/r*  (/.p16 a (*.p16 b c))         (/.p16 (/.p16 a b) c)]
  [associate-/l*  (/.p16 (*.p16 b c) a)         (/.p16 b (/.p16 a c))]
  [associate-/r/  (/.p16 a (/.p16 b c))         (*.p16 (/.p16 a b) c)]
  [associate-/l/  (/.p16 (/.p16 b c) a)         (/.p16 b (*.p16 a c))]
  [sub-neg        (-.p16 a b)                   (+.p16 a (neg.p16 b))]
  [unsub-neg      (+.p16 a (neg.p16 b))         (-.p16 a b)])

(define-ruleset distributivity.p16 (arithmetic simplify posit)
  #:type ([a posit16] [b posit16] [c posit16])
  [distribute-lft-in      (*.p16 a (+.p16 b c))           (+.p16 (*.p16 a b) (*.p16 a c))]
  [distribute-rgt-in      (*.p16 a (+.p16 b c))           (+.p16 (*.p16 b a) (*.p16 c a))]
  [distribute-lft-out     (+.p16 (*.p16 a b) (*.p16 a c))   (*.p16 a (+.p16 b c))]
  [distribute-lft-out--   (-.p16 (*.p16 a b) (*.p16 a c))   (*.p16 a (-.p16 b c))]
  [distribute-rgt-out     (+.p16 (*.p16 b a) (*.p16 c a))   (*.p16 a (+.p16 b c))]
  [distribute-rgt-out--   (-.p16 (*.p16 b a) (*.p16 c a))   (*.p16 a (-.p16 b c))]
  [distribute-lft1-in     (+.p16 (*.p16 b a) a)           (*.p16 (+.p16 b (real->posit16 1.0)) a)]
  [distribute-rgt1-in     (+.p16 a (*.p16 c a))           (*.p16 (+.p16 c (real->posit16 1.0)) a)])

(define-ruleset difference-of-squares-canonicalize.p16 (polynomials simplify posit)
  #:type ([a posit16] [b posit16])
  [difference-of-squares (-.p16 (*.p16 a a) (*.p16 b b))   (*.p16 (+.p16 a b) (-.p16 a b))]
  [difference-of-sqr-1   (-.p16 (*.p16 a a) (real->posit16 1.0))
                         (*.p16 (+.p16 a (real->posit16 1.0)) (-.p16 a (real->posit16 1.0)))]
  [difference-of-sqr--1  (+.p16 (*.p16 a a) (real->posit16 -1.0))
                         (*.p16 (+.p16 a (real->posit16 1.0)) (-.p16 a (real->posit16 1.0)))])

(define-ruleset exact-posit16 (arithmetic simplify posit fp-safe-nan)
  #:type ([a posit16])
  [+-inverses.p16    (-.p16 a a)                                 (real->posit16 0.0)]
  [*-inverses.p16    (/.p16 a a)                                 (real->posit16 1.0)]
  [div0.p16          (/.p16 (real->posit16 0.0) a)               (real->posit16 0.0)]
  [mul0.p16          (*.p16 (real->posit16 0.0) a)               (real->posit16 0.0)]
  [mul0.p16          (*.p16 a (real->posit16 0.0))               (real->posit16 0.0)])

(define-ruleset id-reduce-posit16 (arithmetic simplify posit)
  #:type ([a posit16])
  [remove-double-div.p16 (/.p16 (real->posit16 1.0) (/.p16 (real->posit16 1.0) a))         a]
  [rgt-mult-inverse.p16  (*.p16 a (/.p16 (real->posit16 1.0) a))         (real->posit16 1.0)]
  [lft-mult-inverse.p16  (*.p16 (/.p16 (real->posit16 1.0) a) a)         (real->posit16 1.0)])
