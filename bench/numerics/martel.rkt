; From  Matthieu Martel,
; Semantics-Based Transformation of Arithmetic Expressions,
; Static Analysis Symposium, SAS'07,
; Lecture Notes in Computer Science, Springer-Verlag, 4634, 2007 (pdf)

(herbie-test ([d 2] [a 1] [b 1/9] [c 1/9])
  "Rectangular parallelepiped of dimension a×b×c"
  (* d (+ (* a b) (* b c) (* c a)))
  (+ (+ (* (* c a) d) (* d (* b c))) (* d (* a b))))

(herbie-test ([a (uniform 56789 98765)] [b (<= 0 default 1)]
              [c (<= 0 default 1.6773e-3)] [d (<= 0 default 1.6773e-3)])
  "Expression, p14"
  (* a (+ (+ b c) d))
  (+ (* a b) (* a (+ c d))))

(herbie-test ([a (uniform 1 2)] [b (uniform 2 4)] [c (uniform 4 8)]
              [d (uniform 8 16)] [e (uniform 16 32)])
  "Expression 1, p15"
  (+ (+ (+ (+ e d) c) b) a)
  (+ (+ d (+ c (+ a b))) e))

(herbie-test ([x (<= 0 default 2)])
  "Expression 2, p15"
  (+ x (* x x))
  (* (+ 1.0 x) x))

(herbie-test ([x (<= 0 default 2)])
  "Expression 3, p15"
  (+ (* x (* x x)) (* x x))
  (* (* (+ 1.0 x) x) x))

(herbie-test ([a (<= 5 default 10)] [b (<= 0 default 0.001)])
  "Expression 4, p15"
  (* (+ a b) (+ a b))
  (+ (+ (+ (* b a) (* b b)) (* b a)) (* a a)))

; From Matthieu Martel,
; Enhancing the Implementation of Mathematical Formulas for Fixed-Point and Floating-Point Arithmetics,
; Journal of Formal Methods in System Design, volume 35, pages 265-278, 2009, Springer (pdf)

; Fixed point expressions not used, as Herbie does not support fixed point.

(herbie-test ([a (uniform -14 -13)] [b (uniform -3 -2)]
   [c (uniform 3 3.5)] [d (uniform 12.5 13.5)])
  "Expression, p6"
  (let* ([e 2])
    (* (+ a (+ b (+ c d))) e))
  (let* ([e 2])
    (+ (* (+ a b) e) (* (+ c d) e))))

; From Arnault Ioualalen and Matthieu Martel,
; Synthesizing accurate floating-point formulas,
; 24th International Conference on Application-Specific Systems, Architectures and Processors, ASAP 2013,
; Washington, DC, USA, June 5-7, 2013 (pdf)
