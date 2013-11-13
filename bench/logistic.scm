(lambda (w.x y)
  (if (> w.x 0)
      (- (/ 1 (+ 1 (exp (- w.x)))) y)
      (- (- 1 y) (/ 1 (1+ (exp w.x))))))

(lambda (w.x y)
  (if (<= w.x 0)
      ;;cases are flipped.
      (- (log (1+ (exp w.x))) (* w.x y))
      (- (log (1+ (exp (- w.x)))) (* (- w.x) (- 1 y)))))
