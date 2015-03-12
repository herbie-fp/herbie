
;; I would use 'e' for the error term, but that actually has special
;; meaning as the constant e, and would break things if I used it.
(herbie-test ([lst (list 20 double .25)])
  "Mean of a list"
  (do-list ([sum 0.0 (+ item sum)])
           ([item lst])
    (/ sum (length lst)))
  (do-list ([sum 0.0 (+ sum (- item c))]
            [c 0.0 (- (- (+ sum (- item c)) sum) (- item c))])
           ([item lst])
    (/ (- sum c) (length lst))))

(herbie-test ([lst (list 20 double .25)])
  "Variance 1"
  (do-list ([sum 0.0 (+ item sum)])
           ([item lst])
    (let ([mean (/ sum (length lst))])
      (do-list ([var 0.0 (+ (* (- item mean) (- item mean))
                            var)])
               ([item lst])
        (/ var (length lst)))))
  (do-list ([sum 0.0 (+ sum (- item c))]
            [c 0.0 (- (- (+ sum (- item c)) sum) (- item c))])
           ([item lst])
    (let* ([mean (/ sum (length lst))]
           [dm (/ (+ sum (- (* mean (- (length lst))) c)) (length lst))])
      (do-list ([v 0.0 (let* ([t1 (- (- item mean) dm)]
                              [t2 (+ (/ (* t1 t1) (length lst)) dv)])
                         (+ v t2))]
                [dv 0.0 (let* ([t1 (- (- item mean) dm)]
                               [t2 (+ (/ (* t1 t1) (length lst)) dv)]
                               [v2 (+ v t2)])
                          (+ (- v v2) t2))])
               ([item lst])
         v))))

(herbie-test ([lst (list 20 double .25)])
  "Variance 2"
  (do-list ([sum 0.0 (+ sum item)]
            [sqrsum 0.0 (+ sqrsum (sqr item))])
           ([item lst])
    (- (/ sqrsum (length lst)) (sqr (/ sum (length lst)))))
  (do-list ([sum 0.0 (+ sum (- item c1))]
            [sqrsum 0.0 (+ sqrsum (- (sqr item) c2))]
            [c1 0.0 (- (- (+ sum (- item c1)) sum) (- item c1))]
            [c2 0.0 (- (- (+ sqrsum (- (sqr item) c2)) sqrsum) (- (sqr item) c2))])
           ([item lst])
    (- (/ (- sqrsum c2) (length lst))
       (sqr (/ (- sum c1) (length lst))))))

(herbie-test ([lst (list 20 double .25)])
  "Variance 3"
  (do-list ([mean (car lst) (let ([d (/ (- item mean) n)])
                              (+ mean d))]
            [var 0.0 (let* ([d (/ (- item mean) n)]
                            [d2 (/ (- (* d (* d (* n (- n 1)))) var) n)])
                       (+ var d2))]
            [n 2 (+ n 1)])
           ([item (cdr lst)])
    var)
  (do-list ([mean (car lst) (let ([d (/ (- (- item mean) emean) n)])
                              (+ mean (+ d emean)))]
            [emean 0.0 (let* ([d (/ (- (- item mean) emean) n)]
                              [mean* (+ mean (+ d emean))])
                         (+ (- mean mean*) (+ d emean)))]
            [var 0.0 (let* ([d (/ (- (- item mean) emean) n)]
                            [d2 (/ (- (- (* d (* d (* n (- n 1)))) var) evar) n)])
                       (+ var (+ d2 evar)))]
            [evar 0.0 (let* ([d (/ (- (- item mean) emean) n)]
                             [d2 (/ (- (- (* d (* d (* n (- n 1)))) var) evar) n)]
                             [var* (+ var (+ d2 evar))])
                        (+ (- var var*) (+ d2 evar)))]
            [n 2 (+ n 1)])
           ([item (cdr lst)])
     var))
