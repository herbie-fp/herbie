#lang racket

(require math/bigfloat math/flonum) 
(require "../common.rkt" "../points.rkt" "../float.rkt" "../programs.rkt"
         "../syntax/types.rkt" "../syntax/syntax.rkt" "../syntax/read.rkt"
         "../alternative.rkt" "../core/bsearch.rkt" "../sandbox.rkt")

(provide real->ordinal regime-splitpoints choose-ticks regime-var) 

;;  Repr conversions

(define (ordinal->real x repr)
  (repr->real ((representation-ordinal->repr repr) x) repr))

(define (real->ordinal x repr) 
  ((representation-repr->ordinal repr) (real->repr x repr))) 

(define (first-power10 min max repr)
  (define value
    (cond
     [(negative? max) 
      (- (expt 10 (ceiling (/ (log (- max)) (log 10)))))]
    [else
      (expt 10 (floor (/ (log max) (log 10))))]))
  (if (<= value min) #f value))

(define (clamp x lo hi)
  (min hi (max x lo)))

(define (choose-between min max number repr)
  ; Returns a given number of ticks, roughly evenly spaced, between min and max
  ; For any tick, n divisions below max, the tick is an ordinal corresponding to:
  ;  (a) a power of 10 between n and (n + ε) divisions below max where ε is some tolerance, or
  ;  (b) a value, n divisions below max
  (define sub-range (round (/ (- max min) (add1 number))))
  (define near (λ (x n) (and (<= x n) (<= (abs (/ (- x n) sub-range)) 0.2)))) ; <- tolerance
  (for/list ([itr (in-range 1 (add1 number))])
    (define power10 
      (first-power10 (ordinal->real (clamp (- max (* (add1 itr) sub-range)) min max) repr)
                     (ordinal->real (clamp (- max (* itr sub-range)) min max) repr)
                     repr))
    (if (and power10 (near (real->ordinal power10 repr) (- max (* itr sub-range))))
        (real->ordinal power10 repr)
        (- max (* itr sub-range)))))

(define (pick-spaced-ordinals necessary min max number repr)
  (define sub-range (/ (- max min) number)) ; size of a division on the ordinal range
  (define necessary*      ; filter out necessary points that are too close
    (let loop ([necessary necessary])
      (cond
       [(< (length necessary) 2) necessary]
       [(< (- (cadr necessary) (car necessary)) sub-range)
        (loop (cdr necessary))]
       [else (cons (car necessary) (loop (cdr necessary)))])))
  (define all
    (let loop ([necessary necessary*] [min* min] [start 0])
      (cond
       [(>= start number) '()]
       [(empty? necessary)
        (choose-between min* max (- number start) repr)]
       [else
        (define idx (for/first ([i (in-range number)] 
                                #:when (<= (- (first necessary) (+ min (* i sub-range))) sub-range))
                        i))
        (append
          (choose-between min* (first necessary) (- idx start) repr)
          (loop (cdr necessary) (first necessary) (add1 idx)))])))
  (sort (append all necessary*) <))

(define (choose-ticks min max repr)
  (define tick-count 13)
  (define necessary (map (curryr real->ordinal repr) 
                         (filter (λ (x) (<= min x max)) (list min -1.0 0.0 1.0 max))))
  (map
    (curryr ordinal->real repr)
    (pick-spaced-ordinals necessary (real->ordinal min repr) (real->ordinal max repr)
                          tick-count repr)))

(define/contract (regime-info altn)
  (-> alt? (or/c (listof sp?) #f))
  (let loop ([altn altn])
    (match altn
      [(alt _ `(regimes ,splitpoints) prevs) splitpoints]
      [(alt _ _ (list)) #f]
      [(alt _ _ (list prev _ ...)) (loop prev)])))

(define (regime-splitpoints altn)
  (map sp-point (drop-right (regime-info altn) 1)))

(define/contract (regime-var altn)
  (-> alt? (or/c expr? #f))
  (define info (regime-info altn))
  (and info (sp-bexpr (car info))))
