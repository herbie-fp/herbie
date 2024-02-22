#lang racket

;;; The AVX platform:
;;; Costs taken from Skylake AVX instructions


(require "../plugin.rkt")

; universal boolean opertaions
(define boolean-platform
  (with-terminal-cost ([bool 1])
         (platform
      #:default-cost 1
      #:if-cost 1
      [(bool) (TRUE FALSE)]
      [(bool bool) not]
      [(bool bool bool) (and or)])))

; machine floating-point operations (without conversions)
(define machine-platform
  (with-terminal-cost ([binary64 64] [binary32 32])
    (let ([rel-costs (cost-map
                        [(PI E INFINITY NAN) 7]
                        [(+) 4]
                        [(-) 4]    
                        [(*) 4]
                        [(/) 14]
                        ;[(neg)1]
                        [(== != > < >= <=) 4])])
      (platform-product
        [([real binary64] [bool bool]) (cost-map-scale 64 rel-costs)]
        [([real binary32] [bool bool]) (cost-map-scale 32 rel-costs)]
        (operator-set
          [(real) (PI E INFINITY NAN)]
          ;[(real real) neg]
          [(real real real) (+ - * /)]
          [(real real bool) (== != > < >= <=)])))))

; libm operations
(define libm-platform 
  (with-terminal-cost ([binary64 64] [binary32 32])
    (let ([rel-costs (cost-map
                        [(ceil) 8]
                        [(floor) 4]    
                        [(sqrt) 12]
                        [(round) 8]
                        [(fmax) 4]
                        [(fmin) 4]
                        )
                     ])
      (platform-product #:optional
        [([real binary64]) (cost-map-scale 64 rel-costs)]
        [([real binary32]) (cost-map-scale 32 rel-costs)] 
        (operator-set 
          [(real real)
           (ceil floor sqrt round)]
          [(real real real)
           (fmax fmin)])))))

; compose platforms

(define hardware-platform
  (platform-union boolean-platform
                  machine-platform))

(define default-platform
  (platform-union boolean-platform
                  machine-platform
                  libm-platform))

; Register all three

(register-platform! 'boolean boolean-platform)
(register-platform! 'hardware hardware-platform)
(register-platform! 'default default-platform)

;; Do not run this file during testing
(module test racket/base)
