#lang racket

;; Minimal interval type used by Herbie when interacting with Rival
;; Replaces dependency on Racket Rival's interval module.

(provide ival
         ival?
         ival-lo
         ival-hi)

(struct ival (lo hi) #:prefab)
