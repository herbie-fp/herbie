#lang info

(define collection "rival")
(define version "2.2")
(define license 'MIT)

;; Packaging information

(define pkg-desc "Interval arithmetic for real computation")
(define pkg-authors '("Pavel Panchekha" "Oliver Flatt" "Artem Yadrov"))

(define compile-omit-paths '("infra"))
(define test-omit-paths '("infra"))

;; Dependencies
(define deps '(("base" #:version "8.0") "math-lib" "rackunit-lib" "profile-lib"))
(define build-deps '("rackunit-lib" "scribble-lib" "racket-doc" "math-doc" "sandbox-lib"))
(define scribblings '(("scribblings/rival.scrbl" (multi-page) (library))))
