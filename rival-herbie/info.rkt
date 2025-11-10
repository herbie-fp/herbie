#lang info

(define collection "rival-herbie")
(define version "0.1.0")

(define pkg-desc "Racket FFI bindings to Rust Rival for Herbie")
(define pkg-authors `("Herbie Team"))

(define build-deps '("rackunit-lib"))
(define deps '(("base" #:version "8.0") "math-lib"))

(define license 'MIT)

