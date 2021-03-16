#lang info

(define name "float32-herbie")

(define pre-install-collection "install.rkt")
(define compile-omit-files '("install.rkt"))
(define move-foreign-libs '("float32-arith.so"))
(define pkg-desc "Arithmetic functions for single floats")
(define version "1.0")
(define pkg-authors '(Brett Saiki))