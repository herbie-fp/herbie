# Herbie's FFI to egraphs-good/egg

This is a small Rust+Racket package co-developed with Herbie so that
Herbie can bind to and use the [egg](https://egraphs-good.github.io)
equality saturation library.

The Racket side is in [`main.rkt`](main.rkt). It does only two things:
find the `libegg_math` shared library and expose its main functions to
Racket. It has a bit of logic for detecting architecture mismatches
due to Rosetta Apple Silicon.

The Rust side is implemented in standard Rust using the `egg` library.
The `math` module contains math-specific implementation work while the
`lib` module contains code to interface with Racket.

The main Herbie repository's Github Actions build and publish versions
of the Racket package (including pre-built Rust libraries).
