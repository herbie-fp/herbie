Extracted Test Suite from Math.js
=================================

Math.js is a math library for JavaScript.  It supports matrices,
complex numbers, and basic units.  All code appears to be written from
scratch and cites no literature beyond Wikipedia.

All code examples from Math.js, which can be found on Github at
https://github.com/josdejong/mathjs/blob/master/.  All code is
distributed under an Apache 2 license.  All floating point code in
`lib/` was extracted, except the `lib/matrix/` determinant and inverse
routines (contains complex control flow) and the `lib/trigonometry/`
functions `acos`, `asin`, `atan`, `cot`, `csc`, `sec`, and `tan`
(expansion was too large).

= To Do =

Finish extracting the `lib/trigonometry` functions missed above; it
simply requires a lot of manual work or an automatic tool to rewrite
`let` blocks into equivalent call-by-name blocks.
