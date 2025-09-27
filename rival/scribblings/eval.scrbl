#lang scribble/manual

@title{Real Computation}

Rival's main use case is to evaluate real-number expressions to
correctly-rounded floating-point outputs. To do so, you first compile
your real-number expression to a "rival machine", and then apply that
machine to specific inputs.

@section{Supported Real-Number Operation}

Rival supports a simple language of real-number expressions containing
variables, rational literals, common mathematical functions, and
common mathematical constants:

@racketgrammar*[
  #:literals (real? symbol?)
  [rival-expr?
   variable
   literal
   (constant)
   (operator rival-expr ...)]
  [variable symbol?]
  [literal real?]
  [constant TRUE FALSE PI E]
  [operator
   (code:line + - * / fma fabs)
   (code:line sqrt cbrt hypot)
   (code:line exp exp2 expm1 pow)
   (code:line log log2 log10 log1p logb)
   (code:line sin cos tan asin acos atan atan2)
   (code:line sinh cosh tanh asinh acosh atanh)
   (code:line erf erfc lgamma tgamma)
   (code:line fmod remainder rint round ceil floor trunc)
   (code:line fmin fmax copysign fdim)
   (code:line < <= > >= == !=)
   (code:line if and or not)
   (code:line assert error)]]
   
Expressions largely follow the semantics of @code{math.h}, not Racket,
when it comes to, for example, the order of arguments to
@racket[atan2] or the naming of the exponential function.

Some inputs are invalid to some operations, such as division by zero,
square roots of negative numbers, and similar. For @code{pow}, Rival
considers @code{(pow 0 x)} valid for non-negative @code{x}, and
@code{(pow x y)} invalid for negative @code{x} and non-integer
@code{y}. In general these conventions again follow those in
@code{math.h}. Colloquially we say that these expressions "throw" on
invalid points, though note that internally Rival uses error intervals
to soundly track whether an input is invalid or not.
   
Expressions that mix boolean and real-number operations must
type-check in the expected way, and variables must have consistent
types. Rival does not perform typechecking; that is a user
responsibility, and Rival may return undefined results if passed
ill-typed formulas.

The @racket[assert] and @racket[error] operators need additional
explanation; these control the definition of a "valid" input to an
expression. The @racket[assert] function takes in a boolean input and
returns a boolean output. If the input is false, @racket[assert]
throws. Its output is always true. @racket[error?] has the opposite
behavior. This function never throws, and instead returns true if its
argument throws and false if it doesn't.

@racket[assert] and @racket[error] can be used to model constructs like
preconditions, tests, @code{try}/@code{catch} blocks, and others.

@section{Compiling Real Expressions}

To define the floating-point format that Rival is supposed to compute
the output in, you provide a "discretization".

@defstruct*[discretization
  ([target integer?]
   [convert (-> (or/c bigfloat? boolean?) T)]
   [distance (-> T T integer?)])]{
A discretization represents some subset of the real numbers
  (for example, @racket[flonum?]).
The @racket[target] describes
  the @racket[bf-precision] needed
  to exactly represent a value in the subset,
  the @racket[convert] function converts
  a bigfloat value to a value in the subset,
  and the @racket[distance]
  function determines how close two values in the subset are.
A distance of @racket[0] indicates that the two values are equal.
A distance of @racket[2] or greater indicates
  that the two values are far apart.
A value of exactly @racket[1] indicates
  that the two values are sequential, that is,
  that they share a rounding boundary.
This last case triggers special behavior inside Rival
  to handle double-rounding issues.
Note that (the absolute value of) @racket[flonums-between?]
  already returns values that fit these requirements.
}

The typical use case is evaluating an expression to double-precision
floating-point:

@deftogether[(
  @defthing[flonum-discretization discretization?]
  @defthing[bool-discretization discretization?]
  @defproc[(bf-discretization [precision natural? (bf-precision)]) discretization?]
)]{
Use @racket[flonum-discretization] to evaluate expressions
to double precision and @racket[bool-discretization] to evaluate
boolean expressions.
}

Once you have the appropriate discretization, you can compile your
real expression using @racket[rival-compile].

@defproc[
  (rival-compile
    [exprs (listof rival-expr?)]
    [vars (listof symbol?)]
    [discretizations (listof discretization)])
  rival-machine?]{
@racket[exprs] is a list of real-number expressions, using the grammar described above.
@racket[vars] is a list of the free variables of these expressions.
An empty @racket[vars] list can be provided if the expressions have no free variables.
@racket[discretizations] is a list of discretizations to use,
  one per expression. @racket[exprs] and @racket[discretizations] must be the same length.
Returns a @racket[rival-machine?], an opaque type that can be passed to
  @racket[rival-apply] to evaluate the compiled real expression
  on a specific point.
}

Internally, @racket[rival-compile] converts the @racket[exprs] into a
simple register machine. @racket[rival-compile] is fairly slow, so the
ideal use case for Rival is to compile a function once and then apply
it to multiple points.

If more than one expression is provided as an input to
@racket[rival-compile], common subexpressions will be identified and
eliminated during compilation. This makes Rival ideal for evaluting
large families of related expressions, a feature that is heavily used
in @hyperlink["https://herbie.uwplse.org"]{Herbie}. Note that each
expression can use a different discretization.

@section{Evaluating Real Expressions}

Once a real expression has been compiled to a @racket[rival-machine?], it
can be evaluated at specific input points using @racket[rival-apply].

@defproc[
  (rival-apply
    [machine rival-machine?]
    [point (vectorof (or/c bigfloat? boolean?))])
  (vectorof T)]{
Evaluates the compiled real expression on an input point represented
as a vector of bigfloats. @racket[point] must be the same length as
the list @racket[vars] passed to @racket[rival-compile]. The output
is a vector of output values of the same length as the @racket[exprs]
passed to @racket[rival-compile]. Moreover, the output values are
the same types @racket[T] as used in the corresponding @racket[discretization].

If the @racket[point] is an invalid input to at least one of the
@racket[exprs] passed to @racket[rival-compile], @racket[rival-apply]
raises @racket[exn:rival:invalid]. If Rival is unable to evaluate 
at least one of the @racket[exprs] on the @racket[point],
@racket[rival-apply] raises @racket[exn:rival:unsamplable].
}

@deftogether[(
  @defstruct*[(exn:rival exn:fail) ()]
  @defstruct*[(exn:rival:invalid exn:fail) ([pt (vectorof (or/c bigfloat? boolean?))])]
  @defstruct*[(exn:rival:unsamplable exn:fail) ([pt (vectorof (or/c bigfloat? boolean?))])]
)]{}

Note that @racket[rival-apply] will only return a result if it can prove
that it has correctly-rounded the output, and it will only throw an
@racket[exn:rival:invalid] exception if it can prove that at least
one of the output expressions in the @racket[machine] throws on the
given input. In all other cases

Internally, @racket[rival-apply] executes the register machine stored
in @racket[machine] repeatedly, at ever-higher @racket[bf-precision],
until the output intervals are narrow enough that both endpoints
@racket[convert] to outputs @racket[distance] 0 apart in the requested
@racket[discretization]. The precision is chosen through a
fairly-sophisticated process that can execute different operations in
different precisions to maximize performance. Caching avoids
re-executing instructions whose precision needs haven't changed.
Detailed execution information can be found via
@racket[racket-profile].

@deftogether[(
  @defparam[*rival-max-precision* precision natural? #:value 10000]
  @defparam[*rival-max-iterations* iterations natural? #:value 5]
)]

Sets the maximum precision used by Rival internally and the maximum
iterations performed by Rival before raising the
@racket[exn:rival:unsamplable] error. Note that Rival will stop at
whichever of @racket[*rival-max-precision*] or
@racket[*rival-max-iterations*] is hit first. For example, Rival may
execute the maximum number of iterations before reaching the maximum
precision, or choose to use a precision over the maximum before
reaching the maximum number of iterations. The default values are
likely sufficient for most use cases.

@defproc[
  (rival-analyze
    [machine rival-machine?]
    [input-ranges (vectorof ival?)])
  ival?]{
Returns a boolean interval which indicates whether a call to @racket[rival-apply],
with inputs in the supplied @racket[input-ranges],
is guaranteed to raise an exception.

In other words, if @racket[ival-false] is returned, there is no point
calling @racket[rival-apply] with any point in the
@racket[input-range]. If @racket[ival-maybe] is returned, some points
in the @racket[input-range] may raise errors, while others may not,
though nothing is guaranteed. If @racket[ival-true] is returned, an
@racket[exn:rival:invalid] will not be raised for any point in the
@racket[input-range]. However, a @racket[exn:rival:unsamplable] may
still be raised.
}

The advantage of @racket[rival-analyze] over @racket[rival-apply] is that
it applies to whole ranges of input points and is much faster.
