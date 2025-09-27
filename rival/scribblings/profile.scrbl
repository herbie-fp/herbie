#lang scribble/manual

@title{Profiling Rival}

Rival's compiler and evaluator expose a variety of interesting
profiling data, which can be accessed via the @racket[rival-profile]
function. Note that all of Rival's profiling information is not
guaranteed to be stable, backward-, or forward-compatible between
versions. We do not recommend relying on @racket[rival-profile] in
your code without following Rival development closely.

@defproc[(rival-profile [machine rival-machine?] [command symbol?]) any/c]{

The @racket[rival-profile] function is called on a @racket[rival-machine?]
with a command symbol that decribes the kind of profiling information to return.
Profiling commands may be added or removed freely across versions; do not rely on them.

The currently-supported command symbols and their return values are:

@itemlist[
@item{@code{instructions} returns the number of register machine instructions in the compiled @racket[machine].}
@item{@code{iterations} returns the number of re-evaluation iterations needed for the most recent call to @racket[rival-apply] with the compiled @racket[machine]. This should be a number from 0 to @racket[*rival-max-iterations*], inclusive.}
@item{@code{bumps} returns the number of unexpected non-convergences detected during the most recent call to @racket[rival-apply] with the compiled @racket[machine]. These generally represent internal errors in Rival. While Rival will attempt to handle these "bumps" smoothly, they should still be reported to the developers as a bug.}
@item{@code{executions} returns a list of @racket{execution} structs, one for every register machine instruction executed by Rival. These executions are stored in a fixed-size buffer (see @racket[*rival-profile-executions*]) which is retained across @racket[rival-apply] calls and can fill up. The buffer is emptied by calls to @racket[(rival-profile machine 'executions)], so make sure to call this function regularly. If the list of @racket{execution}s returned by @racket[rival-profile] is equal in length to @racket[*rival-profile-executions*], you likely filled the buffer and are missing some executions.}
]

}

@defstruct*[execution
  ([name symbol?]
   [number natural?]
   [precision natural?]
   [time flonum?])]{
  Each execution corresponds to a single Rival interval operator being executed.
  The @racket[name] names the operator,
    except the special symbol @racket['adjust],
    in which case it refers to an internal precision-tuning pass.
  The @racket[number] gives its position in the compiled instruction sequence;
    this allows disambiguating if an expression contains, say, multiple
    addition operations.
  The @racket[precision] is the @racket[bf-precision] that the operator is executed at,
    and the @racket[time] is the time, in milliseconds, that that execution took.
  Note that, because Rival executes the register machine multiple times,
    the same operator (with the same @racket[name] and @racket[number])
    can appear multiple times for a single point.
  On the other hand, in some iterations Rival might skip some operators,
    if the precision is unchanged from previous iterations,
    so not every operator may show up in the executions list
    the same number of times.
}

@defparam[*rival-profile-executions* executions natural? #:value 1000]{

The executions are, for maximum performance, written into a single
fixed-size vector allocated when @racket[rival-compile] is called. To
change the size of this struct, set the
@racket[*rival-profile-executions*] during compilation. Once the
@racket[rival-compile] returns, the profiling vector's size cannot be
changed.

}

