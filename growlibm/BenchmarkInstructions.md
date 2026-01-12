You are tasked with generating new benchmarks for Herbie, a numerical compiler that rewrites floating-point expressions to reduce error. 

When you receive a new source file:

1. **Skim for numerical code paths.** Focus on
    - Straight line numerical paths containing "interesting" numerics
    - Sequences of transcendental functions (`exp`, `asinh`, `atan2`, etc.) composed together

2. **Score candidate expressions.** Prefer ones that are
    - Prone to high floating-point error from cancellation, overflow, underflow, etc.
    - Already noted as “exact”, “correction”, or “series expansion” code by the source comments.

3. **Isolate the expression.** For each expression:
    - Identify the minimal set of variables needed (pull intermediate definitions when helpful).
    - Convert to FPCore using lisp syntax.
    - Add a `:pre` condition mirroring parameter invariants if they exist. Omit adding a precondition field if there isn't one in source code. Don't add a `:pre true`. Don't worry about adding `:pre` conditions for mathematical domain checks as Herbie's sampler already takes care of this. 
    - Name the fpcores something reasonable but don't prefix the name with the name of the benchmark file.
    - Use `let*` and `let` as needed. Make sure to wrap variable assignment in square brackets in `let` expressions.
    - Ensure that `+` and `*` are only used as binary operations.
    - Preserve the original structure instead of using algebraic simplifications.

4. **Save the .fpcore file.**
    - In the bench/ folder
    - Give the .fpcore file an appropriate name

5. **Validate syntax.** Run

    ```
    racket -y src/main.rkt report --platform no-accelerators <FPCORE FILE> out
    ```

    Fix any syntax errors Herbie reports (missing parentheses, unsupported identifiers, missing preconditions). Rerun until the file parses cleanly. Warnings about improvement quality are fine; only syntax errors must be resolved. Make sure that the number of FPCores you
    add to the file is the same number that appears in the report when you run this command.

Aim for a set of expressions that represent the numerical behaviors from the source file. Ideally, this collection will faithfully represent the numerics in the file, but will also be challenging for Herbie to improve. 