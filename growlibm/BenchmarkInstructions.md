You are tasked with generating new benchmarks for Herbie, a numerical compiler that rewrites floating-point expressions to reduce error. 

When you receive a new source file:

1. **Skim for numerically fragile code paths.** Focus on
   - Branches guarding against domain violations (division by zero, `fabs` comparisons, overflow checks).
   - Expressions wrapped in conditionals, iterative refinements, or polynomial corrections applied only to small ranges.
   - Sequences of transcendental functions (`exp`, `asinh`, `atan2`, etc.) composed together.

2. **Score candidate expressions.** Prefer ones that are
   - Prone to catastrophic cancellation (differences of comparably sized terms, exponentials combined with reciprocals).
   - Already noted as “exact”, “correction”, or “series expansion” code by the source comments.

3. **Isolate the expression.** For each expression:
   - Identify the minimal set of variables needed (pull intermediate definitions when helpful).
   - Convert to FPCore using lisp syntax.
   - Add a `:pre` condition mirroring the guards in the source so Herbie’s sampler stays inside the intended domain.
   - Preserve structure instead of using algebraic simplifications.

4. **Validate syntax.** Run

   ```
   racket -y src/main.rkt report --platform vanilla <FPCORE FILE> out
   ```

   Fix any syntax errors Herbie reports (missing parentheses, unsupported identifiers, missing preconditions). Rerun until the file parses cleanly. Warnings about improvement quality are fine; only syntax errors must be resolved. Make sure that the number of FPCores you
   add to the file is the same number that appears in the report when you run this command.

Aim for a set of 15–20 expressions that represent the numerical behaviors from the source file. Ideally, this collection will include  hard expressions that Herbie has trouble improving. 