import { strict as assert } from 'node:assert';  // use strict equality everywhere 

// Future TODO: before this API becomes set in stone/offered publicly, we should change the results of these methods to be just the output data rather than duplicating input values.

// Reusable testing data
const FPCoreFormula = '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))'
const FPCoreFormula2 = '(FPCore (x) (- (sqrt (+ x 1))))'
const eval_sample = [[[1], -1.4142135623730951]]

const longRunningJob = `(FPCore (w h dX.u dX.v dY.u dY.v maxAniso)
                          :precision binary32
                          (let* ((t_0 (* (floor h) dX.v))
                                (t_1 (* (floor w) dY.u))
                                (t_2 (* (floor h) dY.v))
                                (t_3 (* (floor w) dX.u))
                                (t_4 (fmax (+ (* t_3 t_3) (* t_0 t_0)) (+ (* t_1 t_1) (* t_2 t_2))))
                                (t_5 (sqrt t_4))
                                (t_6 (fabs (- (* t_3 t_2) (* t_0 t_1)))))
                            (log2
                            (if (> (/ t_4 t_6) (floor maxAniso))
                              (/ t_5 (floor maxAniso))
                              (/ t_6 t_5)))))`

// improve-start endpoint
// TODO

// improve endpoint
// TODO

/*
INPROGRESS - ZE
// Check status endpoint
const idk = await fetch('http://127.0.0.1:8000/api/improve', { method: 'POST', body: JSON.stringify({ formula: longRunningJob, seed: 5 }) })
// console.log(await idk)
const jobID = `idk-yet`
const checkStatus = await fetch(
  `http://127.0.0.1:8000/check-status?${jobID}`,
  { method: 'GET' })
// console.log(checkStatus)
*/

// up endpoint
const up = await fetch(
  'http://127.0.0.1:8000/up',
  { method: 'GET' })

assert.equal('Up', up.statusText)
// TODO how do I test down state?

// Sample endpoint
const sample = (await (await fetch('http://127.0.0.1:8000/api/sample', { method: 'POST', body: JSON.stringify({ formula: FPCoreFormula2, seed: 5 }) })).json())

const SAMPLE_SIZE = 8000
assert.ok(sample.points)
const points = sample.points
assert.equal(points.length, SAMPLE_SIZE, `sample size should be ${SAMPLE_SIZE}`)

const sample2 = (await (await fetch('http://127.0.0.1:8000/api/sample', { method: 'POST', body: JSON.stringify({ formula: FPCoreFormula2, seed: 5 }) })).json())
const points2 = sample2.points

assert.deepEqual(points[1], points2[1], `request with seed should always return the same value;\nrequest was (await(await fetch('http://127.0.0.1:8000/api/sample', { method: 'POST', body: JSON.stringify({ formula: ${FPCoreFormula2}, seed: 5 }) })).json())`)

// Analyze endpoint

const errors = (await (await fetch('http://127.0.0.1:8000/api/analyze', {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: [[[
      14.97651307489794
    ], 0.12711304680349078]]
  })
})).json()).points  // HACK tiny sample

assert.deepEqual(errors, [[[14.97651307489794], "2.3"]], `error shouldn't change;\n request was (await (await fetch('http://127.0.0.1:8000/api/analyze', { method: 'POST', body: JSON.stringify({ formula: ${FPCoreFormula}, sample: [[[
  14.97651307489794
], 0.12711304680349078]] }) })).json())`)

// Local error endpoint

const localError = (await (await fetch('http://127.0.0.1:8000/api/localerror', {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: sample2.points
  })
})).json())

assert.equal(localError.tree['avg-error'] > 0, true)

// Alternatives endpoint

const alternatives = (await (await fetch('http://127.0.0.1:8000/api/alternatives', {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: [[[
      14.97651307489794
    ], 0.12711304680349078]]
  })
})).json())  // HACK tiny sample

assert.equal(Array.isArray(alternatives.alternatives), true)

// Exacts endpoint

const exacts = (await (await fetch('http://127.0.0.1:8000/api/exacts', {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula2, sample: eval_sample
  })
})).json()).points

assert.deepEqual(exacts, [[[1], -1.4142135623730951]])

// Calculate endpoint

const calculate = (await (await fetch('http://127.0.0.1:8000/api/calculate', {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula2, sample: eval_sample
  })
})).json()).points

assert.deepEqual(calculate, [[[1], -1.4142135623730951]])

// Cost endpoint

const cost = (await (await fetch('http://127.0.0.1:8000/api/cost', {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula2, sample: eval_sample
  })
})).json())
assert.equal(cost.cost > 0, true)

// MathJS endpoint

const mathjs = (await (await fetch('http://127.0.0.1:8000/api/mathjs', {
  method: 'POST',
  body: JSON.stringify({ formula: FPCoreFormula })
})).json()).mathjs

assert.equal(mathjs, "sqrt(x + 1.0) - sqrt(x)")

// Translate endpoint

const languageList = ["python", "c", "fortran", "java", "julia", "matlab", "wls", "tex", "js"]
const actualExpressions = []
const expectedExpressions = [
  'def expr(x):\n\treturn math.sqrt((x + 1.0)) - math.sqrt(x)\n', // python
  'double expr(double x) {\n\treturn sqrt((x + 1.0)) - sqrt(x);\n}\n', // c
  'real(8) function expr(x)\n    real(8), intent (in) :: x\n    expr = sqrt((x + 1.0d0)) - sqrt(x)\nend function\n', // fortran
  'public static double expr(double x) {\n\treturn Math.sqrt((x + 1.0)) - Math.sqrt(x);\n}\n', // java
  'function expr(x)\n\treturn Float64(sqrt(Float64(x + 1.0)) - sqrt(x))\nend\n', // julia
  'function tmp = expr(x)\n\ttmp = sqrt((x + 1.0)) - sqrt(x);\nend\n', // matlab
  'expr[x_] := N[(N[Sqrt[N[(x + 1), $MachinePrecision]], $MachinePrecision] - N[Sqrt[x], $MachinePrecision]), $MachinePrecision]\n', // wls
  '\\mathsf{expr}\\left(x\\right) = \\sqrt{x + 1} - \\sqrt{x}\n', // tex
  'function expr(x) {\n\treturn Math.sqrt((x + 1.0)) - Math.sqrt(x);\n}\n' // js
]

for (const language of languageList) {
  const translatedExpr =
    (await (await fetch('http://127.0.0.1:8000/api/translate',
      {
        method: 'POST', body: JSON.stringify(
          { formula: FPCoreFormula, language: language })
      })).json())

  actualExpressions.push(translatedExpr)
}

for (let i = 0; i < expectedExpressions.length; i++) {
  assert.equal(actualExpressions[i].result, expectedExpressions[i])
}

// Results.json endpoint

const jsonResults = await (await fetch(
  'http://127.0.0.1:8000/results.json',
  { method: 'GET' })).json()

// Basic test that checks that there are the two results after the above test.
// TODO add a way to reset the results.json file?
assert.equal(jsonResults.tests.length, 1)