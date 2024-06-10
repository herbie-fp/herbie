import { strict as assert } from 'node:assert';  // use strict equality everywhere 

// Future TODO: before this API becomes set in stone/offered publicly, we should change the results of these methods to be just the output data rather than duplicating input values.

// Reusable testing data
const FPCoreFormula = '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))'
const FPCoreFormula2 = '(FPCore (x) (- (sqrt (+ x 1))))'
const eval_sample = [[[1], -1.4142135623730951]]

// improve endpoint
const improveURL = `http://127.0.0.1:8000/improve?formula=${encodeURIComponent(FPCoreFormula2)}`
const improveResponse = await fetch(improveURL, { method: 'GET' })
assert.equal(improveResponse.status, 200)
// This test is a little flaky as the character count of the response is not consistent.
// const improveHTML = await improveResponse.text()
// const improveHTMLexpectedCount = 25871
// assert.equal(improveHTML.length, improveHTMLexpectedCount, `HTML response character count should be ${improveHTMLexpectedCount} unless HTML changes.`)

// improve-start endpoint
const startURL = `http://127.0.0.1:8000/improve-start`
const URIencodedBody = "formula=" + encodeURIComponent(FPCoreFormula)
const startResponse = await fetch(startURL, {
  method: 'POST',
  headers: {
    'Content-Type': 'application/x-www-form-urlencoded',
  },
  body: URIencodedBody
})
assert.equal(startResponse.status, 201)
const path = startResponse.headers.get("location")

// Check status endpoint
const checkStatus = await fetch(
  `http://127.0.0.1:8000${path}`,
  { method: 'GET' })
assert.equal(checkStatus.status, 201)
assert.equal(checkStatus.statusText, 'Job complete')

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

assert.deepEqual(points[1], points2[1])

// Analyze endpoint
const errors = (await (await fetch('http://127.0.0.1:8000/api/analyze', {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: [[[
      14.97651307489794
    ], 0.12711304680349078]]
  })
})).json()).points  // HACK tiny sample

assert.deepEqual(errors, [[[14.97651307489794], "2.3"]])


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
const expectedExpressions = {
  "python": 'def expr(x):\n\treturn math.sqrt((x + 1.0)) - math.sqrt(x)\n',
  "c": 'double expr(double x) {\n\treturn sqrt((x + 1.0)) - sqrt(x);\n}\n',
  "fortran": 'real(8) function expr(x)\n    real(8), intent (in) :: x\n    expr = sqrt((x + 1.0d0)) - sqrt(x)\nend function\n',
  "java": 'public static double expr(double x) {\n\treturn Math.sqrt((x + 1.0)) - Math.sqrt(x);\n}\n',
  "julia": 'function expr(x)\n\treturn Float64(sqrt(Float64(x + 1.0)) - sqrt(x))\nend\n',
  "matlab": 'function tmp = expr(x)\n\ttmp = sqrt((x + 1.0)) - sqrt(x);\nend\n',
  "wls": 'expr[x_] := N[(N[Sqrt[N[(x + 1), $MachinePrecision]], $MachinePrecision] - N[Sqrt[x], $MachinePrecision]), $MachinePrecision]\n', // Wolfram 
  "tex": '\\mathsf{expr}\\left(x\\right) = \\sqrt{x + 1} - \\sqrt{x}\n',
  "js": 'function expr(x) {\n\treturn Math.sqrt((x + 1.0)) - Math.sqrt(x);\n}\n'
}

for (const e in expectedExpressions) {
  const translatedExpr =
    (await (await fetch('http://127.0.0.1:8000/api/translate',
      {
        method: 'POST', body: JSON.stringify(
          { formula: FPCoreFormula, language: e })
      })).json())

  assert.equal(translatedExpr.result, expectedExpressions[e])
}

// Results.json endpoint

const jsonResults = await (await fetch(
  'http://127.0.0.1:8000/results.json',
  { method: 'GET' })).json()

// Basic test that checks that there are the one result after the above test.
// TODO add a way to reset the results.json file?
assert.equal(jsonResults.tests.length, 1)
