import { strict as assert } from 'node:assert';  // use strict equality everywhere 

// Future TODO: before this API becomes set in stone/offered publicly, we should change the results of these methods to be just the output data rather than duplicating input values.

// Reusable testing data
const FPCoreFormula = '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))'
const FPCoreFormula2 = '(FPCore (x) (- (sqrt (+ x 1))))'
const eval_sample = [[[1], -1.4142135623730951]]

// improve endpoint
const improveURL = await callHerbie(`/improve?formula=${encodeURIComponent(FPCoreFormula2)}`, { method: 'GET' })
assert.equal(improveURL.status, 200)
// This test is a little flaky as the character count of the response is not consistent.
// const improveHTML = await improveResponse.text()
// const improveHTMLexpectedCount = 25871
// assert.equal(improveHTML.length, improveHTMLexpectedCount, `HTML response character count should be ${improveHTMLexpectedCount} unless HTML changes.`)

// improve-start endpoint
const URIencodedBody = "formula=" + encodeURIComponent(FPCoreFormula)
const startResponse = await callHerbie(`/improve-start`, {
  method: 'POST',
  headers: {
    'Content-Type': 'application/x-www-form-urlencoded',
  },
  body: URIencodedBody
})
const testResult = startResponse.status == (201 || 202)
assert.equal(testResult, true)
const path = startResponse.headers.get("location")

// Check status endpoint
const checkStatus = await callHerbie(path, { method: 'GET' })
assert.equal(checkStatus.status, 201)
assert.equal(checkStatus.statusText, 'Job complete')

// up endpoint
const up = await callHerbie("/up", { method: 'GET' })
assert.equal('Up', up.statusText)
// TODO how do I test down state?

// Sample endpoint
const sample = await callHerbie("/api/sample", { method: 'POST', body: JSON.stringify({ formula: FPCoreFormula2, seed: 5 }) })

const SAMPLE_SIZE = 8000
assert.ok(sample.points)
const points = sample.points
assert.equal(points.length, SAMPLE_SIZE, `sample size should be ${SAMPLE_SIZE}`)

const sample2 = await callHerbie("/api/sample", { method: 'POST', body: JSON.stringify({ formula: FPCoreFormula2, seed: 5 }) })
const points2 = sample2.points

assert.deepEqual(points[1], points2[1])

// Analyze endpoint
const errors = await callHerbie("/api/analyze", {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: [[[
      14.97651307489794
    ], 0.12711304680349078]]
  })
})
assert.deepEqual(errors.points, [[[14.97651307489794], "2.3"]])

// Local error endpoint
const localError = await callHerbie("/api/localerror", {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: sample2.points
  })
})

assert.equal(localError.tree['avg-error'] > 0, true)

// Alternatives endpoint

const alternatives = await callHerbie("/api/alternatives", {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: [[[
      14.97651307489794
    ], 0.12711304680349078]]
  })
})
assert.equal(Array.isArray(alternatives.alternatives), true)

// Exacts endpoint
const exacts = await callHerbie("/api/exacts", {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula2, sample: eval_sample
  })
})
assert.deepEqual(exacts.points, [[[1], -1.4142135623730951]])

// Calculate endpoint
const calculate = await callHerbie("/api/calculate", {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula2, sample: eval_sample
  })
})

assert.deepEqual(calculate.points, [[[1], -1.4142135623730951]])

// Cost endpoint
const cost = await callHerbie("/api/cost", {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula2, sample: eval_sample
  })
})
assert.equal(cost.cost > 0, true)

// // MathJS endpoint
const mathjs = await callHerbie("/api/mathjs", {
  method: 'POST',
  body: JSON.stringify({ formula: FPCoreFormula })
})
assert.equal(mathjs.mathjs, "sqrt(x + 1.0) - sqrt(x)")

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
  const translatedExpr = await callHerbie("/api/translate", {
    method: 'POST', body: JSON.stringify(
      { formula: FPCoreFormula, language: e })
  })

  assert.equal(translatedExpr.result, expectedExpressions[e])
}

// Results.json endpoint
const jsonResults = await callHerbie("/results.json", { method: 'GET' })

// Basic test that checks that there are the one result after the above test.
// TODO add a way to reset the results.json file?
assert.equal(jsonResults.tests.length, 2)

async function callHerbie(endPoint, body) {
  const url = new URL(`http://127.0.0.1:8000${endPoint}`)
  const pathname = url.pathname
  const rsp = await fetch(url, body)
  if (pathname == "/improve" ||
    pathname == "/improve-start" ||
    pathname.includes("check-status") ||
    pathname == "/up") {
    return rsp
  } else {
    return rsp.json()
  }
}