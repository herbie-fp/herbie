import { strict as assert } from 'node:assert';  // use strict equality everywhere 

// Future TODO: before this API becomes set in stone/offered publicly, we should change the results of these methods to be just the output data rather than duplicating input values.

// Reusable testing data
const SAMPLE_SIZE = 8000
const FPCoreFormula = '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))'
const FPCoreFormula2 = '(FPCore (x) (- (sqrt (+ x 1))))'
const eval_sample = [[[1], -1.4142135623730951]]

// improve endpoint
const improveResponse = await fetch(makeEndpoint(`/improve?formula=${encodeURIComponent(FPCoreFormula2)}`), { method: 'GET' })
assert.equal(improveResponse.status, 200)
let redirect = improveResponse.url.split("/")
const jobID = redirect[3].split(".")[0]
// This test is a little flaky as the character count of the response is not consistent.
// const improveHTML = await improveResponse.text()
// const improveHTMLexpectedCount = 25871
// assert.equal(improveHTML.length, improveHTMLexpectedCount, `HTML response character count should be ${improveHTMLexpectedCount} unless HTML changes.`)

// timeline
const timelineRSP = await fetch(makeEndpoint(`/timeline/${jobID}`), { method: 'GET' })
assert.equal(timelineRSP.status, 201)
const timeline = await timelineRSP.json()
assert.equal(timeline.length > 0, true)

// Test with a likely missing job-id
const badTimelineRSP = await fetch(makeEndpoint("/timeline/42069"), { method: 'GET' })
assert.equal(badTimelineRSP.status, 404)

// improve-start endpoint
const URIencodedBody = "formula=" + encodeURIComponent(FPCoreFormula)
const startResponse = await fetch(makeEndpoint("/api/start/improve"), {
  method: 'POST',
  headers: {
    'Content-Type': 'application/x-www-form-urlencoded',
  },
  body: URIencodedBody
})
const testResult = (startResponse.status == 201 || startResponse.status == 202)
assert.equal(testResult, true)
const improveResultPath = startResponse.headers.get("location")
let counter = 0
let cap = 100
// Check status endpoint
let checkStatus = await fetch(makeEndpoint(improveResultPath), { method: 'GET' })
/*
This is testing if the /api/start/improve test at the beginning has been completed. The cap and counter is a sort of timeout for the test. Ends up being 10 seconds max.
*/
while (checkStatus.status != 201 && counter < cap) {
  counter += 1
  checkStatus = await fetch(makeEndpoint(improveResultPath), { method: 'GET' })
  await new Promise(r => setTimeout(r, 100)); // ms
}
assert.equal(checkStatus.statusText, 'Job complete')

// up endpoint
const up = await fetch(makeEndpoint("/up"), { method: 'GET' })
assert.equal('Up', up.statusText)
// TODO how do I test down state?

// Sample endpoint
const sampleBody = {
  method: 'POST',
  body: JSON.stringify({ formula: FPCoreFormula2, seed: 5 })
}
const sampleRSP = await fetch(makeEndpoint("/api/sample"), sampleBody)
const sampleAsyncResult = await callAsyncAndWaitJSONResult("/api/start/sample", sampleBody)
const jid = sampleRSP.headers.get("x-herbie-job-id")
assert.notEqual(jid, null)
const sample = await sampleRSP.json()
assertIdAndPath(sampleAsyncResult)
assert.ok(sampleAsyncResult.points)
assert.equal(sampleAsyncResult.points.length, SAMPLE_SIZE)
assertIdAndPath(sample)
assert.ok(sample.points)
assert.equal(sample.points.length, SAMPLE_SIZE, `sample size should be ${SAMPLE_SIZE}`)

// Make second call to test that results are the same
const sample2RPS = await fetch(makeEndpoint("/api/sample"), sampleBody)
const jid2 = sample2RPS.headers.get("x-herbie-job-id")
assert.notEqual(jid2, null)
const sample2 = await sample2RPS.json()
assertIdAndPath(sample2)
assert.deepEqual(sample.points[1], sample2.points[1])

//Explanations endpoint
const explainBody = {
  method: 'POST',
  body: JSON.stringify({
    formula: FPCoreFormula, sample: sample2.points
  })
}
const explain = await (await fetch(makeEndpoint("/api/explanations"), explainBody)).json()
assertIdAndPath(explain)
assert.equal(explain.explanation.length > 0, true, 'explanation should not be empty');
const explainAsyncResult = await callAsyncAndWaitJSONResult("/api/start/explanations", explainBody)
assertIdAndPath(explainAsyncResult)
assert.equal(explainAsyncResult.explanation.length > 0, true, 'explanation should not be empty');

// Analyze endpoint
const errorsBody = {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: [[[
      14.97651307489794
    ], 0.12711304680349078]]
  })
}
const errors = await (await fetch(makeEndpoint("/api/analyze"), errorsBody)).json()
assertIdAndPath(errors)
assert.deepEqual(errors.points, [[[14.97651307489794], "2.3"]])
const analyzeAsyncResult = await callAsyncAndWaitJSONResult("/api/start/analyze", errorsBody)
assertIdAndPath(analyzeAsyncResult)
assert.deepEqual(analyzeAsyncResult.points, [[[14.97651307489794], "2.3"]])

// Exacts endpoint
const exactsBody = {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula2, sample: eval_sample
  })
}
const exacts = await (await fetch(makeEndpoint("/api/exacts"), exactsBody)).json()
assertIdAndPath(exacts)
assert.deepEqual(exacts.points, [[[1], -1.4142135623730951]])
const exactsAsyncResult = await callAsyncAndWaitJSONResult("/api/start/exacts", exactsBody)
assertIdAndPath(exactsAsyncResult)
assert.deepEqual(exactsAsyncResult.points, [[[1], -1.4142135623730951]])

// Calculate endpoint
const calculateBody = {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula2, sample: eval_sample
  })
}
const calculate = await (await fetch(makeEndpoint("/api/calculate"), calculateBody)).json()
assertIdAndPath(calculate)
assert.deepEqual(calculate.points, [[[1], -1.4142135623730951]])
const calculateAsyncResult = await callAsyncAndWaitJSONResult("/api/start/calculate", calculateBody)
assertIdAndPath(calculateAsyncResult)
assert.deepEqual(calculateAsyncResult.points, [[[1], -1.4142135623730951]])

// Local error endpoint
const localErrorBody = {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: sample2.points
  })
}
const localError = await (await fetch(makeEndpoint("/api/localerror"), localErrorBody)).json()
assertIdAndPath(localError)
assert.equal(localError.tree['avg-error'] > 0, true)
const localErrorAsyncResult = await callAsyncAndWaitJSONResult("/api/start/localerror", localErrorBody)
assertIdAndPath(localErrorAsyncResult)
assert.equal(localErrorAsyncResult.tree['avg-error'] > 0, true)

const localError1 = await (await fetch(makeEndpoint("/api/localerror"), {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: [[[2.852044568544089e-150], 1e+308]], seed: 5
  })
})).json()
const localError2 = await (await fetch(makeEndpoint("/api/localerror"), {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: [[[1.5223342548065899e-15], 1e+308]], seed: 5
  })
})).json()
// Test that different sample points produce different job ids ensuring that different results are served for these inputs.
assert.notEqual(localError1.job, localError2.job)

// Alternatives endpoint
const altBody = {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: [[[
      14.97651307489794
    ], 0.12711304680349078]]
  })
}
const alternatives = await (await fetch(makeEndpoint("/api/alternatives"), altBody)).json()
assertIdAndPath(alternatives)
assert.equal(Array.isArray(alternatives.alternatives), true)
const alternativesAsyncResult = await callAsyncAndWaitJSONResult("/api/start/alternatives", altBody)
assertIdAndPath(alternativesAsyncResult)
assert.equal(Array.isArray(alternativesAsyncResult.alternatives), true)

// Cost endpoint
const costBody = {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula2, sample: eval_sample
  })
}
const cost = await (await fetch(makeEndpoint("/api/cost"), costBody)).json()
assertIdAndPath(cost)
assert.equal(cost.cost > 0, true)
const costAsyncResult = await callAsyncAndWaitJSONResult("/api/start/cost", costBody)
assertIdAndPath(costAsyncResult)
assert.equal(costAsyncResult.cost > 0, true)

// MathJS endpoint
const mathjs = await (await fetch(makeEndpoint("/api/mathjs"), {
  method: 'POST', body: JSON.stringify({ formula: FPCoreFormula })
})).json()
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
  const translatedExpr = await (await fetch(makeEndpoint("/api/translate"), {
    method: 'POST', body: JSON.stringify(
      { formula: FPCoreFormula, language: e })
  })).json()

  assert.equal(translatedExpr.result, expectedExpressions[e])
}

// Results.json endpoint
const jsonResults = await (await fetch(makeEndpoint("/results.json"), { method: 'GET' })).json()

// Basic test that checks that there are the two results after the above test.
// TODO add a way to reset the results.json file?
assert.equal(jsonResults.tests.length, 2)

// Helper Functions
function makeEndpoint(endpoint) {
  return new URL(`http://127.0.0.1:8000${endpoint}`)
}

function assertIdAndPath(json) {
  assert.equal(json.job.length > 0, true)
  assert.equal(json.path.includes("."), true)
}

async function callAsyncAndWaitJSONResult(endpoint, body) {
  let counter = 0
  let cap = 100
  // Check status endpoint
  let jobInfo = await fetch(makeEndpoint(endpoint), body)
  /*
  The cap and counter is a sort of timeout for the test. Ends up being 10 seconds max.
  */
  const jobJSON = await jobInfo.json()
  const checkStatus = await fetch(makeEndpoint(`/check-status/${jobJSON.job}`), { method: 'GET' })
  while (checkStatus.status != 201 && counter < cap) {
    counter += 1
    checkStatus = await fetch(makeEndpoint(`/check-status/${jobJSON.job}`), { method: 'GET' })
    await new Promise(r => setTimeout(r, 100)); // ms
  }
  const result = await fetch(makeEndpoint(`/api/result/${jobJSON.job}`), { method: 'GET' })
  return await result.json()
}