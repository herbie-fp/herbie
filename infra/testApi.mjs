import { strict as assert } from 'node:assert';  // use strict equality everywhere 

// Future TODO: before this API becomes set in stone/offered publicly, we should change the results of these methods to be just the output data rather than duplicating input values.

// Reusable testing data
const SAMPLE_SIZE = 8000
const FPCoreFormula = '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))'
const FPCoreFormula2 = '(FPCore (x) (- (sqrt (+ x 1))))'
const eval_sample = [[[1], -1.4142135623730951]]
const analyzeBody = JSON.stringify({
  formula: FPCoreFormula, sample: [[[
    14.97651307489794
  ], 0.12711304680349078]]
})
// --------------------------------------
// TEST ASYNC APIs
// --------------------------------------

// Sample
const sampleStartData = await testAsyncAPI("sample-start", JSON.stringify({ formula: FPCoreFormula2, seed: 5 }))
assert.ok(sampleStartData.points)
assert.equal(sampleStartData.points.length, SAMPLE_SIZE, `sample size should be ${SAMPLE_SIZE}`)

// Analyze
const analyzeStartData = await testAsyncAPI("analyze-start", analyzeBody)
assertIdAndPath(analyzeStartData)
assert.deepEqual(analyzeStartData.points, [[[14.97651307489794], "2.3"]])

// Localerror
const localErrorBody = JSON.stringify({
  formula: FPCoreFormula, sample: sampleStartData.points
})
const localerrorStartData = await testAsyncAPI("localerror-start", analyzeBody)
assertIdAndPath(localerrorStartData)
assert.equal(localerrorStartData.tree['avg-error'] > 0, true)

// --------------------------------------
// END ASYNC APIS
// --------------------------------------

// improve endpoint
const improveResponse = await callHerbie(`/improve?formula=${encodeURIComponent(FPCoreFormula2)}`, { method: 'GET' })
assert.equal(improveResponse.status, 200)
let redirect = improveResponse.url.split("/")
const jobID = redirect[3].split(".")[0]
// This test is a little flaky as the character count of the response is not consistent.
// const improveHTML = await improveResponse.text()
// const improveHTMLexpectedCount = 25871
// assert.equal(improveHTML.length, improveHTMLexpectedCount, `HTML response character count should be ${improveHTMLexpectedCount} unless HTML changes.`)

// timeline
const timelineRSP = await callHerbie(`/timeline/${jobID}`, { method: 'GET' })
assert.equal(timelineRSP.status, 201)
const timeline = await timelineRSP.json()
assert.equal(timeline.length > 0, true)

// Test with a likely missing job-id
const badTimelineRSP = await callHerbie(`/timeline/42069`, { method: 'GET' })
assert.equal(badTimelineRSP.status, 404)

// improve-start endpoint
const URIencodedBody = "formula=" + encodeURIComponent(FPCoreFormula)
const startResponse = await callHerbie(`/improve-start`, {
  method: 'POST',
  headers: {
    'Content-Type': 'application/x-www-form-urlencoded',
  },
  body: URIencodedBody
})
const testResult = (startResponse.status == 201 || startResponse.status == 202)
assert.equal(testResult, true)
const path = startResponse.headers.get("location")

// up endpoint
const up = await callHerbie("/up", { method: 'GET' })
assert.equal('Up', up.statusText)
// TODO how do I test down state?

// Sample endpoint
const sampleRSP = await callHerbie("/api/sample", { method: 'POST', body: JSON.stringify({ formula: FPCoreFormula2, seed: 5 }) })
const jid = sampleRSP.headers.get("x-herbie-job-id")
assert.notEqual(jid, null)

const sample = await sampleRSP.json()
assertIdAndPath(sample)

assert.ok(sample.points)
const points = sample.points
assert.equal(points.length, SAMPLE_SIZE, `sample size should be ${SAMPLE_SIZE}`)

const sample2RPS = await callHerbie("/api/sample", { method: 'POST', body: JSON.stringify({ formula: FPCoreFormula2, seed: 5 }) })
const jid2 = sample2RPS.headers.get("x-herbie-job-id")
assert.notEqual(jid2, null)
const sample2 = await sample2RPS.json()
const points2 = sample2.points
assertIdAndPath(sample2)
assert.deepEqual(points[1], points2[1])

// Analyze endpoint
const errors = await callHerbie("/api/analyze", {
  method: 'POST', body: analyzeBody
})
assertIdAndPath(errors)
assert.deepEqual(errors.points, [[[14.97651307489794], "2.3"]])

// Local error endpoint
const localError = await callHerbie("/api/localerror", {
  method: 'POST', body: localErrorBody
})
assertIdAndPath(localError)
assert.equal(localError.tree['avg-error'] > 0, true)

const json1 = JSON.stringify({
  formula: FPCoreFormula, sample: [[[2.852044568544089e-150], 1e+308]], seed: 5
})
const json2 = JSON.stringify({
  formula: FPCoreFormula, sample: [[[1.5223342548065899e-15], 1e+308]], seed: 5
})
const localError1 = await callHerbie("/api/localerror", {
  method: 'POST', body: json1
})
const localError2 = await callHerbie("/api/localerror", {
  method: 'POST', body: json2
})
// Test that different sample points produce different job ids ensuring that different results are served for these inputs.
assert.notEqual(localError1.job, localError2.job)

// Alternatives endpoint

const alternatives = await callHerbie("/api/alternatives", {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: [[[
      14.97651307489794
    ], 0.12711304680349078]]
  })
})
assertIdAndPath(alternatives)
assert.equal(Array.isArray(alternatives.alternatives), true)

//Explanations endpoint
const sampleExp = (await (await fetch('http://127.0.0.1:8000/api/sample', { method: 'POST', body: JSON.stringify({ formula: FPCoreFormula2, seed: 5 }) })).json())
const explain = await callHerbie("/api/explanations", {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula, sample: sampleExp.points
  })
})
assertIdAndPath(explain)
assert.equal(explain.explanation.length > 0, true, 'explanation should not be empty');
// Exacts endpoint
const exacts = await callHerbie("/api/exacts", {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula2, sample: eval_sample
  })
})
assertIdAndPath(exacts)
assert.deepEqual(exacts.points, [[[1], -1.4142135623730951]])

// Calculate endpoint
const calculate = await callHerbie("/api/calculate", {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula2, sample: eval_sample
  })
})
assertIdAndPath(calculate)
assert.deepEqual(calculate.points, [[[1], -1.4142135623730951]])

// Cost endpoint
const cost = await callHerbie("/api/cost", {
  method: 'POST', body: JSON.stringify({
    formula: FPCoreFormula2, sample: eval_sample
  })
})
assertIdAndPath(cost)
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

let counter = 0
let cap = 100
// Check status endpoint
let checkStatus = await callHerbie(path, { method: 'GET' })
/*
This is testing if the /improve-start test at the beginning has been completed. The cap and counter is a sort of timeout for the test. Ends up being 10 seconds max.
*/
while (checkStatus.status != 201 && counter < cap) {
  counter += 1
  checkStatus = await callHerbie(path, { method: 'GET' })
  await new Promise(r => setTimeout(r, 100)); // ms
}
assert.equal(checkStatus.statusText, 'Job complete')

// Results.json endpoint
const jsonResults = await callHerbie("/results.json", { method: 'GET' })

// Basic test that checks that there are the two results after the above test.
// TODO add a way to reset the results.json file?
assert.equal(jsonResults.tests.length, 2)

async function callHerbie(endPoint, body) {
  const url = new URL(`http://127.0.0.1:8000${endPoint}`)
  const pathname = url.pathname
  const rsp = await fetch(url, body)
  if (pathname == "/improve" ||
    pathname == "/improve-start" ||
    pathname.includes("check-status") ||
    pathname.includes("timeline") ||
    pathname.includes("sample") ||
    pathname == "/up") {
    return rsp
  } else {
    return rsp.json()
  }
}

function assertIdAndPath(json) {
  assert.equal(json.job.length > 0, true)
  // TODO regex for valid hashes?
  assert.equal(json.path.includes("."), true)
}

async function testAsyncAPI(endpointName, fetchBodyString) {
  const serverURL = `http://127.0.0.1:8000`
  const sampleStartURL = new URL(`${serverURL}${`/api/${endpointName}`}`)

  const rsp = await fetch(sampleStartURL, { method: 'POST', body: fetchBodyString })
  assert.equal(rsp.status, 200)
  const rspJSON = await rsp.json()
  assertIdAndPath(rspJSON)

  const checkStatusURL = new URL(`${serverURL}${"/check-status/"}${rspJSON.job}`)
  let counter = 0
  let cap = 100
  let checkStatus = await fetch(checkStatusURL, { method: 'GET' })
  // Loop to wait for for job to finnish
  while (checkStatus.status != 201 && counter < cap) {
    counter += 1
    checkStatus = await fetch(checkStatusURL, { method: 'GET' })
    await new Promise(r => setTimeout(r, 100)); // ms
  }
  assert.equal(checkStatus.statusText, 'Job complete')
  const resultsURL = new URL(`${serverURL}${"/api/results/"}${rspJSON.job}`)
  const results = await fetch(resultsURL, { method: 'GET' })
  return await results.json()
}