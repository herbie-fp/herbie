import { strict as assert } from 'node:assert';  // use strict equality everywhere 

// Future TODO: before this API becomes set in stone/offered publicly, we should change the results of these methods to be just the output data rather than duplicating input values.
// Reusable testing data
const SAMPLE_SIZE = 8000
const CHECK_CORS = true
const FPCoreFormula = '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))'
const FPCoreFormula2 = '(FPCore (x) (- (sqrt (+ x 1))))'
const FPCoreFormula3 = '(FPCore (x) (if (<= (- (sqrt (+ x 1.0)) (sqrt x)) 0.05) (* 0.5 (sqrt (/ 1.0 x))) (fma (fma (- 0.125) x 0.5) x (- 1.0 (sqrt x)))))'
const eval_sample = [[[1], -1.4142135623730951]]

const lots_of_vars = `(FPCore  (a b c d e f g h i j k l m n o p q r s t) 0)`

await simulateOdysseyCall(FPCoreFormula);
await simulateOdysseyCall(FPCoreFormula2);
await simulateOdysseyCall(FPCoreFormula3);
await simulateOdysseyCall(lots_of_vars);

// MARK: Analyze
const expected_analyze = [[[14.97651307489794], "2.3"]]
const analyze_result1 = await callAnalyze(FPCoreFormula, [[[
  14.97651307489794
], 0.12711304680349078]], false);

const analyze_result2 = await callAnalyze(FPCoreFormula, [[[
  14.97651307489794
], 0.12711304680349078]], true);

assert.deepEqual(analyze_result1.points, expected_analyze);
assert.deepEqual(analyze_result2.points, expected_analyze);

async function callAnalyze(fpcore, p_context, async_huh) {
  const analyze_json = await getJSONFor('analyze', 'POST', JSON.stringify({
    formula: fpcore, sample: p_context
  }), async_huh);
  assertIdAndPath(analyze_json);
  checkFelids(analyze_json, ['command', 'points', 'job', 'path']);
  assert.equal(analyze_json.points.length == p_context.length, true);
  return analyze_json
}

// Test with a likely missing job-id
const badTimelineRSP = await fetchAndCheckRSPHeaders("/timeline/42069", { method: 'GET' }, CHECK_CORS)
assert.equal(badTimelineRSP.status, 404);

// MARK: Check Status
const check_missing_job = await fetchAndCheckRSPHeaders(`/check-status/42069`, { method: 'GET' }, CHECK_CORS)
assert.equal(check_missing_job.status, 202);

// MARK: UP
async function callUp() {
  const up = await fetchAndCheckRSPHeaders("/up", { method: 'GET' }, CHECK_CORS);
  assert.equal(up.statusText, 'Up');
}

// MARK: Sample
async function callSample(fpcore, async_huh) {
  var sample_json = await getJSONFor('sample', 'POST', JSON.stringify({ formula: fpcore, seed: 5 }), async_huh);
  assertIdAndPath(sample_json);
  checkFelids(sample_json, ['command', 'points', 'job', 'path']);
  assert.equal(sample_json.points.length, SAMPLE_SIZE);
  return sample_json;
}

const sample1 = await callSample(FPCoreFormula, true);
const sample2 = await callSample(FPCoreFormula, false);
assert.deepEqual(sample1.points[1], sample2.points[1]);

// MARK: Explanations
async function callExplanations(fpcore, p_context, async_huh) {
  const explain_json = await getJSONFor('explanations', 'POST', JSON.stringify({
    formula: fpcore, sample: p_context
  }), async_huh);
  assertIdAndPath(explain_json);
  checkFelids(explain_json, ['command', 'explanation', 'job', 'path']);
  return explain_json;
}

// MARK: Exacts endpoint
async function callExacts(fpcore, p_context, async_huh) {
  const exacts_json = await getJSONFor('exacts', 'POST', JSON.stringify({
    formula: fpcore, sample: p_context
  }), async_huh);
  assertIdAndPath(exacts_json);
  checkFelids(exacts_json, ['command', 'points', 'job', 'path']);
  return exacts_json;
}

const exacts1 = await callExacts(FPCoreFormula2, eval_sample, true);
const exacts2 = await callExacts(FPCoreFormula2, eval_sample, false);
assert.deepEqual(exacts1.points, [[[1], -1.4142135623730951]]);
assert.deepEqual(exacts2.points, [[[1], -1.4142135623730951]]);

// MARK: Calculate endpoint
async function callCalculate(fpcore, p_context, async_huh) {
  const calculate_json = await getJSONFor('calculate', 'POST', JSON.stringify({
    formula: fpcore, sample: p_context
  }), async_huh);
  assertIdAndPath(calculate_json);
  checkFelids(calculate_json, ['command', 'points', 'job', 'path']);
  return calculate_json;
}

const calc1 = await callCalculate(FPCoreFormula2, eval_sample, true);
const calc2 = await callCalculate(FPCoreFormula2, eval_sample, false);
assert.deepEqual(calc1.points, [[[1], -1.4142135623730951]]);
assert.deepEqual(calc2.points, [[[1], -1.4142135623730951]]);

// MARK: Local error
async function callLocalError(fpcore, p_context, async_huh) {
  const localerror_json = await getJSONFor('localerror', 'POST', JSON.stringify({
    formula: fpcore, sample: p_context
  }), async_huh);
  assertIdAndPath(localerror_json);
  checkFelids(localerror_json, ['command', 'tree', 'job', 'path']);
  assert.ok(localerror_json.tree['avg-error']);
  return localerror_json;
}

const tree1 = await callLocalError(FPCoreFormula, [[[2.852044568544089e-150], 1e+308]], true);
const tree2 = await callLocalError(FPCoreFormula, [[[1.5223342548065899e-15], 1e+308]], false);
// Test that different sample points produce different job ids ensuring that different results are served for these inputs.

assert.notEqual(tree1.job, tree2.job);

const ignoredValue = 1e+308
'(FPCore (1e-100) (- (sqrt (+ x 1)) (sqrt x)))'
const localError5 = await callLocalError(FPCoreFormula, [[[1e-100], ignoredValue]], false);

// avg_error, actual_value, exact_value, absolute_difference, ulps_error
// root node
checkLocalErrorNode(localError5.tree, [],
  '-', '0.0', '1.0', '1.0', '1e-50', '0.0')
// left sqrt
checkLocalErrorNode(localError5.tree, [0],
  'sqrt', '0.0', '1.0', '1.0', '5e-101', '0.0')
// right sqrt 
checkLocalErrorNode(localError5.tree, [1],
  'sqrt', '0.0', '1e-50', '1e-50', '2.379726195519099e-68', '0.0')
// plus 
checkLocalErrorNode(localError5.tree, [0, 0],
  '+', '0.0', '1.0', '1.0', '1e-100', '0.0')
// var x
checkLocalErrorNode(localError5.tree, [0, 0, 0],
  'x', '0.0', '1e-100', '1e-100', 'equal', '0.0')
// literal 1
checkLocalErrorNode(localError5.tree, [0, 0, 1],
  '1.0', '0.0', '1.0', '1.0', 'equal', '0.0')

// '(FPCore (1e100) (- (sqrt (+ x 1)) (sqrt x)))'
const localError6 = await callLocalError(FPCoreFormula, [[[1e100], ignoredValue]], false);
// avg_error, actual_value, exact_value, absolute_error, ulps_error
// root node
checkLocalErrorNode(localError6.tree, [],
  '-', '61.7', '0.0', '5e-51', '5e-51', '61.74124908607812')
// left sqrt
checkLocalErrorNode(localError6.tree, [0],
  'sqrt', '0.0', '1e+50', '1e+50', '6.834625285603891e+33', '0.0')
// right sqrt 
checkLocalErrorNode(localError6.tree, [1],
  'sqrt', '0.0', '1e+50', '1e+50', '6.834625285603891e+33', '0.0')
// plus 
checkLocalErrorNode(localError6.tree, [0, 0],
  '+', '0.0', '1e+100', '1e+100', '1.0', '0.0')
// var x
checkLocalErrorNode(localError6.tree, [0, 0, 0],
  'x', '0.0', '1e+100', '1e+100', 'equal', '0.0')
// literal 1
checkLocalErrorNode(localError6.tree, [0, 0, 1],
  '1.0', '0.0', '1.0', '1.0', 'equal', '0.0')

// Test a large number `2e269` to trigger NaNs in local error
const localError7 = await callLocalError(FPCoreFormula3, [[[2e269], ignoredValue]], false);
// Test against conditionals expressions
checkLocalErrorNode(localError7.tree, [0],
  '<=', '0.0', 'true', 'true', 'equal', '0.0')
checkLocalErrorNode(localError7.tree, [0, 0],
  '-', '61.2', '0.0', '1.1180339887498948e-135', '1.1180339887498948e-135', '61.16647760559045')
checkLocalErrorNode(localError7.tree, [0, 1],
  '0.05', '0.0', '0.05', '0.05', 'equal', '0.0')
checkLocalErrorNode(localError7.tree, [2],
  'fma', '0.0', '-inf.0', '-inf.0', '+inf.0', '0.0')

// MARK: Alternatives endpoint
const alt_pcontext = [[[14.97651307489794], 0.12711304680349078]]
const alts_sync = callAlternatives(FPCoreFormula, alt_pcontext, false);
const alts_async = callAlternatives(FPCoreFormula, alt_pcontext, true);
assert.deepEqual(alts_sync, alts_async);

async function callAlternatives(fpcore, p_context, async_huh) {
  const alternatives_json = await getJSONFor('alternatives', 'POST', JSON.stringify({
    formula: fpcore, sample: p_context
  }), async_huh);
  assertIdAndPath(alternatives_json);
  checkFelids(alternatives_json, ['command', 'alternatives', 'histories', 'derivations', 'splitpoints', 'job', 'path']);
  assert.equal(Array.isArray(alternatives_json.alternatives), true);
  return alternatives_json;
}

// MARK: Cost endpoint
async function callCost(fpcore, p_context, async_huh) {
  const cost_json = await getJSONFor('cost', 'POST', JSON.stringify({
    formula: fpcore, sample: p_context
  }), async_huh);
  assertIdAndPath(cost_json);
  checkFelids(cost_json, ['command', 'cost', 'job', 'path']);
  assert.equal(cost_json.cost > 0, true);
  return cost_json;
}

// MARK: Translate
async function callTranslate(fpcore, language) {
  if (language === 'mathjs') {
    const mathjs_rsp = await fetchAndCheckRSPHeaders("/api/mathjs", {
      method: 'POST', body: JSON.stringify({ formula: fpcore })
    }, CHECK_CORS);
    assert.equal(mathjs_rsp.status, 200);
    const json = await mathjs_rsp.json();
    checkFelids(json, ['mathjs']);
    return json;
  } else {
    const translate_rsp = await fetchAndCheckRSPHeaders("/api/translate", {
      method: 'POST', body: JSON.stringify(
        { formula: fpcore, language: language })
    }, CHECK_CORS)
    assert.equal(translate_rsp.status, 200);
    const json = await translate_rsp.json();
    checkFelids(json, ['language', 'result']);
    return json;
  }
}

// MathJS endpoint
const test_json = await callTranslate(FPCoreFormula, 'mathjs');
const expected_value = "sqrt(x + 1.0) - sqrt(x)";
assert.equal(test_json.mathjs, expected_value);

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
  const test_json = await callTranslate(FPCoreFormula, e);
  assert.equal(test_json['language'], e);
  assert.equal(test_json['result'], expectedExpressions[e]);
}

// MARK: Herbie Demo
// Endpoints associated only with Herbie Demo

// improve endpoint
const improveResponse = await fetch(makeEndpoint(`/improve?formula=${encodeURIComponent(FPCoreFormula2)}`), { method: 'GET' })
assert.equal(improveResponse.status, 200);
let redirect = improveResponse.url.split("/");
const job_iD = redirect[3].split(".")[0];
// MARK: Timeline
const timelineRSP = await fetchAndCheckRSPHeaders(`/timeline/${job_iD}`, { method: 'GET' }, CHECK_CORS);
assert.equal(timelineRSP.status, 201);
// const timeline = await timelineRSP.json()
// assert.equal(timeline.length > 0, true)

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

// MARK: Helpers

/// root: The root node of the local error tree.
/// path: the path to get to the node you want to test.
/// name: Name of the node you are testing
/// avg_error: Average Error
/// actual_value: Value of the node
/// exact_value: The correct evaluation of the expression
/// absolute_difference: The ABS of the error at the node |approx - exact|
/// ulps_error: ulps of error at this node.
function checkLocalErrorNode(root, path, name,
  avg_error, actual_value, exact_value, absolute_difference, ulps_error) {
  const node = getNodeFromPath(root, path)
  // console.log(node) // Helpful for seeing which node is failing a test
  assert.equal(node['e'], name)
  assert.equal(node['avg-error'], avg_error)
  assert.equal(node['actual-value'], actual_value)
  assert.equal(node['exact-value'], exact_value)
  assert.equal(node['abs-error-difference'], absolute_difference)
  assert.equal(node['ulps-error'], ulps_error)
}

function getNodeFromPath(node, path) {
  if (path.length > 0) {
    const index = path.shift()
    const child = node['children'][index]
    return getNodeFromPath(child, path)
  } else {
    return node
  }
}

// pseudo simulates a current call to Odyssey.
async function simulateOdysseyCall(fpcore) {
  const as_async = false
  await callTranslate(fpcore, 'tex')
  await callUp()
  const sample = await callSample(fpcore, as_async)
  await callTranslate(fpcore, 'tex')
  await callAnalyze(fpcore, sample.points, null, as_async)
  const cost = callCost(fpcore, sample.points, as_async)
  const localError = await callLocalError(fpcore, sample.points, as_async)
  await callExplanations(fpcore, sample.points, as_async)
  const alternatives = await callAlternatives(fpcore, sample.points, as_async)
  for (const alt of alternatives['alternatives']) {
    await callTranslate(alt, 'mathjs', null)
    await callTranslate(alt, 'tex', null)
    await callAnalyze(alt, sample.points, null, false)
    await callCost(alt, sample.points)
  }
}

function makeEndpoint(endpoint) {
  return new URL(`http://127.0.0.1:8000${endpoint}`)
}

async function fetchAndCheckRSPHeaders(endpoint, body, check) {
  const rsp = await fetch(makeEndpoint(endpoint), body)
  if (check) {
    const header = rsp['headers'].get('access-control-allow-origin')
    assert.equal(header, '*', `Missing CORS header on rsp: ${rsp}`)
  }
  return rsp
}

function assertIdAndPath(json) {
  assert.equal(json.job.length > 0, true)
  assert.equal(json.path.includes(json.job), true)
  assert.equal(json.path.includes("."), true)
}

async function getJSONFor(endpoint, method, body, async_huh) {
  const request = {
    method: method,
    body: body
  };
  var response = null;
  if (async_huh) {
    response = await callAsyncAndWaitResult(`/api/start/${endpoint}`, request, CHECK_CORS);
  } else {
    response = await fetchAndCheckRSPHeaders(`/api/${endpoint}`, request, CHECK_CORS);
  }
  assert.equal(response.status, 200);
  return await response.json();
}

async function callAsyncAndWaitResult(endpoint, body, check) {
  let counter = 0
  let cap = 100
  // Check status endpoint
  let jobInfo = await fetchAndCheckRSPHeaders(endpoint, body, check)
  /*
  The cap and counter is a sort of timeout for the test. Ends up being 10 seconds max.
  */
  const jobJSON = await jobInfo.json()
  var checkStatus = await fetchAndCheckRSPHeaders(`/check-status/${jobJSON.job}`, { method: 'GET' }, check)
  while (checkStatus.status != 200 && counter < cap) {
    counter += 1
    checkStatus = await fetchAndCheckRSPHeaders(`/check-status/${jobJSON.job}`, { method: 'GET' }, check)
    await new Promise(r => setTimeout(r, 100)); // ms
  }
  return await fetchAndCheckRSPHeaders(`/api/result/${jobJSON.job}`, { method: 'GET' }, check)
}

function checkFelids(json, names) {
  assert.equal(Object.values(json).length, names.length);
  for (const felid of names) {
    assert.ok(json[felid]);
  }
}