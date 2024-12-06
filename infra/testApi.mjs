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
await callAnalyze(FPCoreFormula, [[[
  14.97651307489794
], 0.12711304680349078]], [[[14.97651307489794], "2.3"]], false);

await callAnalyze(FPCoreFormula, [[[
  14.97651307489794
], 0.12711304680349078]], [[[14.97651307489794], "2.3"]], true);

async function callAnalyze(fpcore, p_context, result_huh, async_huh) {
  console.debug(`Analyze for ${fpcore}`);
  const errors_body = {
    method: 'POST', body: JSON.stringify({
      formula: fpcore, sample: p_context
    })
  };
  var alt_error = null;
  if (async_huh) {
    const analyze_rsp = await callAsyncAndWaitResult("/api/start/analyze", errors_body, CHECK_CORS);
    assert.equal(analyze_rsp.status, 200);
    alt_error = await analyze_rsp.json();
  } else {
    const analyze_rsp = await fetchAndCheckRSPHeaders("/api/analyze", errors_body, CHECK_CORS);
    assert.equal(analyze_rsp.status, 200);
    alt_error = await analyze_rsp.json();
  }
  assert.equal(Object.values(alt_error).length, 4);
  assertIdAndPath(alt_error);
  assert.equal(alt_error.points.length == p_context.length, true);
  if (result_huh != null) {
    assert.deepEqual(alt_error.points, result_huh);
  }
}

// MARK: Translate
async function callTranslate(fpcore, language, maybe_value) {
  if (language === 'mathjs') {
    console.debug(`Translate to mathjs for ${fpcore}.`);
    const mathjs_rsp = await fetchAndCheckRSPHeaders("/api/mathjs", {
      method: 'POST', body: JSON.stringify({ formula: fpcore })
    }, CHECK_CORS);
    const mathjs = await mathjs_rsp.json();
    if (maybe_value != null) {
      assert.equal(mathjs.mathjs, maybe_value);
    } else {
      assert.equal(mathjs.mathjs.length > 0, true);
    }
  } else {
    console.debug(`Translate to ${language} for ${fpcore}.`);
    const json = await (await fetchAndCheckRSPHeaders("/api/translate", {
      method: 'POST', body: JSON.stringify(
        { formula: fpcore, language: language })
    }, CHECK_CORS)).json();
    assert.equal(Object.values(json).length, 2);
    assert.equal(json['language'], language);
    if (maybe_value != null) {
      assert.equal(json['result'], maybe_value);
    } else {
      assert.equal(json['language'].length > 0, true);
    }
  }
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
async function callSample(fpcore, async) {
  console.debug(`Sampling ${fpcore}`);
  const sampleBody = {
    method: 'POST',
    body: JSON.stringify({ formula: fpcore, seed: 5 })
  };
  var sample_json = null;
  if (async) {
    const sample_rsp = await callAsyncAndWaitResult("/api/start/sample", sampleBody, CHECK_CORS);
    assert.equal(sample_rsp.status, 200);
    sample_json = await sample_rsp.json();
  } else {
    const sample_rsp = await fetchAndCheckRSPHeaders("/api/sample", sampleBody, CHECK_CORS);
    assert.equal(sample_rsp.status, 200);
    sample_json = await sample_rsp.json();
  }
  assert.equal(Object.values(sample_json).length, 4);
  assert.ok(sample_json.points);
  assert.equal(sample_json.points.length, SAMPLE_SIZE);
  return sample_json;
}

const sample1 = await callSample(FPCoreFormula, true);
const sample2 = await callSample(FPCoreFormula, false);
assert.deepEqual(sample1.points[1], sample2.points[1]);

// MARK: Explanations
async function callExplanations(fpcore, p_context, async) {
  console.debug(`Explaining ${fpcore}`);
  const explainBody = {
    method: 'POST',
    body: JSON.stringify({
      formula: fpcore, sample: p_context
    })
  };
  var explain_json = null;
  if (async) {
    const explain_rsp = await callAsyncAndWaitResult("/api/start/explanations", explainBody, CHECK_CORS);
    assert.equal(explain_rsp.status, 200);
    explain_json = await explain_rsp.json();
  } else {
    const explain_rsp = await fetchAndCheckRSPHeaders("/api/explanations", explainBody, CHECK_CORS);
    assert.equal(explain_rsp.status, 200);
    explain_json = await explain_rsp.json();
  }
  assertIdAndPath(explain_json);
  assert.equal(Object.values(explain_json).length, 4);
}

// MARK: Exacts endpoint
async function callExacts(fpcore, p_context, as_async) {
  const exacts_body = {
    method: 'POST', body: JSON.stringify({
      formula: fpcore, sample: p_context
    })
  };
  var exacts_json = null;
  if (as_async) {
    const exacts_rsp = await callAsyncAndWaitResult("/api/start/exacts", exacts_body, CHECK_CORS);
    assert.equal(exacts_rsp.status, 200);
    exacts_json = await exacts_rsp.json();
  } else {
    const exacts_rsp = await fetchAndCheckRSPHeaders("/api/exacts", exacts_body, CHECK_CORS);
    assert.equal(exacts_rsp.status, 200);
    exacts_json = await exacts_rsp.json();
  }
  assertIdAndPath(exacts_json);
  assert.equal(Object.values(exacts_json).length, 4);
  return exacts_json;
}

const exacts1 = await callExacts(FPCoreFormula2, eval_sample, true);
const exacts2 = await callExacts(FPCoreFormula2, eval_sample, false);
assert.deepEqual(exacts1.points, [[[1], -1.4142135623730951]]);
assert.deepEqual(exacts2.points, [[[1], -1.4142135623730951]]);

// MARK: Calculate endpoint
async function callCalculate(fpcore, p_context, as_async) {
  const calculateBody = {
    method: 'POST', body: JSON.stringify({
      formula: fpcore, sample: p_context
    })
  }
  var calculate_json = null;
  var calculate_rsp = null;
  if (as_async) {
    calculate_rsp = await callAsyncAndWaitResult("/api/start/calculate", calculateBody, CHECK_CORS);
    assert.equal(calculate_rsp.status, 200);
    calculate_json = await calculate_rsp.json();
  } else {
    calculate_rsp = await fetchAndCheckRSPHeaders("/api/calculate", calculateBody, CHECK_CORS);
    assert.equal(calculate_rsp.status, 200);
    calculate_json = await calculate_rsp.json();
  }
  assertIdAndPath(calculate_json);
  assert.equal(Object.values(calculate_json).length, 4);
  return calculate_json;
}

const calc1 = await callCalculate(FPCoreFormula2, eval_sample, true);
const calc2 = await callCalculate(FPCoreFormula2, eval_sample, false);
assert.deepEqual(calc1.points, [[[1], -1.4142135623730951]]);
assert.deepEqual(calc2.points, [[[1], -1.4142135623730951]]);

// MARK: Local error
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

async function callLocalError(fpcore, p_context, as_async) {
  const localError_body = {
    method: 'POST', body: JSON.stringify({
      formula: fpcore, sample: p_context
    })
  };
  var localerror_json = null;
  var localerror_rsp = null;
  if (as_async) {
    localerror_rsp = await callAsyncAndWaitResult("/api/start/localerror", localError_body, CHECK_CORS);
    assert.equal(localerror_rsp.status, 200);
    localerror_json = await localerror_rsp.json();
  } else {
    localerror_rsp = await fetchAndCheckRSPHeaders("/api/localerror", localError_body, CHECK_CORS);
    assert.equal(localerror_rsp.status, 200);
    localerror_json = await localerror_rsp.json();
  }
  assertIdAndPath(localerror_json);
  assert.equal(Object.values(localerror_json).length, 4);
  assert.ok(localerror_json.tree['avg-error']);
  return localerror_json;
}

// MARK: Alternatives endpoint
const alts_sync = callAlternatives(FPCoreFormula, [[[
  14.97651307489794
], 0.12711304680349078]], false);
const alts_async = callAlternatives(FPCoreFormula, [[[
  14.97651307489794
], 0.12711304680349078]], true);
assert.deepEqual(alts_sync, alts_async);

async function callAlternatives(fpcore, p_context, async_huh) {
  console.debug(`Getting alts for ${fpcore}`);
  const alternatives_body = {
    method: 'POST', body: JSON.stringify({
      formula: fpcore, sample: p_context
    })
  };
  var alternatives_json = null;
  var alternatives_rsp = null;
  if (async_huh) {
    alternatives_rsp = await callAsyncAndWaitResult("/api/start/alternatives", alternatives_body, CHECK_CORS);
    assert.equal(alternatives_rsp.status, 200);
    alternatives_json = await alternatives_rsp.json();
  } else {
    alternatives_rsp = await fetchAndCheckRSPHeaders("/api/alternatives", alternatives_body, CHECK_CORS);
    assert.equal(alternatives_rsp.status, 200);
    alternatives_json = await alternatives_rsp.json();
  }
  assertIdAndPath(alternatives_json);
  assert.equal(Object.values(alternatives_json).length, 7);
  assert.equal(Array.isArray(alternatives_json.alternatives), true);
  return alternatives_json;
}

// MARK: Cost endpoint
async function callCost(fpcore, p_context, async_huh) {
  const costBody = {
    method: 'POST', body: JSON.stringify({
      formula: fpcore, sample: p_context
    })
  };
  var cost_result = null;
  if (async_huh) {
    const cost_rsp = await callAsyncAndWaitResult("/api/start/cost", costBody, CHECK_CORS);
    assert.equal(cost_rsp.status, 200);
    cost_result = await cost_rsp.json();
  } else {
    const cost_rsp = await fetchAndCheckRSPHeaders("/api/cost", costBody, CHECK_CORS)
    assert.equal(cost_rsp.status, 200);
    cost_result = await cost_rsp.json();
  }
  assertIdAndPath(cost_result);
  assert.equal(Object.values(cost_result).length, 4);
  assert.equal(cost_result.cost > 0, true);
  return cost_result;
}

// MathJS endpoint
await callTranslate(FPCoreFormula, 'mathjs', "sqrt(x + 1.0) - sqrt(x)");

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
  await callTranslate(FPCoreFormula, e, expectedExpressions[e]);
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
    console.debug(`cost for ${alt}`)
    await callCost(alt, sample.points)
  }
  console.log(`Odyssey call for ${fpcore} completed.`)
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