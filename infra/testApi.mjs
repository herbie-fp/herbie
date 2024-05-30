import { strict as assert } from 'node:assert';  // use strict equality everywhere 

// Future TODO: before this API becomes set in stone/offered publically, we should change the results of these methods to be just the output data rather than duplicating input values.

const sample = (await(await fetch('http://127.0.0.1:8000/api/sample', { method: 'POST', body: JSON.stringify({ formula: '(FPCore (x) (- (sqrt (+ x 1))))', seed: 5 }) })).json())

const SAMPLE_SIZE = 8000
assert.ok(sample.points)
const points = sample.points
assert.equal(points.length, SAMPLE_SIZE, `sample size should be ${SAMPLE_SIZE}`)

const sample2 = (await (await fetch('http://127.0.0.1:8000/api/sample', { method: 'POST', body: JSON.stringify({ formula: '(FPCore (x) (- (sqrt (+ x 1))))', seed: 5 }) })).json())
const points2 = sample2.points

assert.deepEqual(points[1], points2[1], `request with seed should always return the same value;\nrequest was (await(await fetch('http://127.0.0.1:8000/api/sample', { method: 'POST', body: JSON.stringify({ formula: '(FPCore (x) (- (sqrt (+ x 1))))', seed: 5 }) })).json())`)

const errors = (await (await fetch('http://127.0.0.1:8000/api/analyze', { method: 'POST', body: JSON.stringify({ formula: '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))', sample: [[[
  14.97651307489794
], 0.12711304680349078]] }) })).json()).points  // HACK tiny sample

assert.deepEqual(errors, [[[14.97651307489794], "2.3"]], `error shouldn't change;\n request was (await (await fetch('http://127.0.0.1:8000/api/analyze', { method: 'POST', body: JSON.stringify({ formula: '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))', sample: [[[
  14.97651307489794
], 0.12711304680349078]] }) })).json())`)

const alternatives = (await (await fetch('http://127.0.0.1:8000/api/alternatives', { method: 'POST', body: JSON.stringify({ formula: '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))', sample: [[[
  14.97651307489794
], 0.12711304680349078]] }) })).json())  // HACK tiny sample

assert.equal(Array.isArray(alternatives.alternatives), true)

const mathjs = (await (await fetch('http://127.0.0.1:8000/api/mathjs', { 
	method: 'POST', 
	body: JSON.stringify({ formula: '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))'})
      })).json()).mathjs

assert.equal(mathjs, "sqrt(x + 1.0) - sqrt(x)")

const eval_sample = [[[1], -1.4142135623730951]]
const exacts = (await (await fetch('http://127.0.0.1:8000/api/exacts', {method: 'POST', body: JSON.stringify({formula: "(FPCore (x) (- (sqrt (+ x 1))))", sample: eval_sample})})).json()).points

assert.deepEqual(exacts, [[[1], -1.4142135623730951]])

const calculate = (await (await fetch('http://127.0.0.1:8000/api/calculate', {method: 'POST', body: JSON.stringify({formula: "(FPCore (x) (- (sqrt (+ x 1))))", sample: eval_sample})})).json()).points

assert.deepEqual(exacts, [[[1], -1.4142135623730951]])

const cost = (await (await fetch('http://127.0.0.1:8000/api/cost', {method: 'POST', body: JSON.stringify({formula: "(FPCore (x) (- (sqrt (+ x 1))))", sample: eval_sample})})).json())

const sample3 = (await (await fetch('http://127.0.0.1:8000/api/sample', { method: 'POST', body: JSON.stringify({ formula: '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))', seed: 5 }) })).json())

const localerror = (await (await fetch('http://127.0.0.1:8000/api/localerror', { method: 'POST', body: JSON.stringify({ formula: '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))', sample: sample2.points }) })).json())

assert.equal(localerror.tree['avg-error'] > 0, true)
assert.equal(cost.cost > 0, true)

const languageList = ["python", "c", "fortran", "java", "julia", "matlab", "wls", "tex", "js"]
const FPCoreFormula = '(FPCore (x) (- (sqrt (+ x 1)) (sqrt x)))'
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
        { method: 'POST', body: JSON.stringify(
        { formula: FPCoreFormula, language: language }) })).json())

    actualExpressions.push(translatedExpr)
}

for (let i = 0; i < expectedExpressions.length; i++) {
    assert.equal(actualExpressions[i].result, expectedExpressions[i])
}