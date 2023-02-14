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

const exacts = (await (await fetch('http://127.0.0.1:8000/api/exacts', {method: 'POST', body: JSON.stringify({formula: "(FPCore (x) (- (sqrt (+ x 1))))", points: [[1, 1]]})})).json()).points

assert.deepEqual(exacts, [[1, 1], -1.4142135623730951])
