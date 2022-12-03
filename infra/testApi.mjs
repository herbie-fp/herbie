import { strict as assert } from 'node:assert';  // use strict equality everywhere 

const sample = (await(await fetch('http://127.0.0.1:8000/api/sample', { method: 'POST', body: JSON.stringify({ formula: '(FPCore (x) (- (sqrt (+ x 1))))', seed: 5 }) })).json())

const SAMPLE_SIZE = 8000
assert.ok(sample.points)
const points = sample.points
assert.equal(points.length, SAMPLE_SIZE, `sample size should be ${SAMPLE_SIZE}`)
assert.deepEqual(points[1], [[6.455685974004423e+259],-8.034728355087322e+129], `request with seed should always return the same value;\nrequest was (await(await fetch('http://127.0.0.1:8000/api/sample', { method: 'POST', body: JSON.stringify({ formula: '(FPCore (x) (- (sqrt (+ x 1))))', seed: 5 }) })).json())`)