const tempXY_A = "Output vs Input Accuracy"
const tempXY_B = "Each point represents a Herbie run below. Its horizontal position shows initial accuracy, and vertical position shows final accuracy. Points above the line are improved by Herbie."

const tempPareto_A = "Accuracy vs Speed"
const tempPareto_B = "A joint speed-accuracy pareto curve. Accuracy is on the vertical axis, speed is on the horizontal axis. Up and to the right is better. The initial program is shown by the red square."

const resultHelpText = `Color key:
    Green: improved accuracy
    Light green: no initial error
    Orange: no accuracy change
    Red: accuracy worsened
    Gray: timeout
    Dark Gray: error`
const targetHelpText = `Color key:
    Dark green: better than target
    Green: matched target
    Orange: improved but did not match target
    Yellow: no accuracy change
    `

function Element(tagname, props, children) {
    if (children === undefined) { children = props; props = {}; }

    var $elt = document.createElement(tagname);
    for (var i in props) if (props.hasOwnProperty(i)) $elt[i] = props[i];

    function addAll(c) {
        if (!c) return;
        else if (Array.isArray(c)) c.map(addAll);
        else if (typeof c == "string") $elt.appendChild(document.createTextNode(c))
        else if (c instanceof Node) $elt.appendChild(c);
        else {
            console.error("Not an element: ", c);
            throw "Invalid element!"
        }
    }
    addAll(children);
    return $elt;
}

// Helper Functions
function calculatePercent(decimal) {
    return ((100 - (100 * (decimal)))).toFixed(1)
}

function formatAccuracy(decimal) {
    return `${calculatePercent(decimal)}%`
}

function formatTime(ms) {
    if (ms > 60_000) {
        return (ms / 60_000).toFixed(1) + "min"
    } else {
        return (ms / 1000).toFixed(1) + "s"
    }
}

function calculateSpeedup(mergedCostAccuracy) {
    const initial_accuracy = mergedCostAccuracy[0][1]
    const deepCopy = JSON.parse(JSON.stringify(mergedCostAccuracy[1]))
    for (const point of deepCopy.reverse()) {
        if (point[1] > initial_accuracy) {
            return point[0].toFixed(1) + "×"
        }
    }
}

//https://stackoverflow.com/questions/196972/convert-string-to-title-case-with-javascript
function toTitleCase(str) {
    return str.replace(
        /\w\S*/g,
        function (txt) {
            return txt.charAt(0).toUpperCase() + txt.substr(1).toLowerCase();
        }
    )
}

// Based on https://observablehq.com/@fil/plot-onclick-experimental-plugin
// However, simplified because we don't need hit box data
function on(mark, listeners = {}) {
    const render = mark.render
    mark.render = function (facet, { x, y }, channels) {
        const data = this.data
        const g = render.apply(this, arguments)
        const r = d3.select(g).selectChildren()
        for (const [type, callback] of Object.entries(listeners)) {
            r.on(type, function (event, i) {
                return callback(event, data[i])
            })
        }
        return g
    }
    return mark
}

function plotXY(testsData, filterFunction) {
    var filteredTests = []
    testsData.forEach((test) => {
        if (filterFunction(test)) {
            filteredTests.push(test)
        }
    })
    const out = Plot.plot({
        marks: [
            Plot.line([[0, 0], [1, 1]], { stroke: '#ddd' }),
            on(Plot.dot(filteredTests, {
                x: d => 1 - d.start / 64, y: d => 1 - d.end / 64,
                fill: "#00a", strokeWidth: 2,
            }), {
                click: (e, d) => { window.location = d.link + "/graph.html"; },
            }),
        ],
        className: "clickable",
        marginBottom: 0,
        marginRight: 0,
        width: '400',
        height: '400',
        x: { nice: true, line: true, tickFormat: "%", },
        y: { nice: true, line: true, tickFormat: "%", },
    })
    out.setAttribute('viewBox', '0 0 420 420')
    return out;
}

function plotPareto(jsonData) {
    const [initial, frontier] = jsonData["merged-cost-accuracy"];
    const out = Plot.plot({
        marks: [
            Plot.dot([initial], {
                stroke: "#d00",
                symbol: "square",
                strokeWidth: 2
            }),
            Plot.line(frontier, {
                stroke: "#00a",
                strokeWidth: 2,
            }),
        ],
        width: '400',
        height: '400',
        x: { line: true, nice: true, tickFormat: c => c + "×" },
        y: { line: true, nice: true, domain: [0, 1], tickFormat: "%", },
        marginBottom: 0,
        marginRight: 0,
    })
    out.setAttribute('viewBox', '0 0 420 420')
    return out;
}

// -------------------------------------------------
// ------ Setup and Data fetching helpers ----------
// -------------------------------------------------

const renames = {
    "imp-start": "Improved start",
    "apx-start": "Approximate start",
    "uni-start": "Regressed from start",
    "ex-start": "Exact start",
    "eq-start": "Equal start",
    "lt-start": "Less than start",
    "gt-target": "Greater than target",
    "gt-start": "Greater than start",
    "eq-target": "Equal to target",
    "lt-target": "Less than target",
    "error": "Error",
    "timeout": "Timeout",
    "crash": "Crash",
}

// State for Forum radio buttons
// Why no some Types :(
var radioStatesIndex = -1
var radioStates = [
    "output",
    "startAccuracy",
    "resultAccuracy",
    "targetAccuracy",
    "time"
]
var filterTolerance = 1

var filterDetailsState = false

var groupState = {
    "improved": true,
    "regressed": true
}

var selectedBenchmarkIndex = -1
var benchMarks = []

var filterState = {
    "imp-start": true,
    "ex-start": true,
    "eq-start": true,
    "eq-target": true,
    "gt-target": true,
    "gt-start": true,
    "uni-start": true,
    "lt-target": true,
    "lt-start": true,
    "apx-start": true,
    "timeout": true,
    "crash": true,
    "error": true,
}

function buildBody(jsonData, otherJsonData, filterFunction) {
    // Maybe reuse current build body as the part that currently sucks is the tableBody and the filter logic

    function hasNote(note) {
        return (note ? toTitleCase(note) + " " : "") + "Results"
    }

    var total_start = 0
    var total_result = 0
    var maximum_accuracy = 0
    var total_time = 0
    var total_crash_timeout = 0
    jsonData.tests.forEach((test) => {
        total_start += test.start
        total_result += test.end
        maximum_accuracy += test.bits
        total_time += test.time
        if (test.status == "timeout" || test.status == "crash") {
            total_crash_timeout += 1
        }
    })

    const stats = Element("div", { id: "large" }, [
        Element("div", {}, [
            "Average Percentage Accurate: ",
            Element("span", { classList: "number" }, [
                formatAccuracy(total_start / maximum_accuracy),
                Element("span", { classList: "unit" }, [" → ",]),
                formatAccuracy(total_result / maximum_accuracy),]),
        ]),
        Element("div", {}, [
            "Time:",
            Element("span", { classList: "number" }, [formatTime(total_time)])
        ]),
        Element("div", {}, [
            "Bad Runs:",
            Element("span", { classList: "number", title: "Crashes and timeouts are considered bad runs." }, [`${total_crash_timeout}/${jsonData.tests.length}`])
        ]),
        Element("div", {}, [
            "Speedup:",
            Element("span", {
                classList: "number",
                title: "Aggregate speedup of fastest alternative that improves accuracy."
            }, [calculateSpeedup(jsonData["merged-cost-accuracy"])])
        ]),
    ])

    const header = Element("header", {}, [
        Element("h1", {}, hasNote(jsonData.note)),
        Element("img", { src: "logo-car.png" }, []),
        Element("nav", {}, [
            Element("ul", {}, [Element("li", {}, [Element("a", { href: "timeline.html" }, ["Metrics"])])])
        ]),
    ])

    const figureRow = Element("div", { classList: "figure-row" }, [
        Element("figure", { id: "xy" }, [
            Element("h2", {}, [tempXY_A]),
            plotXY(jsonData.tests, filterFunction),
            Element("figcaption", {}, [tempXY_B])
        ]),
        Element("figure", { id: "pareto" }, [
            Element("h2", {}, [tempPareto_A]),
            plotPareto(jsonData),
            Element("figcaption", {}, [tempPareto_B])
        ])
    ])
    return [header, stats, figureRow, tableBody(jsonData, otherJsonData, filterFunction)]
}

function tableBody(jsonData, otherJsonData, filterFunction) {
    for (let test of jsonData.tests) {
        let other = diffAgainstFields[test.name]
        if (filterFunction(test, other)) {
            eitherOr(test, other,
                (function () {
                    console.log("only normal")
                })
                , (function () {
                    console.log("pair")
                }))
        }
    }
}


function eitherOr(baselineRow, diffRow, singleFunction, pairFunctions) {
    // Pulled out into a function so if testing for diffRow needs to change only have to update here
    if (diffRow == undefined) {
        singleFunction()
    } else {
        pairFunctions()
    }
}

function update(jsonData, otherJsonData) {
    /*
    - Probably the first step of update should be taking the internal state and turning it into a filter function plus maybe a diff function or something like that.
    - Make each take both rows (baseline and diff)
    */
    const currentFilterFunction = makeFilterFunction()

    const newBody = Element("body", {}, buildBody(jsonData, otherJsonData, currentFilterFunction))
    htmlNode.replaceChild(newBody, bodyNode)
    bodyNode = newBody
}

function makeFilterFunction() {
    return function filterFunction(baselineRow, diffRow) {
        const currentSelectedBenchmarkIndex = selectedBenchmarkIndex
        const startAccuracyChecked = radioStates[radioStatesIndex] == "startAccuracy"
        const currentFilterState = filterState
        // TODO collect internal state into a filter function
        // TODO actually filter
        eitherOr(baselineRow, diffRow,
            (function () {
                console.log("filter normal")
            })
            , (function () {
                console.log(`baselineRow: ${baselineRow}`)
                console.log(`diffRow: ${diffRow}`)
            }))
        return true
    }
}

async function fetchAndUpdate(jsonData, url) {
    if (url.length > 0) {
        // FIXME url verifying if needed
        compareAgainstURL = url
        // Could also split string on / and check if the last component = "results.json"
        let lastChar = url.slice(url.length - 1, url.length)
        if (lastChar == "/") {
            url = url + "results.json"
        }
        let response = await fetch(url, {
            headers: { "content-type": "text/plain" },
            method: "GET",
            mode: "cors",
        })
        const json = await response.json()
        for (let test of json.tests) {
            diffAgainstFields[`${test.name}`] = test
        }
        otherJsonData = json
        update(jsonData)
    } else {
        otherJsonData = null
        update(jsonData)
    }
}

async function getResultsJson() {
    if (resultsJsonData == null) {
        let response = await fetch("results.json", {
            headers: { "content-type": "text/plain" },
            method: "GET",
            mode: "cors",
        });
        resultsJsonData = (await response.json());
        storeBenchmarks(resultsJsonData.tests)
    }
}

function storeBenchmarks(tests) {
    var tempDir = {}
    for (let test of tests) {
        const linkComponents = test.link.split("/")
        if (linkComponents.length > 1) {
            tempDir[linkComponents[0]] = linkComponents[0]
        }
    }
    for (let b in tempDir) {
        benchMarks.push(b)
    }
}

const htmlNode = document.querySelector("html")
var bodyNode = htmlNode.querySelector("body")

var compareAgainstURL = ""
var diffAgainstFields = {}
var otherJsonData = null
var resultsJsonData = null

await getResultsJson()
update(resultsJsonData, otherJsonData)