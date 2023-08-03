// Constants
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

// Helpers
function formatAccuracy(num, dom) {
    return `${((100 - (100 * (num / dom)))).toFixed(1)}%`
}

function formatTime(ms) {
    if (ms > 60_000) {
        return (ms / 60_000).toFixed(1) + "min"
    } else {
        return (ms / 1000).toFixed(1) + "s"
    }
}

function displayCrashTimeoutRatio(errors, total) {
    return `${errors}/${total}`
}

function calculateSpeedup(mergedCostAccuracy) {
    const initial_accuracy = mergedCostAccuracy[0][1]
    const list = mergedCostAccuracy[1].reverse()
    // BUG seems to be inconsistent in which number this displays. 
    // Currently bouncing between 0.3 and 2.1
    for (const point of list) {
        if (point[1] > initial_accuracy) {
            return point[0].toFixed(1) + "×"
        }
    }
}
// end Helpers

function update(jsonData, otherJson) {

    const navigation = Element("nav", {}, [
        Element("ul", {}, [Element("li", {}, [Element("a", { href: "timeline.html" }, ["Metrics"])])])
    ])

    //https://stackoverflow.com/questions/196972/convert-string-to-title-case-with-javascript
    function toTitleCase(str) {
        return str.replace(
            /\w\S*/g,
            function (txt) {
                return txt.charAt(0).toUpperCase() + txt.substr(1).toLowerCase();
            }
        )
    }

    function hasNote(note) {
        return (note ? toTitleCase(note) + " " : "") + "Results"
    }

    const header = Element("header", {}, [
        Element("h1", {}, hasNote(jsonData.note)),
        Element("img", { src: "logo-car.png" }, []),
        navigation,
    ])

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
                formatAccuracy(total_start, maximum_accuracy),
                Element("span", { classList: "unit" }, [" → ",]),
                formatAccuracy(total_result, maximum_accuracy),]),
        ]),
        Element("div", {}, [
            "Time:",
            Element("span", { classList: "number" }, [formatTime(total_time)])
        ]),
        Element("div", {}, [
            "Bad Runs:",
            Element("span", { classList: "number", title: "Crashes and timeouts are considered bad runs." }, [displayCrashTimeoutRatio(total_crash_timeout, jsonData.tests.length)])
        ]),
        Element("div", {}, [
            "Speedup:",
            Element("span", {
                classList: "number",
                title: "Aggregate speedup of fastest alternative that improves accuracy."
            }, [calculateSpeedup(jsonData["merged-cost-accuracy"])])
        ]),
    ])

    const figureRow = Element("div", { classList: "figure-row" }, [
        Element("figure", { id: "xy" }, [
            Element("h2", {}, [tempXY_A]),
            plotXY(jsonData.tests),
            Element("figcaption", {}, [tempXY_B])
        ]),
        Element("figure", { id: "pareto" }, [
            Element("h2", {}, [tempPareto_A]),
            plotPareto(jsonData),
            Element("figcaption", {}, [tempPareto_B])
        ])
    ])

    const resultsTable = Element("table", { id: "results" }, [
        Element("thead", {}, [
            Element("tr", {}, [
                Element("th", {}, ["Test"]),
                Element("th", {}, ["Start"]),
                Element("th", {}, ["Result",
                    Element("span", { classList: "help-button", title: resultHelpText }, ["?"])]),
                Element("th", {}, ["Target",
                    Element("span", { classList: "help-button", title: targetHelpText }, ["?"])]),
                Element("th", {}, ["Time"]),
            ])
        ]),
        tableBody(jsonData)
    ])

    if (otherJson != undefined) {
        console.log(otherJson)
        const newBody = Element("body", {}, [
            header,
            stats,
            figureRow,
            compareReports(jsonData),
            buildFilters(jsonData.tests),
            resultsTable,
        ])
        htmlNode.replaceChild(newBody, bodyNode)
        bodyNode = newBody
    } else {
        const newBody = Element("body", {}, [
            header,
            stats,
            figureRow,
            compareReports(jsonData),
            buildFilters(jsonData.tests),
            resultsTable,
        ])
        htmlNode.replaceChild(newBody, bodyNode)
        bodyNode = newBody
    }
}

// View State
var detailsState = false
var groupState = {
    "improved": true,
    "regressed": true
}
var compareState = {
    url: "",
    other: false,
    start: true,
}
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

function compareReports(jsonData) {
    const formName = "compare-form"
    const compareID = "compare-compare"
    const defaultID = "compare-default"
    const inputID = "compare-input"
    const other = Element("input", {
        id: compareID, type: "radio", checked: compareState["other"],
        name: formName
    }, [])
    const starting = Element("input", {
        id: defaultID, type: "radio", checked: compareState["start"],
        name: formName
    }, [])
    const input = Element("input", {
        id: inputID, value: compareState["url"]
    }, [])
    const form = Element("form", { classList: "compare" }, [
        Element("h2", {}, ["Compare"]), input, starting, "Default", other, "Compare"])
    form.addEventListener("submit", async (e) => {
        e.preventDefault()
        if (e != undefined) {
            await fetchAndUpdate(jsonData,
                e.target.childNodes[1].value,
                e.target.childNodes[2].checked,
                e.target.childNodes[4].checked)
        }
    })
    form.addEventListener("click", async (e) => {
        if (e.target.nodeName == "INPUT") {
            await fetchAndUpdate(jsonData,
                e.target.parentNode.childNodes[1].value,
                e.target.parentNode.childNodes[2].checked,
                e.target.parentNode.childNodes[4].checked)
        }
    })
    return form
}

async function fetchAndUpdate(jsonData, url, start, other) {
    // TODO url verifying if needed
    compareState["url"] = url
    compareState["other"] = other
    compareState["start"] = start
    let response = await fetch(url, {
        headers: { "content-type": "text/plain" },
        method: "GET",
        mode: "cors",
    })
    const json = await response.json()
    update(jsonData, json)
}

function buildFilters(jsonTestData) {
    var testTypeCounts = {}
    for (let test of jsonTestData) {
        testTypeCounts[test.status] == null ?
            testTypeCounts[test.status] = 1 :
            testTypeCounts[test.status] += 1
    }

    var filterButtons = []
    for (let f in filterState) {
        const name = `${renames[f]} (${testTypeCounts[f] ? testTypeCounts[f] : "0"})`
        const button = buildCheckboxLabel(f + " sub-filter", name, filterState[f])
        button.addEventListener("click", () => {
            filterState[f] = button.querySelector("input").checked
            update(resultsJsonData)
        })
        filterButtons.push(button)
    }

    function setupGroup(name, childStateNames, parent) {
        parent.addEventListener("click", (e) => {
            if (e.target.nodeName == "INPUT") {
                groupState[name] = e.target.checked
                for (let i in childStateNames) {
                    filterState[childStateNames[i]] = e.target.checked
                }
                update(resultsJsonData)
            }
        })
    }

    const regressedTags = ["uni-start", "lt-target", "lt-start",
        "apx-start", "timeout", "crash", "error"]
    const improvedTags = ["imp-start", "ex-start", "eq-start", "eq-target",
        "gt-target", "gt-start"]

    const improvedButton = buildCheckboxLabel("improved", "Improved", groupState["improved"])
    const regressedButton = buildCheckboxLabel("regressed", "Regressed", groupState["regressed"])

    setupGroup("improved", improvedTags, improvedButton)
    setupGroup("regressed", regressedTags, regressedButton)

    const details = Element("details", { id: "filters", open: detailsState }, [
        Element("summary", {}, [
            Element("h2", {}, "Filters"), improvedButton, regressedButton]), [
            filterButtons]])
    details.addEventListener("click", (e) => {
        if (e.target.nodeName == "SUMMARY") {
            detailsState = !detailsState
        }
    })
    return details
}

function buildCheckboxLabel(classes, text, boolState) {
    return Element("label", { classList: classes }, [
        Element("input", { type: "checkbox", checked: boolState }, []),
        text])
}

function plotXY(testsData) {
    var filteredTests = []
    testsData.forEach((test) => {
        if (filterState[test.status]) {
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

function tableBody(jsonData) {
    var rows = []
    for (let test of jsonData.tests) {
        if (filterState[test.status]) {
            rows.push(tableRow(test, rows.length))
        }
    }
    return Element("tbody", {}, rows)
}

function tableRow(test, i) {
    var startAccuracy = formatAccuracy(test.start, test.bits)
    var resultAccuracy = formatAccuracy(test.end, test.bits)
    var targetAccuracy = formatAccuracy(test.target, test.bits)
    if (test.status == "imp-start" || test.status == "ex-start" || test.status == "apx-start") {
        targetAccuracy = ""
    }
    if (test.status == "timeout" || test.status == "error") {
        startAccuracy = ""
        resultAccuracy = ""
        targetAccuracy = ""
    }

    const tr = Element("tr", { classList: test.status }, [
        Element("td", {}, [test.name]),
        Element("td", {}, [startAccuracy]),
        Element("td", {}, [resultAccuracy]),
        Element("td", {}, [targetAccuracy]),
        Element("td", {}, [formatTime(test.time)]),
        Element("td", {}, [
            Element("a", {
                id: `test${i}`,
                href: `${test.link}/graph.html`
            }, ["»"])]),
    ])
    tr.addEventListener("click", () => tr.querySelector("a").click())
    return tr
}

async function getResultsJson() {
    if (resultsJsonData == null) {
        let response = await fetch("results.json", {
            headers: { "content-type": "text/plain" },
            method: "GET",
            mode: "cors",
        });
        resultsJsonData = (await response.json());
        return resultsJsonData
    } else {
        return resultsJsonData
    }
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


const htmlNode = document.querySelector("html")
var bodyNode = htmlNode.querySelector("body")

var resultsJsonData = await getResultsJson()

update(resultsJsonData)