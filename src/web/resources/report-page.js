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

function displayCrashTimeoutRatio(errors, total) {
    return `${errors}/${total}`
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

function update(jsonData) {

    const navigation = Element("nav", {}, [
        Element("ul", {}, [Element("li", {}, [Element("a", { href: "timeline.html" }, ["Metrics"])])])
    ])

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

    const header = Element("header", {}, [
        Element("h1", {}, hasNote(jsonData.note)),
        Element("img", { src: "logo-car.png" }, []),
        navigation,
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

    const tableData = tableBody(jsonData)

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
        tableData["tbody"]
    ])

    const compareDiv = Element("div", { classList: "report-details" }, [
        compareForm(jsonData),
        compareInfo(tableData["diffCount"]),
    ])

    const newBody = Element("body", {}, [
        header,
        stats,
        figureRow,
        compareDiv,
        buildFilters(jsonData.tests),
        resultsTable,
    ])
    htmlNode.replaceChild(newBody, bodyNode)
    bodyNode = newBody
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

var filterDetailsState = false

var compareState = {
    url: "",
    compare: false,
    start: true,
}

var groupState = {
    "improved": true,
    "regressed": true
}

var selectedBenchmarkIndex = -1
var benchMarks = []

var diffViewState = {
    "status": false,
    "output": false,
    "accuracy": false,
    "time": false
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

    var dropDownElements = []
    const defaultName = "All Benchmarks"
    if (selectedBenchmarkIndex == -1) {
        dropDownElements = [Element("option", { selected: true }, [defaultName])]
        for (let i in benchMarks) {
            const name = toTitleCase(benchMarks[i])
            dropDownElements.push(Element("option", {}, [name]))
        }
    } else {
        dropDownElements = [Element("option", {}, [defaultName])]
        for (let i in benchMarks) {
            const name = toTitleCase(benchMarks[i])
            if (selectedBenchmarkIndex == i) {
                dropDownElements.push(Element("option", { selected: true }, [name]))
            } else {
                dropDownElements.push(Element("option", {}, [name]))
            }
        }
    }

    const dropDown = Element("select", { id: "dropdown" }, dropDownElements)

    dropDown.addEventListener("click", (e) => {
        for (let i in benchMarks) {
            if (benchMarks[i].toLowerCase() == e.target.label.toLowerCase()) {
                selectedBenchmarkIndex = i
                update(resultsJsonData)
                return
            }
        }
        selectedBenchmarkIndex = -1
        update(resultsJsonData)
    })

    const details = Element("details", { id: "filters", open: filterDetailsState, classList: "report-details" }, [
        Element("summary", {}, [
            Element("h2", {}, "Filters"), improvedButton, regressedButton, dropDown]), [
            filterButtons]])
    details.addEventListener("click", (e) => {
        if (e.target.nodeName == "SUMMARY") {
            filterDetailsState = !filterDetailsState
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
        if (filterTest(test)) {
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

function filterTest(test) {
    const linkComponents = test.link.split("/")
    if (selectedBenchmarkIndex == -1 && filterState[test.status]) {
        return true
    } else if (selectedBenchmarkIndex != -1 && linkComponents.length > 1) {
        if (benchMarks[selectedBenchmarkIndex].toLowerCase() == linkComponents[0] && filterState[test.status]) {
            return true
        }
    } else {
        return false
    }
}

function tableBody(jsonData) {
    var rows = []
    for (let test of jsonData.tests) {
        if (filterTest(test)) {
            // TODO merge tableRowDiff and tableRow so we only have one code path
            /* 
            This should be possible now that tableRowDiff without any diff options checked displays the same view as tableRow
            */
            if (diffAgainstFields[test.name] && compareState["compare"]) {
                const row = tableRowDiff(test)
                if (!row.equal) {
                    rows.push(row.tr)
                }
            } else {
                rows.push(tableRow(test))
            }
        }
    }
    return { tbody: Element("tbody", {}, rows), diffCount: rows.length }
}

function tableRow(test) {
    var startAccuracy = formatAccuracy(test.start / test.bits)
    var resultAccuracy = formatAccuracy(test.end / test.bits)
    var targetAccuracy = formatAccuracy(test.target / test.bits)
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
                href: `${test.link}/graph.html`
            }, ["»"])]),
    ])
    tr.addEventListener("click", () => tr.querySelector("a").click())
    return tr
}

function tableRowDiff(test) {
    function timeTD(test) {
        var timeDiff = test.time - diffAgainstFields[test.name].time
        var color = "diff-time-red"
        var text
        var titleText = ""
        // Dirty equal less then 1 second
        if (Math.abs(timeDiff) < 1000) {
            color = "diff-time-gray"
            text = "~"
            titleText = `current: ${formatTime(test.time)} vs ${formatTime(diffAgainstFields[test.name].time)}`
        } else if (timeDiff < 0) {
            color = "diff-time-green"
            text = "+ " + `${formatTime(Math.abs(timeDiff))}`
        } else {
            text = "-" + `${formatTime(timeDiff)}`
        }
        return { td: Element("td", { classList: color, title: titleText }, [text]), equal: titleText != "" }
    }

    function buildTDfor(o, t) {
        const op = calculatePercent(o)
        const tp = calculatePercent(t)
        var color = "diff-time-red"
        var diff = op - tp
        var areEqual = false
        var titleText = ""
        var tdText = `- ${(diff).toFixed(1)}%`
        if (diff < 0) {
            diff = Math.abs(diff)
            color = "diff-time-green"
            tdText = `+ ${(diff).toFixed(1)}%`
        }
        // TODO what should the tolerance be?
        else if (diff == 0) {
            titleText = `Original: ${op} vs ${tp}`
            color = "diff-time-gray"
            areEqual = true
            tdText = "~"
        }
        return { td: Element("td", { classList: color, title: titleText }, [tdText]), equal: areEqual }
    }

    function startAccuracyTD(test) {
        const t = test.start / test.bits
        const o = diffAgainstFields[test.name].start / diffAgainstFields[test.name].bits
        return buildTDfor(o, t)
    }

    function resultAccuracyTD(test) {
        const t = test.end / test.bits
        const o = diffAgainstFields[test.name].end / diffAgainstFields[test.name].bits
        return buildTDfor(o, t)
    }

    function targetAccuracyTD(test) {
        const t = test.target / test.bits
        const o = diffAgainstFields[test.name].target / diffAgainstFields[test.name].bits
        return buildTDfor(o, t)
    }

    var testTile = ""
    var classList = [test.status]
    const startAccuracy = startAccuracyTD(test)
    const resultAccuracy = resultAccuracyTD(test)
    const targetAccuracy = targetAccuracyTD(test)
    const time = timeTD(test)

    var tdStartAccuracy = diffViewState["accuracy"] ? startAccuracy.td : Element("td", {}, [formatAccuracy(test.start / test.bits)])
    var tdResultAccuracy = diffViewState["accuracy"] ? resultAccuracy.td : Element("td", {}, [formatAccuracy(test.end / test.bits)])
    var tdTargetAccuracy = diffViewState["accuracy"] ? targetAccuracy.td : Element("td", {}, [formatAccuracy(test.target / test.bits)])
    const tdTime = diffViewState["time"] ? time.td : Element("td", {}, [formatTime(test.time)])

    var statusEqual = false
    var outputEqual = false

    if (diffViewState["status"] && test.status != diffAgainstFields[test.name].status) {
        statusEqual = true
        classList.push("diff-status")
        testTile = "(" + test.status + " != " + diffAgainstFields[test.name].status + ")"
    }

    if (diffViewState["output"] && test.output != diffAgainstFields[test.name].output) {
        outputEqual = true
        classList.push("diff-output")
        if (testTile != "") {
            testTile += "\n\n"
        }
        testTile += "{" + test.output + "} != {" + diffAgainstFields[test.name].output + "}"
    }

    if (test.status == "imp-start" ||
        test.status == "ex-start" ||
        test.status == "apx-start") {
        tdTargetAccuracy = Element("td", {}, [])
    }
    if (test.status == "timeout" || test.status == "error") {
        tdStartAccuracy = Element("td", {}, [])
        tdResultAccuracy = Element("td", {}, [])
        tdTargetAccuracy = Element("td", {}, [])
    }

    // Logic whether to display this table row or not.
    const areEqual = (time.equal && diffViewState["time"]) || (diffViewState["accuracy"] && startAccuracy.equal && resultAccuracy.equal && targetAccuracy.equal) || (diffViewState["output"] && !outputEqual) || (diffViewState["status"] && !statusEqual)

    var nameTD = Element("td", {}, [test.name])
    if (testTile != "") {
        nameTD = Element("td", { title: testTile }, [test.name])
    }

    const tr = Element("tr", { classList: classList.join(" ") }, [
        nameTD,
        tdStartAccuracy,
        tdResultAccuracy,
        tdTargetAccuracy,
        tdTime,
        Element("td", {}, [
            Element("a", {
                href: `${test.link}/graph.html`
            }, ["»"])]),
    ])
    tr.addEventListener("click", () => tr.querySelector("a").click())
    return { tr: tr, equal: areEqual }
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

function compareInfo(diffCount) {
    if (otherJsonData != null) {
        return [
            Element("details", {}, [
                Element("summary", {}, [
                    Element("h2", {}, ["More Info"]),
                    `Displaying ${diffCount}/${resultsJsonData.tests.length} tests that are not equal, Test marked pink have different output, Test with blue text have different status.`
                ]),
                Element("div", {}, [
                    Element("h3", {}, ["Current report:"]),
                    `${resultsJsonData.branch}: @${resultsJsonData.commit}, ${resultsJsonData.date}`]),
                Element("div", {}, [
                    Element("h3", {}, ["Compared to:"]),
                    `${otherJsonData.branch}: @${otherJsonData.commit}, ${otherJsonData.date}`]),
            ])
        ]
    } else {
        return
    }
}

function compareForm(jsonData) {
    const formName = "compare-form"
    const compareID = "compare-compare"
    const defaultID = "compare-default"
    const inputID = "compare-input"
    const compare = Element("input", {
        id: compareID, type: "radio", checked: compareState["compare"],
        name: formName
    }, [])
    const status = buildCheckboxLabel("", "status", diffViewState["status"])
    status.addEventListener("click", () => {
        diffViewState["status"] = status.querySelector("input").checked
        update(resultsJsonData)
    })
    const output = buildCheckboxLabel("", "output", diffViewState["output"])
    output.addEventListener("click", () => {
        diffViewState["output"] = output.querySelector("input").checked
        update(resultsJsonData)
    })
    const accuracy = buildCheckboxLabel("", "accuracy", diffViewState["accuracy"])
    accuracy.addEventListener("click", () => {
        diffViewState["accuracy"] = accuracy.querySelector("input").checked
        update(resultsJsonData)
    })
    const time = buildCheckboxLabel("", "time", diffViewState["time"])
    time.addEventListener("click", () => {
        diffViewState["time"] = time.querySelector("input").checked
        update(resultsJsonData)
    })
    const input = Element("input", {
        id: inputID, value: compareState["url"]
    }, [])
    const starting = Element("input", {
        id: defaultID, type: "radio", checked: compareState["start"],
        name: formName
    }, [])
    const form = Element("form", {}, [
        Element("h2", {}, ["Compare"]), input, starting, "Default", compare, "Compare", status, output, accuracy, time])

    compare.addEventListener("click", async (e) => {
        await updateFromForm(jsonData, e.target.parentNode)
    })
    starting.addEventListener("click", async (e) => {
        await updateFromForm(jsonData, e.target.parentNode)
    })
    form.addEventListener("submit", async (e) => {
        e.preventDefault()
        await updateFromForm(jsonData, e.target.parentNode.childNodes[0])
    })
    return form
}

async function updateFromForm(jsonData, formNode) {
    await fetchAndUpdate(jsonData,
        formNode.childNodes[1].value,
        formNode.childNodes[2].checked,
        formNode.childNodes[4].checked)
}

async function fetchAndUpdate(jsonData, url, start, compare) {
    let lastChar = url.slice(url.length - 1, url.length)
    // Could also split string on / and check if the last component = "results.json"
    if (lastChar == "/") {
        url = url + "results.json"
    }
    // TODO url verifying if needed
    compareState["url"] = url
    compareState["start"] = start
    compareState["compare"] = compare
    if (start || url == "") {
        diffAgainstFields = {}
        otherJsonData = null
        update(jsonData)
    } else {
        if (otherJsonData == null) {
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
        }
        update(jsonData)
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

var diffAgainstFields = {}
var otherJsonData = null
var resultsJsonData = null

await getResultsJson()
update(resultsJsonData)