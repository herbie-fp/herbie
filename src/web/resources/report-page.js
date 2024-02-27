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
    testsData.tests.forEach((test) => {
        // Ugh global scope 
        let other = diffAgainstFields[test.name]
        if (filterFunction(test, other)) {
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
// ------ Global State Start ----------
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

// Flag for filtering if out diffs that are under the tolerance
var hideDirtyEqual = true

// State for Forum radio buttons
// Why no some Types :(
// null | 'output' | 'startAccuracy' | 'resultAccuracy' | 'targetAccuracy' | 'time'
let radioState = null;
const radioStates = [
    "output",
    "startAccuracy",
    "resultAccuracy",
    "targetAccuracy",
    "time"
]
var filterTolerance = 1

var filterDetailsState = false

var topLevelState = {
    "improved": true,
    "regressed": true,
    "pre-processed": false
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
var hideShowCompareDetails = false

// true = ascending, false = descending
var sortState = {
    "test": true,
    "start": false,
    "result": false,
    "target": false,
    "time": false
}

// if the state changed return true
function setSort(inputSelection) {
    let alreadySet = sortState[inputSelection];
    for (let k of Object.keys(sortState)) {
        sortState[k] = false;
    }
    sortState[inputSelection] = true;
    return !alreadySet;
}


var sortDescending = true

// -------------------------------------------------
// ------ Global State End ----------
// -------------------------------------------------

function buildCheckboxLabel(classes, text, boolState) {
    return Element("label", { classList: classes }, [
        Element("input", { type: "checkbox", checked: boolState }, []),
        text])
}

function showTolerance(jsonData, show) {
    const toleranceInputField = Element("input", {
        id: `toleranceID`, value: filterTolerance, size: 10, style: "text-align:right;",
    }, [])
    const hidingText = Element("text", {}, [" Hiding: ±"])

    var unitText
    if (radioState == "time") {
        unitText = Element("text", {}, ["s"])
    } else {
        unitText = Element("text", {}, ["%"])
    }
    const submitButton = Element("input", { type: "submit", value: "Update" }, [])
    submitButton.addEventListener("click", async (e) => {
        e.preventDefault();
        compareAgainstURL = e.target.parentNode.querySelector("#compare-input").value
        filterTolerance = toleranceInputField.value
        radioState = "resultAccuracy";
        fetchAndUpdate(jsonData)
    })
    toleranceInputField.style.display = show ? "inline" : "none"
    hidingText.style.display = show ? "inline" : "none"
    unitText.style.display = show ? "inline" : "none"
    return [hidingText, toleranceInputField, unitText, submitButton]
}

function buildRadioLabel(jsonData, title, state, name) {
    const output = Element("input", {
        name: name, id: "compare-" + state, type: "radio",
        checked: radioState == state,
    }, []);
    output.addEventListener("click", async (e) => {
        radioState = state;
        await fetchAndUpdate(jsonData);
    })
    return Element("label", [output, title]);
}

function buildCompareForm(jsonData) {

    const formName = "compare-form"

    const output = buildRadioLabel(jsonData, "Output", "output", formName);
    const startAccuracy = buildRadioLabel(jsonData, "Start Accuracy", "startAccuracy", formName);
    const resultAccuracy = buildRadioLabel(jsonData, "Result Accuracy", "resultAccuracy", formName);
    const targetAccuracy = buildRadioLabel(jsonData, "Target Accuracy", "targetAccuracy", formName);
    const time = buildRadioLabel(jsonData, "Time", "time", formName);

    const showEqual = buildCheckboxLabel("show-equal", "show equal", !hideDirtyEqual)
    showEqual.addEventListener("click", (e) => {
        hideDirtyEqual = !hideDirtyEqual
        update(jsonData)
    })

    const form = Element("form", {},
        [output, startAccuracy, resultAccuracy, targetAccuracy, time, " ", showEqual]);
    return form;
}

function buildBody(jsonData, otherJsonData, filterFunction) {

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
            plotXY(jsonData, filterFunction),
            Element("figcaption", {}, [tempXY_B])
        ]),
        Element("figure", { id: "pareto" }, [
            Element("h2", {}, [tempPareto_A]),
            plotPareto(jsonData),
            Element("figcaption", {}, [tempPareto_B])
        ])
    ])

    function buildSortableTextElement(stringName, nestedArray) {
        const nestedElements = [sortState[stringName] ? `${toTitleCase(stringName)} ` + `${sortDescending ? "⏶" : "⏷"}` : `${toTitleCase(stringName)} –`, nestedArray]
        const textElement = Element("th", {}, nestedElements)
        textElement.addEventListener("click", (e) => {
            if (!setSort(stringName)) {
                sortDescending = !sortDescending
            }
            update(jsonData)
        })
        return textElement
    }

    const rows = buildTableContents(jsonData, otherJsonData, filterFunction)
    const resultsTable = Element("table", { id: "results" }, [
        Element("thead", {}, [
            Element("tr", {}, [
                buildSortableTextElement("test", []),
                buildSortableTextElement("start", []),
                buildSortableTextElement("result", [
                    Element("span", { classList: "help-button", title: resultHelpText }, ["?"])]),
                buildSortableTextElement("target", [
                    Element("span", { classList: "help-button", title: targetHelpText }, ["?"])]),
                buildSortableTextElement("time"),
            ])
        ]),
        rows
    ])
    return [header, stats, figureRow, buildControls(jsonData, rows.length), resultsTable]
}

function sort(test) {
    let the_sort_idx = null;
    for (let k of Object.keys(sortState)) {
        if (sortState[k]) {
            the_sort_idx = k;
            break;
        }
    }
    let compare_field = {
        "start": "start", "result": "end",
        "target": "target", "time": "time",
        "test": "name",
    }[the_sort_idx];
    function compareFunction(l, r) {
        let cmp;
        if (compare_field == "name") {
            cmp = l.name.localeCompare(r.name);
        } else {
            cmp = l[compare_field] - r[compare_field];
        }
        if (sortDescending) cmp = -cmp;
        return cmp;
    }
    test.sort(compareFunction);
    return test;
}

function buildTableContents(jsonData, otherJsonData, filterFunction) {
    var rows = []
    const jsonTest = sort(jsonData.tests)
    for (let test of jsonTest) {
        let other = diffAgainstFields[test.name]
        if (filterFunction(test, other)) {
            let row = buildRow(test, other)
            rows.push(row)
        }
    }
    return rows
}

// HACK I kinda hate this split lambda function, Zane
function buildRow(test, other) {
    var row
    eitherOr(test, other,
        (function () {
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
                Element("td", {}, [
                    Element("a", {
                        href: `${test.link}/timeline.html`
                    }, ["📊"])]),
            ])
            // TODO fix bug with cmd/ctrl click.
            tr.addEventListener("click", () => tr.querySelector("a").click())
            row = tr
        })
        , (function () {
            function timeTD(test) {
                var timeDiff = test.time - diffAgainstFields[test.name].time
                var color = "diff-time-red"
                var text
                var titleText = `current: ${formatTime(test.time)} vs ${formatTime(diffAgainstFields[test.name].time)}`
                // Dirty equal less then 1 second
                var areEqual = false
                if (Math.abs(timeDiff) < (filterTolerance * 1000)) {
                    areEqual = true
                    color = "diff-time-gray"
                    text = "~"
                } else if (timeDiff < 0) {
                    color = "diff-time-green"
                    text = "+ " + `${formatTime(Math.abs(timeDiff))}`
                } else {
                    text = "-" + `${formatTime(timeDiff)}`
                }
                return { td: Element("td", { classList: color, title: titleText }, [text]), equal: areEqual }
            }

            function buildTDfor(o, t) {
                const op = calculatePercent(o)
                const tp = calculatePercent(t)
                var color = "diff-time-red"
                var diff = op - tp
                var areEqual = false
                var titleText = `Original: ${op} vs ${tp}`
                var tdText = `- ${(diff).toFixed(1)}%`
                if (Math.abs((diff).toFixed(1)) <= filterTolerance) {
                    color = "diff-time-gray"
                    areEqual = true
                    tdText = "~"
                } else if (diff < 0) {
                    diff = Math.abs(diff)
                    color = "diff-time-green"
                    tdText = `+ ${(diff).toFixed(1)}%`
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

            var classList = [test.status]
            const startAccuracy = startAccuracyTD(test)
            const resultAccuracy = resultAccuracyTD(test)
            const targetAccuracy = targetAccuracyTD(test)
            const time = timeTD(test)

            var tdStartAccuracy = radioState == "startAccuracy" ? startAccuracy.td : Element("td", {}, [formatAccuracy(test.start / test.bits)])
            var tdResultAccuracy = radioState == "resultAccuracy" ? resultAccuracy.td : Element("td", {}, [formatAccuracy(test.end / test.bits)])
            var tdTargetAccuracy = radioState == "targetAccuracy" ? targetAccuracy.td : Element("td", {}, [formatAccuracy(test.target / test.bits)])
            const tdTime = radioState == "time" ? time.td : Element("td", {}, [formatTime(test.time)])

            var testTile = ""
            var outputEqual = true
            if (radioState == "output") {
                outputEqual = false
            }
            if (test.output != diffAgainstFields[test.name].output) {
                // TODO steal Latex formatting from Odyssey
                testTile += `Current output:\n${test.output} \n \n Comparing to:\n ${diffAgainstFields[test.name].output}`
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

            const radioSelected = radioState

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
                Element("td", {}, [
                    Element("a", {
                        href: `${test.link}/timeline.html`
                    }, ["📊"])]),
            ])
            tr.addEventListener("click", () => tr.querySelector("a").click())
            row = tr
        })
    )
    return row
}

function buildControls(jsonData, diffCount) {
    const showing = diffCount
    const outOf = jsonData.tests.length
    // TODO if branches, or seeds are different
    let resultsDate = new Date(resultsJsonData.date * 1000)
    const resultDayString = `${resultsDate.getFullYear()}/${resultsDate.getMonth() + 1}/${resultsDate.getDay()}`
    let branchName = `${resultsJsonData.branch}`
    var displayingDiv = Element("div", {}, [
        `Displaying ${showing}/${outOf} benchmarks `,
        `on `, Element("code", {}, branchName),
        otherJsonData && [
            `, compared with baseline `, Element("code", {}, otherJsonData.branch),
        ]])

    const input = Element("input", {
        id: "compare-input", value: compareAgainstURL,
        placeholder: "URL to other json file"
    }, [])

    var showToleranceBool = false
    if (radioState == "time" ||
        radioState == "targetAccuracy" ||
        radioState == "resultAccuracy" ||
        radioState == "startAccuracy") {
        showToleranceBool = true
    }

    const toleranceInputField = showTolerance(jsonData, showToleranceBool)
    var summary = Element("details", { open: hideShowCompareDetails }, [
        Element("summary", {}, [
            Element("h2", {}, ["Diff"]),
            input,
            toleranceInputField,
        ]),
        Element("div", {}, [
            buildCompareForm(jsonData),
        ]),
    ])
    // GRR this events are annoying
    summary.addEventListener("click", async (e) => {
        if (e.target.nodeName == "SUMMARY") {
            hideShowCompareDetails = !hideShowCompareDetails
            await fetchAndUpdate(jsonData, compareAgainstURL)
        }
    })

    return Element("div", { classList: "report-details" }, [
        displayingDiv,
        summary,
        buildFiltersElement(jsonData)
    ])
}

function buildFiltersElement(jsonData) {
    var testTypeCounts = {}
    for (let test of jsonData.tests) {
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
            filterDetailsState = true
            update(resultsJsonData)
        })
        filterButtons.push(button)
    }

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
    dropDown.addEventListener("input", (e) => {
        const selected = e.target.selectedOptions[0].value
        for (let i in benchMarks) {
            if (selected != undefined) {
                if (benchMarks[i].toLowerCase() == selected.toLowerCase()) {
                    selectedBenchmarkIndex = i
                }
            }
        }
        update(resultsJsonData)
    })

    function setupGroup(name, childStateNames, parent) {
        parent.addEventListener("click", (e) => {
            if (e.target.nodeName == "INPUT") {
                topLevelState[name] = e.target.checked
                for (let i in childStateNames) {
                    filterState[childStateNames[i]] = e.target.checked
                }
                update(jsonData)
            }
        })
    }

    const regressedTags = ["uni-start", "lt-target", "lt-start",
        "apx-start", "timeout", "crash", "error"]
    const improvedTags = ["imp-start", "ex-start", "eq-start", "eq-target",
        "gt-target", "gt-start"]

    const improvedButton = buildCheckboxLabel("improved", "Improved", topLevelState["improved"])
    const regressedButton = buildCheckboxLabel("regressed", "Regressed", topLevelState["regressed"])

    setupGroup("improved", improvedTags, improvedButton)
    setupGroup("regressed", regressedTags, regressedButton)

    const preProcessed = buildCheckboxLabel("pre-processed", "PreProcessed", topLevelState["pre-processed"])
    preProcessed.addEventListener("click", (e) => {
        topLevelState["pre-processed"] = !topLevelState["pre-processed"]
        update(jsonData)
    })

    const filters = Element("details", { id: "filters", open: filterDetailsState }, [
        Element("summary", {}, [
            Element("h2", {}, "Filters"), improvedButton, regressedButton, preProcessed, dropDown]), [
            filterButtons]])
    filters.addEventListener("click", (e) => {
        if (e.target.nodeName == "SUMMARY") {
            filterDetailsState = !filterDetailsState
        }
    })
    return filters
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
    // capture current global filter state
    const currentFilterFunction = makeFilterFunction()

    const newBody = Element("body", {}, buildBody(jsonData, otherJsonData, currentFilterFunction))
    htmlNode.replaceChild(newBody, bodyNode)
    bodyNode = newBody
}

function filterPreProcess(baseData) {
    if (topLevelState["pre-processed"]) {
        if (baseData.preprocess.length > 2) {
            return true
        } else {
            return false
        }
    } else {
        return true
    }
}

function makeFilterFunction() {
    return function filterFunction(baseData, diffData) {
        var returnValue = true
        eitherOr(baseData, diffData,
            (function () {
                returnValue = returnValue && filterPreProcess(baseData)
            }),
            (function () {
                returnValue = returnValue && filterPreProcess(baseData)
                // Section to hide diffs that are below the provided tolerance
                if (hideDirtyEqual) {
                    // Diff Start Accuracy
                    if (radioState == "output") {
                        if (baseData.output != diffData.output) {
                            returnVAlue = returnValue && false;
                        }
                        const t = baseData.start / baseData.bits
                        const o = diffData.start / diffData.bits
                        const op = calculatePercent(o)
                        const tp = calculatePercent(t)
                        var diff = op - tp
                        if (Math.abs((diff).toFixed(1)) <= filterTolerance) {
                            returnValue = returnValue && false;
                        }
                    }
                    // Diff Start Accuracy
                    if (radioState == "startAccuracy") {
                        const t = baseData.start / baseData.bits
                        const o = diffData.start / diffData.bits
                        const op = calculatePercent(o)
                        const tp = calculatePercent(t)
                        var diff = op - tp
                        if (Math.abs((diff).toFixed(1)) <= filterTolerance) {
                            returnValue = returnValue && false
                        }
                    }

                    // Diff Result Accuracy
                    if (radioState == "resultAccuracy") {
                        const t = baseData.end / baseData.bits
                        const o = diffData.end / diffData.bits
                        const op = calculatePercent(o)
                        const tp = calculatePercent(t)
                        var diff = op - tp
                        if (Math.abs((diff).toFixed(1)) <= filterTolerance) {
                            returnValue = returnValue && false
                        }
                    }

                    // Diff Target Accuracy
                    if (radioState == "targetAccuracy") {
                        const t = baseData.target / baseData.bits
                        const o = diffData.target / diffData.bits
                        const op = calculatePercent(o)
                        const tp = calculatePercent(t)
                        var diff = op - tp
                        if (Math.abs((diff).toFixed(1)) <= filterTolerance) {
                            returnValue = returnValue && false
                        }
                    }

                    // Diff Time
                    if (radioState == "time") {
                        var timeDiff = baseData.time - diffData.time
                        if (Math.abs(timeDiff) < (filterTolerance * 1000)) {
                            returnValue = returnValue && false
                        }
                    }
                }
            }))
        const linkComponents = baseData.link.split("/")
        // guard statement
        if (selectedBenchmarkIndex != -1 && linkComponents.length > 1) {
            // defensive lowerCase
            const left = benchMarks[selectedBenchmarkIndex].toLowerCase()
            const right = linkComponents[0].toLowerCase()
            if (left == right) {
                returnValue = returnValue && true
            } else {
                return false
            }
        }
        if (filterState[baseData.status]) {
            returnValue = returnValue && true
        } else {
            return false
        }
        return returnValue
    }
}

async function fetchAndUpdate(jsonData) {
    if (compareAgainstURL.length > 0) {
        // Could also split string on / and check if the last component = "results.json"
        var url = compareAgainstURL
        let lastChar = compareAgainstURL.slice(url.length - 1, url.length)
        if (lastChar == "/") {
            url = url + "results.json"
        }
        let response = await fetch(url, {
            headers: { "content-type": "text/plain" },
            method: "GET",
            mode: "cors",
        })
        const json = await response.json()
        if (json.error) {
            otherJsonData = null
            update(jsonData)
            return
        }
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
