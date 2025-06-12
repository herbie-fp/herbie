/* The report page uses a dummy-React pattern, where every time the
 * page changes we regenerate the whole HTML content from scratch */

// Here is the loaded data:

var compareAgainstURL = ""
var diffAgainstFields = {}
var otherJsonData = null
var resultsJsonData = null

function update() {
    let bodyNode = document.querySelector("body");
    bodyNode.replaceChildren.apply(bodyNode, buildBody(resultsJsonData, otherJsonData));
}

// Here is the UI state:

const filterNames = {
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
var filterState = Object.fromEntries(Object.keys(filterNames).map(k => [k, true]));

const filterGroups = {
    improved: [
        "ex-start", "eq-start", "eq-target",
        "imp-start", "gt-target", "gt-start",
    ],
    unchanged: [
        "lt-target", "apx-start", "error",
    ],
    regressed: [
        "uni-start", "lt-start", "timeout", "crash",
    ],
};
var filterGroupState = Object.fromEntries(Object.keys(filterGroups).map(k => [k, true]));

const radioStates = {
    output: { title: "Output Expression", tolerance: false, },
    startAcc: { title: "Input Accuracy", tolerance: "%", },
    endAcc: { title: "Output Accuracy", tolerance: "%", },
    targetAcc: { title: "Target Accuracy", tolerance: "%", },
    time: { title: "Running Time", tolerance: "s", },
}
let radioState = null;

// Controlling the diff process
var filterTolerance = 1;
var hideDirtyEqual = true

// Collapsable <details> elements
var showFilterDetails = false;
var showCompareDetails = false;


var filterBySuite = ""
var allSuites = []

var filterByWarning = ""
var allWarnings = []

var sortState = {
    key: "test",
    dir: true, // true = ascending, false = descending
}

// Next are various strings used in the UI

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

// Helper functions of various sorts

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

function calculatePercent(decimal) {
    return ((100 - (100 * (decimal)))).toFixed(1)
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
            if (typeof point[0] == 'number') {
                return point[0].toFixed(1) + "×";
            }
            else {
                return point[0];
            }
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

function buildDropdown(options, selected, placeholder, onChange) {
    const select = Element("select", [
        Element("option", { value: "", selected: !selected }, [placeholder]),
        options.map((opt) =>
            Element("option", { value: opt, selected: selected == opt }, [opt])
        ),
    ]);
    select.addEventListener("input", () => {
        onChange(select.value ?? "");
        update();
    });
    return select;
}

function buildHeader(title) {
    // The online demo always runs with seed 1; hide the Metrics link there
    const showMetricsLink = resultsJsonData?.seed != 1
    return Element("header", {}, [
        Element("h1", {}, title),
        Element("img", { src: "logo-car.png" }, []),
        showMetricsLink && Element("nav", {}, [
            Element("ul", {}, [
                Element("li", {}, [
                    Element("a", { href: "timeline.html" }, ["Metrics"])
                ])
            ])
        ]),
    ])
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

function plotXY(testsData, otherJsonData, filterFunction) {
    const filteredTests = testsData.tests.filter((test) => {
        return filterFunction(test, diffAgainstFields[test.name]);
    });
    function onclick(e, d) {
        window.location = d.link + "/graph.html";
    }
    let marks = [
        Plot.line([[0, 0], [1, 1]], { stroke: '#ddd' }),
    ];
    if (otherJsonData) {
        marks.push(Plot.arrow(filteredTests, {
            x1: d => 1 - diffAgainstFields[d.name].start / 64,
            y1: d => 1 - diffAgainstFields[d.name].end / 64,
            x2: d => 1 - d.start / 64,
            y2: d => 1 - d.end / 64,
            stroke: "#900", strokeWidth: 2,
        }));
    }
    marks.push(on(Plot.dot(filteredTests, {
        x: d => 1 - d.start / 64, y: d => 1 - d.end / 64,
        fill: "#00a", strokeWidth: 2,
    }), { click: onclick }));
    
    const out = Plot.plot({
        marks: marks,
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

function plotPareto(jsonData, otherJsonData) {
    const [initial, frontier] = jsonData["merged-cost-accuracy"];
    let marks = [
        Plot.dot([initial], {
            stroke: "#00a",
            symbol: "square",
            strokeWidth: 2,
        }),
        Plot.line(frontier, {
            stroke: "#00a",
            strokeWidth: 2,
        }),
    ];

    if (otherJsonData) {
        const [initial2, frontier2] = otherJsonData["merged-cost-accuracy"];
        marks = [
            Plot.dot([initial2], {
                stroke: "#900",
                symbol: "square",
                strokeWidth: 2,
            }),
            Plot.line(frontier2, {
                stroke: "#900",
                strokeWidth: 2,
            })
        ].concat(marks);
    }

    const out = Plot.plot({
        marks: marks,
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

function buildCheckboxLabel(classes, text, boolState) {
    return Element("label", { classList: classes }, [
        Element("input", { type: "checkbox", checked: boolState }, []),
        text,
    ]);
}

function buildDiffLine(jsonData, show) {
    const urlInput = Element("input", {
        id: "compare-input", value: compareAgainstURL,
        placeholder: "URL to report or JSON file",
        size: 60,
    }, []);

    var unitText = radioStates[radioState]?.tolerance;

    const submitButton = Element("button", "Compute Diff")
    submitButton.addEventListener("click", async (e) => {
        e.preventDefault();
        radioState = radioState ?? "endAcc";
        otherJsonData = await fetchBaseline(urlInput.value);
        update();
    });

    const toleranceInputField = Element("input", {
        id: `toleranceID`, value: filterTolerance,
        size: 10, style: "text-align:right;",
    }, []);
    toleranceInputField.addEventListener("change", (e) => {
        filterTolerance = toleranceInputField.value;
        update();
    });

    return [
        urlInput,
        unitText && [" Hiding: ±", toleranceInputField, unitText],
        " ", submitButton
    ];
}

function buildCompareForm(jsonData) {
    const formName = "compare-form"

    let radioButtons = [];
    for (let [i, stateInfo] of Object.entries(radioStates)) {
        let radioElt = Element("input", {
            name: formName, id: "compare-" + i,
            type: "radio",
            checked: radioState == i,
        }, []);
        radioElt.addEventListener("click", (e) => {
            radioState = i;
            update();
        });

        let labelElt = Element("label", [radioElt, stateInfo.title]);
        radioButtons.push(labelElt);
    }

    const hideEqual = buildCheckboxLabel("hide-equal", "Hide equal", hideDirtyEqual)
    hideEqual.addEventListener("click", (e) => {
        hideDirtyEqual = ! hideDirtyEqual;
        update();
    })

    return Element("form", {}, [radioButtons, " ", hideEqual]);
}

function summarizeTests(tests) {
    return tests.reduce((acc, test) => {
        acc.totalStart += test.start;
        acc.totalEnd += test.end;
        acc.maxAccuracy += test.bits;
        acc.totalTime += test.time;
        if (test.status == "timeout" || test.status == "crash") {
            acc.crashCount += 1;
        }
        return acc;
    }, { totalStart: 0, totalEnd: 0, maxAccuracy: 0, totalTime: 0, crashCount: 0 });
}

function buildStats(summary) {
    return Element("div", { id: "large" }, [
        Element("div", {}, [
            "Average Percentage Accurate: ",
            Element("span", { classList: "number" }, [
                calculatePercent(summary.totalStart / summary.maxAccuracy), "%",
                Element("span", { classList: "unit" }, [" → ",]),
                calculatePercent(summary.totalEnd / summary.maxAccuracy), "%" ]),
        ]),
        Element("div", {}, [
            "Time:",
            Element("span", { classList: "number" }, [formatTime(summary.totalTime)])
        ]),
        Element("div", {}, [
            "Bad Runs:",
            Element("span", {
                classList: "number",
                title: "Crashes and timeouts are considered bad runs."
            }, [`${summary.crashCount}/${summary.testCount}`])
        ]),
        Element("div", {}, [
            "Speedup:",
            Element("span", {
                classList: "number",
                title: "Aggregate speedup of fastest alternative that improves accuracy."
            }, [calculateSpeedup(summary.mergedCostAccuracy)])
        ]),
    ]);
}

function buildBody(jsonData, otherJsonData) {
    let filterFunction = makeFilterFunction();

    const summary = summarizeTests(jsonData.tests);
    summary.testCount = jsonData.tests.length;
    summary.mergedCostAccuracy = jsonData["merged-cost-accuracy"];

    const stats = buildStats(summary);

    const header = buildHeader("Herbie Results")

    const figureRow = Element("div", { classList: "figure-row" }, [
        Element("figure", { id: "xy" }, [
            Element("h2", {}, [tempXY_A]),
            plotXY(jsonData, otherJsonData, filterFunction),
            Element("figcaption", {}, [tempXY_B])
        ]),
        Element("figure", { id: "pareto" }, [
            Element("h2", {}, [tempPareto_A]),
            plotPareto(jsonData, otherJsonData),
            Element("figcaption", {}, [tempPareto_B])
        ])
    ])

    function buildTableHeader(stringName, help) {
        const textElement = Element("th", {}, [
            toTitleCase(stringName),
            " ",
            (stringName != sortState.key ? "–" : sortState.dir ?  "⏶" : "⏷"),
            help && Element("span", { classList: "help-button", title: help }, ["?"]),
        ]);
        textElement.addEventListener("click", (e) => {
            if (stringName == sortState.key) {
                sortState.dir = !sortState.dir;
            } else {
                sortState.key = stringName;
                sortState.dir = true;
            }
            update();
        })
        return textElement
    }

    const rows = buildTableContents(jsonData, otherJsonData, filterFunction)
    const footer = buildDiffFooter(jsonData, otherJsonData, filterFunction)
    const resultsTable = Element("table", { id: "results" }, [
        Element("thead", {}, [
            Element("tr", {}, [
                buildTableHeader("name"),
                buildTableHeader("start"),
                buildTableHeader("end", resultHelpText),
                buildTableHeader("target", targetHelpText),
                buildTableHeader("time"),
            ]),
        ]),
        rows,
        footer
    ]);
    return [header, stats, figureRow, buildControls(jsonData, rows.length), resultsTable]
}

function compareTests(l, r) {
    let cmp;
    if (sortState.key == "name") {
        cmp = l.name.localeCompare(r.name);
    } else {
        if (l[sortState.key] === false) {
            cmp = 1;
        } else if (r[sortState.key] === false) {
            cmp = -1;
        } else {
            cmp = l[sortState.key] - r[sortState.key];
        }
    }
    if (sortState.dir) cmp = -cmp;
    return cmp;
}

function buildTableContents(jsonData, otherJsonData, filterFunction) {
    var rows = []
    const jsonTest = jsonData.tests.sort(compareTests);
    for (let test of jsonTest) {
        let other = diffAgainstFields[test.name];
        if (filterFunction(test, other)) rows.push(buildRow(test, other));
    }
    return rows;
}

function computeDiffTotal(jsonData, filterFunction) {
    if (!otherJsonData || !radioState) return 0;
    let total = 0;
    for (let test of jsonData.tests) {
        let other = diffAgainstFields[test.name];
        if (!other) continue;
        if (!filterFunction(test, other)) continue;

        if (radioState == "startAcc") {
            const cur = calculatePercent(test.start / test.bits);
            const base = calculatePercent(other.start / other.bits);
            total += cur - base;
        } else if (radioState == "endAcc") {
            const cur = calculatePercent(test.end / test.bits);
            const base = calculatePercent(other.end / other.bits);
            total += cur - base;
        } else if (radioState == "targetAcc") {
            const curMin = getMinimum(test.target);
            const baseMin = getMinimum(other.target);
            if (curMin !== false && baseMin !== false) {
                total += calculatePercent(curMin / test.bits) -
                         calculatePercent(baseMin / other.bits);
            }
        } else if (radioState == "time") {
            total += other.time - test.time;
        }
    }
    return total;
}

function buildDiffFooter(jsonData, otherJsonData, filterFunction) {
    if (!otherJsonData || !radioState) return [];

    const total = computeDiffTotal(jsonData, filterFunction);
    let color = "diff-time-gray";
    let text = "~";

    if (radioState == "time") {
        if (Math.abs(total) > filterTolerance * 1000) {
            if (total > 0) {
                color = "diff-time-green";
                text = "+ " + formatTime(total);
            } else {
                color = "diff-time-red";
                text = "-" + formatTime(Math.abs(total));
            }
        }
    } else {
        if (Math.abs(total.toFixed(1)) > filterTolerance) {
            if (total > 0) {
                color = "diff-time-green";
                text = "+ " + total.toFixed(1) + "%";
            } else {
                color = "diff-time-red";
                text = "-" + Math.abs(total).toFixed(1) + "%";
            }
        }
    }

    const cells = [
        Element("th", {}, ["Total"]),
        radioState == "startAcc" ? Element("td", { classList: color }, [text]) : Element("td", {}, []),
        radioState == "endAcc" ? Element("td", { classList: color }, [text]) : Element("td", {}, []),
        radioState == "targetAcc" ? Element("td", { classList: color }, [text]) : Element("td", {}, []),
        radioState == "time" ? Element("td", { classList: color }, [text]) : Element("td", {}, []),
        Element("td", {}, []),
        Element("td", {}, []),
    ];

    return Element("tfoot", {}, [Element("tr", {}, cells)]);
}

function getMinimum(target) {
    if (target === false) {
        return false
    }

    return target.reduce((minA, current) => {
        const currentA = current[1];
        return currentA < minA ? currentA : minA;
    }, Infinity);
}

// HACK I kinda hate this split lambda function, Zane
function buildRow(test, other) {
    var smallestTarget = getMinimum(test.target)

    if (!other) {
        var startAccuracy = calculatePercent(test.start / test.bits) + "%"
        var resultAccuracy = calculatePercent(test.end / test.bits) + "%"
        var targetAccuracy = calculatePercent(smallestTarget / test.bits) + "%"

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
        return tr;
    } else {
        function timeTD(test) {
            var timeDiff = test.time - other.time
            var color, text
            if (timeDiff > filterTolerance * 1000) {
                color = "diff-time-red";
                text = "-" + formatTime(timeDiff);
            } else if (timeDiff < -filterTolerance * 1000) {
                color = "diff-time-green";
                text = "+ " + formatTime(Math.abs(timeDiff));
            } else {
                color = "diff-time-gray";
                text = "~";
            }
            var titleText = `current: ${formatTime(test.time)} vs ${formatTime(other.time)}`
            return Element("td", { classList: color, title: titleText }, [text]);
        }

        function accuracyTD(testVal, otherVal) {
            const op = calculatePercent(otherVal / other.bits)
            const tp = calculatePercent(testVal / test.bits)
            let diff = op - tp
            let color = "diff-time-red"
            let tdText = `- ${diff.toFixed(1)}%`

            if (Math.abs(diff.toFixed(1)) <= filterTolerance) {
                color = "diff-time-gray"
                tdText = "~"
            } else if (diff < 0) {
                diff = Math.abs(diff)
                color = "diff-time-green"
                tdText = `+ ${diff.toFixed(1)}%`
            }

            const titleText = `Original: ${op} vs ${tp}`
            return Element("td", { classList: color, title: titleText }, [tdText])
        }

        const startAccuracy = accuracyTD(test.start, other.start)
        const resultAccuracy = accuracyTD(test.end, other.end)
        const targetAccuracy = accuracyTD(smallestTarget, other.target)

        var tdStartAccuracy = radioState == "startAcc" ? startAccuracy : Element("td", {}, [calculatePercent(test.start / test.bits), "%"])
        var tdResultAccuracy = radioState == "endAcc" ? resultAccuracy : Element("td", {}, [calculatePercent(test.end / test.bits), "%"])
        var tdTargetAccuracy = radioState == "targetAcc" ? targetAccuracy : Element("td", {}, [calculatePercent(smallestTarget / test.bits), "%"])
        const tdTime = radioState == "time" ? timeTD(test) : Element("td", {}, [formatTime(test.time)])

        var testTitle = ""
        if (test.output != other.output) {
            testTitle = `Current output:\n${test.output} \n \n Comparing to:\n ${other.output}`
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

        const tr = Element("tr", { classList: test.status }, [
            Element("td", { title: testTitle }, [test.name]),
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
        return tr;
    }
}

function buildDiffControls(jsonData) {
    var summary = Element("details", { open: showCompareDetails }, [
        Element("summary", {}, [
            Element("h2", {}, ["Diff"]),
            buildDiffLine(jsonData),
        ]),
        buildCompareForm(jsonData),
    ])

    summary.addEventListener("toggle", (e) => {
        showCompareDetails = summary.open;
    });

    return summary;
}

function buildControls(jsonData, diffCount) {
    const showing = diffCount + "/" + jsonData.tests.length;
    var displayingDiv = Element("div", [
        "Displaying " + showing + " benchmarks",
        " on ", Element("code", jsonData.branch),
        otherJsonData && [
            ", compared with baseline ", Element("code", otherJsonData.branch),
        ],
    ])

    return Element("div", { classList: "report-details" }, [
        displayingDiv,
        buildDiffControls(jsonData),
        buildFilterControls(jsonData),
    ])
}

function buildFilterGroup(jsonData, name, childStateNames) {
    let subFilters = filterGroups[name];
    let label = buildCheckboxLabel(name, toTitleCase(name), filterGroupState[name]);
    label.addEventListener("click", (e) => {
        filterGroupState[name] = e.target.checked;
        for (let filterName of subFilters) {
            filterState[filterName] = e.target.checked;
        }
        update();
    });
    return label;
}

function buildFilterControls(jsonData) {
    var testTypeCounts = {}
    for (let test of jsonData.tests) {
        testTypeCounts[test.status] == null ?
            testTypeCounts[test.status] = 1 :
            testTypeCounts[test.status] += 1
    }

    var filterButtons = []
    for (let f in filterState) {
        const name = `${filterNames[f]} (${testTypeCounts[f] ? testTypeCounts[f] : "0"})`
        const button = buildCheckboxLabel(f + " sub-filter", name, filterState[f])
        button.addEventListener("click", () => {
            filterState[f] = button.querySelector("input").checked
            update()
        })
        filterButtons.push(button)
    }

    const dropDown = buildDropdown(
        allSuites,
        filterBySuite,
        "Filter by suite",
        (value) => { filterBySuite = value; },
    );

    const dropDown2 = buildDropdown(
        allWarnings,
        filterByWarning,
        "Filter to warning",
        (value) => { filterByWarning = value; },
    );

    let groupButtons = [];
    for (let i in filterGroupState) {
        groupButtons.push(buildFilterGroup(jsonData, i));
    }

    const filters = Element("details", { id: "filters", open: showFilterDetails }, [
        Element("summary", {}, [
            Element("h2", {}, "Filters"),
            groupButtons, " ", dropDown, " ", dropDown2,
        ]),
        filterButtons,
    ]);
    filters.addEventListener("toggle", (e) => {
        showFilterDetails = filters.open;
    });
    return filters;
}

function showGetJsonError(error) {
    const header = buildHeader("Error loading results")

    let is_windows = navigator.userAgent.indexOf("Windows") !== -1;
    let page_name = window.location.pathname.split("/").at(-1);
    let page_location;
    if (is_windows) {
        page_location = window.location.pathname.split("/").slice(1, -1).join("\\");
    } else {
        page_location = window.location.pathname.split("/").slice(0, -1).join("/");
    }

    let reason;
    if (window.location.protocol == "file:") {
        reason = [
            Element("p", [
                "Modern browsers prevent access over ",
                Element("code", "file://"), " URLs, which Herbie requires.",
            ]),
            Element("p", [
                "You can work around this by starting a local server, like so:"
            ]),
            Element("pre", [
                is_windows // Python on windows is usually called python, not python3
                    ? "python -m http.server -d " + page_location + " 8123"
                    : "python3 -m http.server -d " + page_location + " 8123"
            ]),
            Element("p", [
                "and then navigating to ",
                Element("a", {
                    href: "http://localhost:8123/" + page_name,
                }, Element("kbd", "http://localhost:8123/" + page_name)),
            ]),
        ];
    }
    
    const message = Element("section", {className: "error"}, [
        Element("h2", "Could not load results"),
        reason,
    ]);

    let body = [header, message];

    let bodyNode = document.querySelector("body");
    if (bodyNode) {
        bodyNode.replaceChildren.apply(bodyNode, body);
    } else {
        document.addEventListener("DOMContentLoaded", () => showGetJsonError(error));
    }
}

function makeFilterFunction() {
    return function filterFunction(baseData, diffData) {

        // Section to hide diffs that are below the provided tolerance
        if (hideDirtyEqual) {
            // Diff Start Accuracy
            if (radioState == "output") {
                if (baseData.output != diffData.output) {
                    return false;
                }
            }
            // Diff Start Accuracy
            if (radioState == "startAcc") {
                const t = baseData.start / baseData.bits
                const o = diffData.start / diffData.bits
                const op = calculatePercent(o)
                const tp = calculatePercent(t)
                var diff = op - tp
                if (Math.abs((diff).toFixed(1)) <= filterTolerance) {
                    return false;
                }
            }
            
            // Diff Result Accuracy
            if (radioState == "endAcc") {
                const t = baseData.end / baseData.bits
                const o = diffData.end / diffData.bits
                const op = calculatePercent(o)
                const tp = calculatePercent(t)
                var diff = op - tp
                if (Math.abs((diff).toFixed(1)) <= filterTolerance) {
                    return false;
                }
            }

            // Diff Target Accuracy
            if (radioState == "targetAcc") {
                var smallestBase = getMinimum(baseData.target)
                var smallestDiff = getMinimum(diffData.target)
                
                const t = smallestBase / baseData.bits
                const o = smallestDiff / diffData.bits
                const op = calculatePercent(o)
                const tp = calculatePercent(t)
                var diff = op - tp
                if (Math.abs((diff).toFixed(1)) <= filterTolerance) {
                    return false;
                }
            }

            // Diff Time
            if (radioState == "time") {
                var timeDiff = baseData.time - diffData.time
                if (Math.abs(timeDiff) < (filterTolerance * 1000)) {
                    return false;
                }
            }
        }

        const linkComponents = baseData.link.split("/")
        if (filterBySuite && linkComponents.length > 1) {
            // defensive lowerCase
            if (filterBySuite.toLowerCase() != linkComponents[0].toLowerCase()) {
                return false
            }
        }

        if (filterByWarning && baseData.warnings.indexOf(filterByWarning) === -1) {
            return false
        }

        if (!filterState[baseData.status]) {
            return false
        }

        return true
    }
}

async function fetchBaseline(url) {
    if (!url) return;

    if (url.endsWith("/")) url += "results.json";
    compareAgainstURL = url;

    let response = await fetch(url, {
        headers: { "content-type": "text/plain" },
        method: "GET",
        mode: "cors",
    });

    const json = await response.json()
    if (json.error) return;

    for (let test of json.tests) {
        diffAgainstFields[test.name] = test;
    }
    return json;
}

async function getResultsJson() {
    if (resultsJsonData == null) {
        let response;
        try {
            response = await fetch("results.json", {
                headers: { "content-type": "application/json" },
            });
        } catch (err) {
            return showGetJsonError(err);
        }
        resultsJsonData = (await response.json());
        storeBenchmarks(resultsJsonData.tests)
    }
}

function storeBenchmarks(tests) {
    var tempDir = {}
    var tempAllWarnings = {}
    for (let test of tests) {
        const linkComponents = test.link.split("/")
        if (linkComponents.length > 1) {
            tempDir[linkComponents[0]] = linkComponents[0]
        }
        for (let warning of test.warnings)  {
            tempAllWarnings[warning] = warning
        }
    }
    allSuites = Object.keys(tempDir);
    allWarnings = Object.keys(tempAllWarnings);
    update();
}

getResultsJson()
