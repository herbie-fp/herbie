/* The report page uses a dummy-React pattern, where every time the
 * page changes we regenerate the whole HTML content from scratch */

// Here is the loaded data:

var compareAgainstURL = ""
var diffAgainstFields = {}
var otherJsonData = null
var resultsJsonData = null
var countsJsonData = null
var costsJsonData = null

function update() {
    const bodyNode = document.querySelector("body");
    bodyNode.replaceChildren.apply(bodyNode, buildBody(resultsJsonData, otherJsonData));
}

class DeferredCurveCache {
    constructor(readTests) {
        this.readTests = readTests;
        this.key = null;
        this.curve = null;
        this.pending = false;
    }

    currentKey() {
        return getCurveCacheKey(this.readTests());
    }

    get() {
        this.ensure();
        const key = this.currentKey();
        return this.key === key ? this.curve : null;
    }

    ensure() {
        const key = this.currentKey();
        if (this.key === key || this.pending) return;
        this.pending = true;
        setTimeout(() => {
            this.key = this.currentKey();
            this.curve = calculateMergedCostAccuracy(this.readTests());
            this.pending = false;
            update();
        }, 0);
    }
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
var filterByWarning = ""

var sortState = {
    key: "name",
    dir: true, // true = descending, false = ascending
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
    const frontier = mergedCostAccuracy[1]
    for (const point of [...frontier].reverse()) {
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

function paretoUnion(curve1, curve2) {
    const curve1Length = curve1.length;
    const curve2Length = curve2.length;
    const output = new Array(curve1Length + curve2Length);
    let outputLength = 0;
    let index1 = 0;
    let index2 = 0;

    while (index1 < curve1Length && index2 < curve2Length) {
        const point1 = curve1[index1];
        const point2 = curve2[index2];
        const cost1 = point1[0];
        const error1 = point1[1];
        const cost2 = point2[0];
        const error2 = point2[1];
        if (cost1 === cost2 && error1 === error2) {
            output[outputLength] = point1;
            outputLength += 1;
            index1 += 1;
            index2 += 1;
        } else if (cost1 <= cost2 && error1 <= error2) {
            index2 += 1;
        } else if (cost1 >= cost2 && error1 >= error2) {
            index1 += 1;
        } else if (error1 < error2) {
            output[outputLength] = point1;
            outputLength += 1;
            index1 += 1;
        } else {
            output[outputLength] = point2;
            outputLength += 1;
            index2 += 1;
        }
    }

    while (index1 < curve1Length) {
        output[outputLength] = curve1[index1];
        outputLength += 1;
        index1 += 1;
    }
    while (index2 < curve2Length) {
        output[outputLength] = curve2[index2];
        outputLength += 1;
        index2 += 1;
    }
    output.length = outputLength;
    return output;
}

const PARETO_BLOCK_SIZE = 32;

function paretoConvex(points) {
    const convex = [];

    for (const point of points) {
        convex.push(point);
        while (convex.length >= 3) {
            const point2 = convex[convex.length - 1];
            const point1 = convex[convex.length - 2];
            const point0 = convex[convex.length - 3];
            const m01 = (point1[1] - point0[1]) / (point1[0] - point0[0]);
            const m12 = (point2[1] - point1[1]) / (point2[0] - point1[0]);
            if (m12 <= m01) {
                break;
            }
            convex.splice(convex.length - 2, 1);
        }
    }

    return convex;
}

function paretoMinimize(points) {
    const pointsSorted = points.slice().sort((left, right) => left[0] - right[0]);
    return pointsSorted.reduce((minimized, point) => paretoUnion([point], minimized), []);
}

function paretoShift([cost0, error0], frontier) {
    return frontier.map(([cost, error]) => [cost0 + cost, error0 + error]);
}

function paretoUnionBalanced(curves) {
    let level = curves;
    while (level.length > 1) {
        const nextLevel = [];
        for (let index = 0; index < level.length; index += 2) {
            if (index + 1 >= level.length) {
                nextLevel.push(level[index]);
            } else {
                nextLevel.push(paretoUnion(level[index], level[index + 1]));
            }
        }
        level = nextLevel;
    }
    return level[0] || [];
}

// Merge shifted frontiers in small balanced batches. This keeps the
// large-large unions that are fast for JS without retaining every
// shifted frontier at once.
function paretoCombineTwo(combined, frontier) {
    if (combined.length === 0) {
        return frontier;
    }

    let combinedFrontier = [];
    for (let index = 0; index < combined.length; index += PARETO_BLOCK_SIZE) {
        const shiftedFrontiers = [];
        const limit = Math.min(index + PARETO_BLOCK_SIZE, combined.length);
        for (let pointIndex = index; pointIndex < limit; pointIndex += 1) {
            shiftedFrontiers.push(paretoShift(combined[pointIndex], frontier));
        }
        combinedFrontier = paretoUnion(paretoUnionBalanced(shiftedFrontiers), combinedFrontier);
    }
    return paretoConvex(combinedFrontier);
}

function paretoCombine(frontiers) {
    let level = frontiers.map((frontier) => paretoConvex(paretoMinimize(frontier)));
    while (level.length > 1) {
        const nextLevel = [];
        for (let index = 0; index < level.length; index += 2) {
            if (index + 1 >= level.length) {
                nextLevel.push(level[index]);
            } else {
                nextLevel.push(paretoCombineTwo(level[index], level[index + 1]));
            }
        }
        level = nextLevel;
    }
    return level[0] || [];
}

function calculateMergedCostAccuracy(tests) {
    const testsLength = tests.length;
    const costAccuracies = tests.map((test) => test["cost-accuracy"]);
    const maximumAccuracy = tests.reduce((sum, test) => sum + test.bits, 0);
    const initialAccuraciesSum = costAccuracies.reduce((sum, costAccuracy) => {
        if (!costAccuracy || costAccuracy.length === 0) {
            return sum;
        }
        return sum + costAccuracy[0][1];
    }, 0);
    const initialAccuracy = maximumAccuracy > 0
        ? 1 - initialAccuraciesSum / maximumAccuracy
        : 1.0;
    const rescaled = costAccuracies
        .filter((costAccuracy) => costAccuracy && costAccuracy.length > 0)
        .map((costAccuracy) => {
            const [initialPoint, bestPoint, otherPoints] = costAccuracy;
            const initialCost = Number(initialPoint[0]);
            return [initialPoint, bestPoint].concat(otherPoints).map((point) => [
                point[0] / initialCost,
                point[1],
            ]);
        });
    const frontier = paretoCombine(rescaled).map(([cost, accuracy]) => {
        if (cost === 0) {
            return ["N/A", 1 - accuracy / maximumAccuracy];
        }
        return [testsLength / cost, 1 - accuracy / maximumAccuracy];
    });
    return [[1.0, initialAccuracy], frontier];
}

function getFilteredTests() {
    return resultsJsonData.tests.filter(filterTest);
}

function getFilteredBaselineTests() {
    return getFilteredTests().map(getBaselineTest);
}

function getCurveCacheKey(tests) {
    return JSON.stringify(tests.map((test) => test.name));
}

const jointCostCache = new DeferredCurveCache(getFilteredTests);
const otherJointCostCache = new DeferredCurveCache(getFilteredBaselineTests);

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
    select.addEventListener("change", () => {
        onChange(select.value);
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

function plotXY(testsData, otherJsonData) {
    const filteredTests = getFilteredTests();
    function onclick(e, d) {
        window.location = d.link + "/graph.html";
    }
    let marks = [
        Plot.line([[0, 0], [1, 1]], { stroke: '#ddd' }),
    ];
    if (otherJsonData) {
        marks.push(Plot.arrow(filteredTests, {
            x1: d => 1 - getBaselineTest(d).start / 64,
            y1: d => 1 - getBaselineTest(d).end / 64,
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
    const mergedCostAccuracy = jointCostCache.get();
    if (mergedCostAccuracy === null) {
        return Element("div", {
            style: "max-width: 100%; aspect-ratio: 1;",
            "aria-label": "Pareto plot loading",
        }, []);
    }

    const [initial, frontier] = mergedCostAccuracy;
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
        const otherMergedCostAccuracy = otherJointCostCache.get();
        if (otherMergedCostAccuracy === null) {
            return Element("div", {
                style: "max-width: 100%; aspect-ratio: 1;",
                "aria-label": "Pareto plot loading",
            }, []);
        }

        const [initial2, frontier2] = otherMergedCostAccuracy;
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

function buildDiffLine() {
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
    toleranceInputField.addEventListener("change", () => {
        filterTolerance = toleranceInputField.value;
        update();
    });

    return [
        urlInput,
        unitText && [" Hiding: ±", toleranceInputField, unitText],
        " ", submitButton
    ];
}

function buildCompareForm() {
    const formName = "compare-form"

    let radioButtons = [];
    for (let [i, stateInfo] of Object.entries(radioStates)) {
        let radioElt = Element("input", {
            name: formName, id: "compare-" + i,
            type: "radio",
            checked: radioState == i,
        }, []);
        radioElt.addEventListener("click", () => {
            radioState = i;
            update();
        });

        let labelElt = Element("label", [radioElt, stateInfo.title]);
        radioButtons.push(labelElt);
    }

    const hideEqual = buildCheckboxLabel("hide-equal", "Hide equal", hideDirtyEqual)
    hideEqual.addEventListener("click", () => {
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

function buildStats(jsonData, mergedCostAccuracy) {
    const summary = summarizeTests(jsonData.tests);
    const speedup = mergedCostAccuracy === null ? "⋯" : calculateSpeedup(mergedCostAccuracy);

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
            }, [`${summary.crashCount}/${jsonData.tests.length}`])
        ]),
        Element("div", {}, [
            "Speedup:",
            Element("span", {
                classList: "number",
                title: "Aggregate speedup of fastest alternative that improves accuracy."
            }, [speedup])
        ]),
    ]);
}

function buildTableHeader(stringName, help) {
    const textElement = Element("th", {}, [
        toTitleCase(stringName),
        " ",
        (stringName != sortState.key ? "–" : sortState.dir ?  "⏶" : "⏷"),
        help && Element("span", { classList: "help-button", title: help }, ["?"]),
    ]);
    textElement.addEventListener("click", () => {
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

function buildBody(jsonData, otherJsonData) {
    const mergedCostAccuracy = jointCostCache.get();
    const stats = buildStats(jsonData, mergedCostAccuracy);

    const header = buildHeader("Herbie Results")

    const figureRow = Element("div", { classList: "figure-row" }, [
        Element("figure", { id: "xy" }, [
            Element("h2", {}, [tempXY_A]),
            plotXY(jsonData, otherJsonData),
            Element("figcaption", {}, [tempXY_B])
        ]),
        Element("figure", { id: "pareto" }, [
            Element("h2", {}, [tempPareto_A]),
            plotPareto(jsonData, otherJsonData),
            Element("figcaption", {}, [tempPareto_B])
        ])
    ])

    const rows = buildTableContents(jsonData)
    const footer = buildDiffFooter(jsonData, otherJsonData)
    const resultsTable = Element("table", { id: "results" }, [
        Element("thead", {}, [
            Element("tr", {}, [
                buildTableHeader("name"),
                buildTableHeader("start"),
                buildTableHeader("end", resultHelpText),
                buildTableHeader("score"),
                buildTableHeader("cost"),
                buildTableHeader("count"),
                buildTableHeader("target", targetHelpText),
                buildTableHeader("time"),
            ]),
        ]),
        rows,
        footer
    ]);
    return [header, stats, figureRow, buildControls(jsonData, otherJsonData, rows.length), resultsTable]
}

function compareTests(l, r) {
    let cmp;
    if (sortState.key == "name") {
        cmp = l.name.localeCompare(r.name);
    } else if (sortState.key == "score") {
        const lv = getScore(l);
        const rv = getScore(r);
        cmp = lv - rv;
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

function getBaselineTest(test) {
    return diffAgainstFields[test.name]
}

function getVisibleTests(jsonData) {
    const visibleTests = []
    for (const test of [...jsonData.tests].sort(compareTests)) {
        if (filterTest(test)) visibleTests.push(test);
    }
    return visibleTests
}

function buildTableContents(jsonData) {
    const visibleTests = getVisibleTests(jsonData);
    return visibleTests.map((test) => buildRow(test, getBaselineTest(test)));
}

function computeDiffTotal(jsonData) {
    if (!otherJsonData || !radioState) return 0;
    let total = 0;
    for (let test of jsonData.tests) {
        let other = getBaselineTest(test);
        if (!other) continue;
        if (!filterTest(test)) continue;

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

function buildDiffFooter(jsonData, otherJsonData) {
    if (!otherJsonData || !radioState) return [];

    const total = computeDiffTotal(jsonData);
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

        Element("td", {}, []),

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

function getCountForTestName(name) {
    if (!countsJsonData) return 0;
    const raw = countsJsonData[name];
    const n = Number(raw);
    return Number.isFinite(n) ? n : 0;
}

function getCostForTestName(name) {
    if (!costsJsonData) return 0;
    const raw = costsJsonData[name];
    const n = Number(raw);
    return Number.isFinite(n) ? n : 0;
}

function getScore(test) {
    const count = getCountForTestName(test.name);
    const cost = getCostForTestName(test.name);
    if (cost === 0)
        return 0 
    const endVal = (test && typeof test.end === "number") ? test.end : 0;
    return ((endVal * count) / (cost / 1000)) * 1000;
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
            Element("td", {}, [String(Math.floor(getScore(test)))]),
            Element("td", {}, [String(Math.floor(getCostForTestName(test.name)))]),
            Element("td", {}, [String(Math.floor(getCountForTestName(test.name)))]),
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
            Element("td", {}, [String(Math.floor(getScore(test)))]),
            Element("td", {}, [String(Math.floor(getCostForTestName(test.name)))]),
            Element("td", {}, [String(Math.floor(getCountForTestName(test.name)))]),
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

function buildDiffControls() {
    var summary = Element("details", { open: showCompareDetails }, [
        Element("summary", {}, [
            Element("h2", {}, ["Diff"]),
            buildDiffLine(),
        ]),
        buildCompareForm(),
    ])

    summary.addEventListener("toggle", () => {
        showCompareDetails = summary.open;
    });

    return summary;
}

function buildControls(jsonData, otherJsonData, diffCount) {
    var displayingDiv = Element("div", [
        "Displaying " + diffCount + "/" + jsonData.tests.length + " benchmarks",
        " on ", Element("code", jsonData.branch),
        otherJsonData && [
            ", compared with baseline ", Element("code", otherJsonData.branch),
        ],
    ])

    return Element("div", { classList: "report-details" }, [
        displayingDiv,
        buildDiffControls(),
        buildFilterControls(jsonData),
    ])
}

function buildFilterGroup(name) {
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

function countTestsByStatus(tests) {
    const counts = {}
    for (const test of tests) {
        counts[test.status] = (counts[test.status] ?? 0) + 1
    }
    return counts
}

function collectSuites(tests) {
    const suites = new Set()
    for (const test of tests) {
        const linkComponents = test.link.split("/")
        if (linkComponents.length > 1) {
            suites.add(linkComponents[0])
        }
    }
    return [...suites]
}

function collectWarnings(tests) {
    const warnings = new Set()
    for (const test of tests) {
        for (const warning of test.warnings)  {
            warnings.add(warning)
        }
    }
    return [...warnings]
}

function buildFilterControls(jsonData) {
    const testTypeCounts = countTestsByStatus(jsonData.tests)

    var filterButtons = []
    for (let f in filterState) {
        const name = `${filterNames[f]} (${testTypeCounts[f] ?? 0})`
        const button = buildCheckboxLabel(f + " sub-filter", name, filterState[f])
        button.addEventListener("click", () => {
            filterState[f] = button.querySelector("input").checked
            update()
        })
        filterButtons.push(button)
    }

    const suiteDropdown = buildDropdown(
        collectSuites(jsonData.tests),
        filterBySuite,
        "Filter by suite",
        (value) => { filterBySuite = value; },
    );

    const warningDropdown = buildDropdown(
        collectWarnings(jsonData.tests),
        filterByWarning,
        "Filter to warning",
        (value) => { filterByWarning = value; },
    );

    let groupButtons = [];
    for (let i in filterGroupState) {
        groupButtons.push(buildFilterGroup(i));
    }

    const filters = Element("details", { id: "filters", open: showFilterDetails }, [
        Element("summary", {}, [
            Element("h2", {}, "Filters"),
            groupButtons, " ", suiteDropdown, " ", warningDropdown,
        ]),
        filterButtons,
    ]);
    filters.addEventListener("toggle", () => {
        showFilterDetails = filters.open;
    });
    return filters;
}

function showGetJsonError() {
    const header = buildHeader("Error loading results")

    const is_windows = navigator.userAgent.indexOf("Windows") !== -1;
    const page_name = window.location.pathname.split("/").at(-1);
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

    const body = [header, message];

    const bodyNode = document.querySelector("body");
    if (bodyNode) {
        bodyNode.replaceChildren.apply(bodyNode, body);
    } else {
        document.addEventListener("DOMContentLoaded", showGetJsonError);
    }
}

function filterDirtyEqual(baseData, diffData) {
    if (!hideDirtyEqual) {
        return false
    }

    if (radioState == "output") {
        return baseData.output == diffData.output
    }

    if (radioState == "startAcc") {
        const t = baseData.start / baseData.bits
        const o = diffData.start / diffData.bits
        const op = calculatePercent(o)
        const tp = calculatePercent(t)
        const diff = op - tp
        return Math.abs((diff).toFixed(1)) <= filterTolerance
    }

    if (radioState == "endAcc") {
        const t = baseData.end / baseData.bits
        const o = diffData.end / diffData.bits
        const op = calculatePercent(o)
        const tp = calculatePercent(t)
        const diff = op - tp
        return Math.abs((diff).toFixed(1)) <= filterTolerance
    }

    if (radioState == "targetAcc") {
        const smallestBase = getMinimum(baseData.target)
        const smallestDiff = getMinimum(diffData.target)

        const t = smallestBase / baseData.bits
        const o = smallestDiff / diffData.bits
        const op = calculatePercent(o)
        const tp = calculatePercent(t)
        const diff = op - tp
        return Math.abs((diff).toFixed(1)) <= filterTolerance
    }

    if (radioState == "time") {
        const timeDiff = baseData.time - diffData.time
        return Math.abs(timeDiff) < (filterTolerance * 1000)
    }

    return false
}

function filterSuite(baseData) {
    const linkComponents = baseData.link.split("/")
    return filterBySuite &&
        linkComponents.length > 1 &&
        // defensive lowerCase
        filterBySuite.toLowerCase() != linkComponents[0].toLowerCase()
}

function filterWarning(baseData) {
    return filterByWarning && baseData.warnings.indexOf(filterByWarning) === -1
}

function filterStatus(baseData) {
    return !filterState[baseData.status]
}

function filterTest(baseData) {
    const diffData = getBaselineTest(baseData)
    if (filterDirtyEqual(baseData, diffData)) return false
    if (filterSuite(baseData)) return false
    if (filterWarning(baseData)) return false
    if (filterStatus(baseData)) return false
    return true
}

async function fetchBaseline(url) {
    if (!url) return;

    if (url.endsWith("/")) url += "results.json";
    compareAgainstURL = url;

    const response = await fetch(url, {
        headers: { "content-type": "text/plain" },
        method: "GET",
        mode: "cors",
    });

    const json = await response.json()
    if (json.error) return;

    diffAgainstFields = Object.fromEntries(json.tests.map((test) => [test.name, test]));
    return json;
}

async function getResultsJson() {
    if (resultsJsonData == null) {
        try {
            const response = await fetch("results.json", {
                headers: { "content-type": "application/json" },
            });
            resultsJsonData = (await response.json());
        } catch (err) {
            return showGetJsonError();
        }
        update();
    }
}

async function getCountsJson() {
    if (countsJsonData != null) return;
    let response;
    try {
        response = await fetch("../counts.json", {
            headers: { "content-type": "application/json" },
        });
    } catch (err) {
        return showGetJsonError(err);
    }
    countsJsonData = await response.json();
    update();
}

async function getCostsJson() {
    if (costsJsonData != null) return;
    let response;
    try {
        response = await fetch("../costs.json", {
            headers: { "content-type": "application/json" },
        });
    } catch (err) {
        return showGetJsonError(err);
    }
    costsJsonData = await response.json();
    update();
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

getCountsJson();
getCostsJson();
getResultsJson();
