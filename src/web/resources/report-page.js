window.COMPONENTS = []

var resultsJsonData = null

const ReportPage = new Component("body", {
    setup: async function () {
        let data = await this.getResultsJson()
        this.update(data)
    },
    getResultsJson: async function () {
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
    },
    update: function (jsonData) {
        const navigation = Element("nav", {}, [
            Element("ul", {}, [Element("li", {}, [Element("a", { href: "timeline.html" }, ["Metrics"])])])
        ])

        const header = Element("header", {}, [
            Element("h1", {}, "Results"),
            Element("img", { src: "logo-car.png" }, []),
            navigation,
        ])

        const stats = Element("div", { id: "large" }, [
            Element("div", {}, [
                "Average Percentage Accurate: ",
                Element("span", { classList: "number" }, [
                    "42.2%",
                    Element("span", { classList: "unit" }, [" → ",]),
                    "97.2%",]),
            ]),
            Element("div", {}, [
                "Time:",
                Element("span", { classList: "number" }, ["7.5min"])
            ]),
            Element("div", {}, [
                "Bad Runs:",
                Element("span", { classList: "number", title: "Crashes and timeouts are considered bad runs." }, ["0/28"])
            ]),
            Element("div", {}, [
                "Speedup:",
                Element("span", { classList: "number", title: "Aggregate speedup of fastest alternative that improves accuracy." }, ["8.9x"])
            ]),
        ])
        // TODO get these from Json?
        const tempXY_A = "Output vs Input Accuracy"
        const tempXY_B = "Each point represents a Herbie run below. Its horizontal position shows initial accuracy, and vertical position shows final accuracy. Points above the line are improved by Herbie."

        const tempPareto_A = "Accuracy vs Speed"
        const tempPareto_B = "A joint speed-accuracy pareto curve. Accuracy is on the vertical axis, speed is on the horizontal axis. Up and to the right is better. The initial program is shown by the red square."

        const figureRow = Element("div", { classList: "figure-row" }, [
            Element("figure", { id: "xy" }, [
                Element("h2", {}, [tempXY_A]),
                this.plotXY(jsonData.tests),
                Element("figcaption", {}, [tempXY_B])
            ]),
            Element("figure", { id: "pareto" }, [
                Element("h2", {}, [tempPareto_A]),
                this.plotPareto(jsonData),
                Element("figcaption", {}, [tempPareto_B])
            ])
        ])

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
            this.tableBody(jsonData)
        ])
        
        this.elt.parentNode.replaceChild(Element("body", {}, [
            header,
            stats,
            figureRow,
            this.buildFilters(),
            resultsTable,
        ]), this.elt)
    },
    plotXY: function (tests) {
        const out = Plot.plot({
            marks: [
                Plot.line([[0, 0], [1, 1]], { stroke: '#ddd' }),
                on(Plot.dot(tests, {
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
    },
    plotPareto: function (jsonData) {
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
    },
    tableBody: function (jsonData) {
        var rows = []
        for (let test of jsonData.tests) {
            rows.push(this.tableRow(test, rows.length))
        }
        return Element("tbody", {}, rows)
    },
    tableRow: function (test, i) {
        const tr = Element("tr", { classList: test.status }, [
            Element("td", {}, [test.name]),
            Element("td", {}, ["%"]),
            Element("td", {}, ["%"]),
            Element("td", {}, ["%"]),
            Element("td", {}, [`${test.time}`]),
            Element("td", {}, [Element("a", { id: `test${i}`, href: `${test.link}/graph.html` }, ["»"])]),
        ])
        tr.addEventListener("click", () => tr.querySelector("a").click())
        return tr
    },
    buildFilters: function () {
        const regressedTags = ["uni-start", "lt-target", "lt-start",
            "apx-start", "timeout", "crash", "error"]
        const improvedTags = ["imp-start", "ex-start", "eq-start", "eq-target",
            "gt-target"]

        const improvedChildren = improvedTags.map((child) => {
            const count = this.getRowsForClass(child).length
            return this.createChild(child, count)
        })
        const regressedChildren = regressedTags.map((child) => {
            const count = this.getRowsForClass(child).length
            return this.createChild(child, count)
        })

        // add listeners
        const improvedLeader = this.attachLeaderToChildren("improved", "Improved", improvedChildren)
        const regressedLeader = this.attachLeaderToChildren("regressed", "Regressed", regressedChildren)

        return Element("div", { id: "filters" }, [
            Element("div", { classList: "section-title" }, "Filters"),
            Element("div", { id: "filter-group" }, [
                improvedLeader, regressedLeader]),
            Element("details", [
                Element("summary", "Advanced"), [
                    improvedChildren, regressedChildren]])])
    },
    attachLeaderToChildren: function (leaderTag, leaderName, childNodes) {
        const parentLabel = this.buildCheckboxLabel(leaderTag, leaderName, true)
        parentLabel.addEventListener("click", () => {
            const parentState = parentLabel.querySelector("input").checked
            childNodes.forEach((child) => {
                const children = this.getRowsForClass(child.dataset.label)
                child.querySelector("input").checked = parentState
                this.updateChildren(children, parentState)
            })
        })
        return parentLabel
    },
    createChild: function (childName, count) {
        const childNode = this.buildCheckboxLabel(childName, `${renames[childName]} (${count})`, true)
        childNode.dataset.label = childName
        childNode.addEventListener("click", (e) => {
            const thisChild = e.target.querySelector("input")
            if (thisChild == null) { return }
            const childr = this.getRowsForClass(childName)
            this.updateChildren(childr, !thisChild.checked)
        })
        return childNode
    },
    buildCheckboxLabel: function (idTag, text, boolState) {
        return Element("label", { classList: idTag }, [
            Element("input", { type: "checkbox", checked: boolState }, []),
            text])
    },
    updateChildren: function (children, state) {
        children.forEach((child) => {
            if (state) {
                child.classList.remove("hidden")
            } else {
                child.classList.add("hidden")
            }
        })
    },
    getRowsForClass: function (childTag) {
        return this.elt.querySelectorAll(`tr.${childTag}`)
    }
})

// TODO not sure if this is needed any more
var Subreport = new Component("#subreports", {
    setup: function () {
        this.elt.classList.add("no-subreports")
        this.button = Element("a", { id: "subreports-toggle" }, "See subreports")
        this.button.addEventListener("click", this.toggle)
        this.elt.insertBefore(this.button, this.elt.children[0])
    },
    toggle: function () {
        this.elt.classList.toggle("no-subreports")
        var changed_only = this.elt.classList.contains("no-subreports")
        this.button.innerText = changed_only ? "See subreports" : "Hide subreports"
    }
})

const renames = {
    "imp-start": "Improved start",
    "apx-start": "Approximate start",
    "uni-start": "Regressed from start",
    "ex-start": "Exact start",
    "eq-start": "Equal start",
    "lt-start": "Less than start",
    "gt-start": "Greater than start",
    "gt-target": "Greater than target",
    "eq-target": "Equal than target",
    "lt-target": "Less than target",
    "error": "Error",
    "timeout": "Timeout",
    "crash": "Crash",
}

// Based on https://observablehq.com/@fil/plot-onclick-experimental-plugin
// However, simplified because we don't need hit box data
function on(mark, listeners = {}) {
    const render = mark.render;
    mark.render = function (facet, { x, y }, channels) {
        const data = this.data;
        const g = render.apply(this, arguments);
        const r = d3.select(g).selectChildren();
        for (const [type, callback] of Object.entries(listeners)) {
            r.on(type, function (event, i) {
                return callback(event, data[i]);
            });
        }
        return g;
    };
    return mark;
}

function Component(selector, fns) {
    this.selector = selector;
    this.fns = fns;
    window.COMPONENTS.push(this);
}

function ComponentInstance(elt, component) {
    for (var i in component.fns) {
        if (component.fns.hasOwnProperty(i)) {
            this[i] = component.fns[i].bind(this);
        }
    }
    this.elt = elt;
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

function run_components() {
    for (var i = 0; i < window.COMPONENTS.length; i++) {
        var component = window.COMPONENTS[i];
        var elts = document.querySelectorAll(component.selector);

        try {
            if (elts.length > 0 && component.fns.depends) component.fns.depends();
        } catch (e) {
            console.error(e);
            continue;
        }

        for (var j = 0; j < elts.length; j++) {
            var instance = new ComponentInstance(elts[j], component);
            console.log("Initiating", component.selector, "component at", elts[j]);
            try {
                instance.setup();
            } catch (e) {
                console.error(e);
            }
        }
    }
}

window.addEventListener("load", run_components);
