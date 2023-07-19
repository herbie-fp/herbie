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
                    "42.2%",]),
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

        this.elt.parentNode.replaceChild(Element("body", {}, [
            header,
            stats,
            figureRow
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
    }
})

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
