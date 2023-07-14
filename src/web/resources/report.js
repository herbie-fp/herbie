window.COMPONENTS = []

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

const Results = new Component("#results", {
    setup: function () {
        // clickable rows
        let $rows = this.elt.querySelectorAll("tbody tr");
        for (let $row of $rows) {
            $row.addEventListener("click", () => $row.querySelector("a").click());
        }

        this.setupFilters()
    },
    setupFilters: function () {
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
        const improvedLeader = this.attachLeaderToChildren("improved","Improved", improvedChildren)
        const regressedLeader = this.attachLeaderToChildren("regressed","Regressed", regressedChildren)

        this.elt.parentNode.insertBefore(Element("div", { id: "filters" }, [
            Element("div", { classList: "section-title" }, "Filters"),
            Element("div", { id: "filter-group" }, [
                improvedLeader, regressedLeader]),
            Element("details", [
                Element("summary", "Advanced"), [
                    improvedChildren, regressedChildren]])]), this.elt)
    },
    attachLeaderToChildren: function (leaderTag,leaderName, childNodes) {
        const parentLabel = this.buildCheckboxLabel(leaderTag, leaderName, true)
        parentLabel.addEventListener("click", () => {
            const parentState = parentLabel.querySelector("input").checked
            childNodes.forEach((child) => {
                const children = this.getRowsForClass(child.classList[0])
                child.checked = parentState
                this.updateChildren(children,parentState)
            })
        })
        return parentLabel
    },
    createChild: function (childName, count) {
        const childNode = this.buildCheckboxLabel(childName, `${renames[childName]} (${count})`, true)
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
        children.forEach((child)  => {
            if (state) {
                child.classList.remove("hidden")
            } else {
                child.classList.add("hidden")
            }
        })
    },
    getRowsForClass: function(childTag) {
        return this.elt.querySelectorAll(`tr.${childTag}`)
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
      r.on(type, function(event, i) {
          return callback(event, data[i]);
      });
    }
    return g;
  };
  return mark;
}

var Subreport = new Component("#subreports", {
    setup: function() {
        this.elt.classList.add("no-subreports");
        this.button = Element("a", {id: "subreports-toggle"}, "See subreports");
        this.button.addEventListener("click", this.toggle);
        this.elt.insertBefore(this.button, this.elt.children[0]);
    },
    toggle: function() {
        this.elt.classList.toggle("no-subreports");
        var changed_only = this.elt.classList.contains("no-subreports");
        this.button.innerText = changed_only ? "See subreports" : "Hide subreports";
    }
});

var TogglableFlags = new Component("#flag-list", {
    setup: function() {
        this.elt.classList.add("changed-flags");
        this.button = Element("a", {id: "flag-list-toggle"}, "see all");
        this.button.addEventListener("click", this.toggle);
        this.elt.insertBefore(this.button, this.elt.children[0]);
    },
    toggle: function() {
        this.elt.classList.toggle("changed-flags");
        var changed_only = this.elt.classList.contains("changed-flags");
        this.button.innerText = changed_only ? "see all" : "see diff";
    }
});

var TryIt = new Component("#try-it", {
    depends: function() {
        if (typeof window.start === "undefined") throw "start() function not defined";
        if (typeof window.end === "undefined") throw "end() function not defined";
    },
    setup: function() {
        this.origOut = this.elt.querySelector("#try-original-output");
        this.herbieOut = this.elt.querySelector("#try-herbie-output");
        this.result = this.elt.querySelector("#try-result");
        this.inputs = this.elt.querySelectorAll("#try-inputs input");
        this.submit();
        for (var i = 0; i < this.inputs.length; i++) {
            this.inputs[i].addEventListener("input", this.submit);
        }
    },
    submit: function() {
        var values = [];
        for (var i = 0; i < this.inputs.length; i++) {
            var val = parseFloat(this.inputs[i].value);
            if (isNaN(val)) {
                if (this.inputs[i].value.length != 0) {
                    // Don't update error message if there is no input
                    this.result.className = 'error'
                }
                return;
            } else {
                this.result.className = 'no-error'
                values.push(val);
            }
        }
        this.origOut.innerHTML = start.apply(null, values);
        this.herbieOut.innerHTML = end.apply(null, values);
    },
});

const ALL_LINES = [
    { name: 'start', description: "Initial program",
      line: { stroke: '#d00' }, dot: { stroke: '#d002'} },
    { name: 'end', description: "Most accurate alternative",
      line: { stroke: '#00a' }, dot: { stroke: '#00a2'} },
    { name: 'target', description: "Developer target",
      line: { stroke: '#080' }, dot: { stroke: '#0802'}}
]

const ClientGraph = new Component('#graphs', {
    setup: async function() {
        const points = await fetch("points.json", {
                headers: {"content-type": "text/plain"},
                method: "GET",
                mode: 'cors'
        });
        this.points_json = await points.json();
        this.all_vars = this.points_json.vars;
        this.$variables = this.elt.querySelector("#variables");
        this.$functions = this.elt.querySelector("#functions");
        await this.render(this.all_vars[0], ['start', 'end']);
    },

    render_variables: function($elt, selected_var_name, selected_functions) {
        $elt.replaceChildren(
            Element("select", {
                oninput: (e) => this.render(e.target.value, selected_functions),
            }, this.all_vars.map(v =>
                Element("option", {
                    value: v,
                    selected: selected_var_name == v,
                }, v)
            )));
    },

    render_functions: function($elt, selected_var_name, selected_functions) {
        const all_lines = ALL_LINES.filter(o => this.points_json.error[o.name] != false)
        const toggle = (option, options) => options.includes(option) ? options.filter(o => o != option) : [...options, option]
        $elt.replaceChildren.apply(
            $elt,
            all_lines.map(fn => 
                Element("label", [
                    Element("input", {
                        type: "checkbox",
                        style: "accent-color: " + fn.line.stroke,
                        checked: selected_functions.includes(fn.name),
                        onclick: (e) => this.render(selected_var_name, toggle(fn.name, selected_functions))
                    }, []),
                    Element("span", { className: "functionDescription" }, [
                        " ", fn.description]),
                ])),
        );
    },
    
    sliding_window: function(A, size) {
        const half = Math.floor(size / 2)
        const running_sum = A.reduce((acc, v) => (acc.length > 0 ? acc.push(v.y + acc[acc.length - 1]) : acc.push(v.y), acc), [])
        return running_sum.reduce((acc, v, i) => {
            const length = 
                  (i - half) < 0 ? half + i
                  : (i + half) >= running_sum.length ? (running_sum.length - (i - half))
                  : size
            const top =
                  (i + half) >= running_sum.length ? running_sum[running_sum.length - 1]
                  : running_sum[i + half]
            const bottom =
                  (i - half) < 0 ? 0
                  : running_sum[i - half]
            acc.push({average: (top - bottom) / length, x: A[i].x, length})
            return acc
        }, [])
    },

    plot: async function(varName, function_names) {
        const functions = ALL_LINES.filter(o => function_names.includes(o.name))
        const index = this.all_vars.indexOf(varName)
        // NOTE ticks and splitpoints include all vars, so we must index
        const { bits, points, error, ticks_by_varidx, splitpoints_by_varidx } = this.points_json
        const ticks = ticks_by_varidx[index]
        if (!ticks) {
            return Element("div", "The function could not be plotted on the given range for this input.")
        }
        const tick_strings = ticks.map(t => t[0])
        const tick_ordinals = ticks.map(t => t[1])
        const tick_0_index = tick_strings.indexOf("0")
        const grouped_data = points.map((p, i) => ({
            input: p,
            error: Object.fromEntries(function_names.map(name => ([name, error[name][i]])))
        }))
        const domain = [Math.min(...tick_ordinals), Math.max(...tick_ordinals)]

        let splitpoints = splitpoints_by_varidx[index].map(p => {
            return Plot.ruleX([p], { stroke: "#888" });
        });
        if (tick_strings.includes("0")) {
            splitpoints.push(Plot.ruleX([
                tick_ordinals[tick_strings.indexOf("0")]
            ], { stroke: "#888" }));
        }

        let marks = []
        for (let { name, fn, line, dot } of functions) {
            const key_fn = fn => (a, b) => fn(a) - fn(b)
            const index = this.all_vars.indexOf(varName)
            const data = grouped_data.map(({ input, error }) => ({
                x: input[index],
                y: 1 - error[name] / bits
            })).sort(key_fn(d => d.x))
                  .map(({ x, y }, i) => ({ x, y, i }))
            const compress = (L, out_len, chunk_compressor = points => points[0]) => L.reduce((acc, pt, i) => i % Math.floor(L.length / out_len) == 0 ? (acc.push(chunk_compressor(L.slice(i, i + Math.floor(L.length / out_len)))), acc) : acc, [])
            const bin_size = 128
            const sliding_window_data = compress(
                this.sliding_window(data, bin_size), 800, points => ({
                    average: points.reduce((acc, e) => e.average + acc, 0) / points.length,
                    x: points.reduce((acc, e) => e.x + acc, 0) / points.length
                }))
            marks = marks.concat([
                Plot.dot(compress(data, 800), {
                    x: "x", y: "y", r: 1.3,
                    title: d => `x: ${d.x} \n i: ${d.i} \n bits of error: ${d.y}`,
                    ...dot
                }),
                Plot.line(sliding_window_data, {
                    x: "x",
                    y: "average",
                    strokeWidth: 2, ...line,
                }),
            ]);
        }
        const out = Plot.plot({
            width: '800',
            height: '300',
            marks: splitpoints.concat(marks),
            x: {
                tickFormat: d => tick_strings[tick_ordinals.indexOf(d)],
                ticks: tick_ordinals, label: varName,
                line: true, grid: true,
                domain,
            },
            y: { line: true, domain: [0, 1], tickFormat: "%",},
            marginBottom: 0,
            marginRight: 0,
        });
        out.setAttribute('viewBox', '0 0 820 320')
        return out
    },

    render: async function(selected_var_name, selected_functions) {
        this.render_variables(this.$variables, selected_var_name, selected_functions);
        this.render_functions(this.$functions, selected_var_name, selected_functions);
        let $svg = this.elt.querySelector("svg");
        this.elt.replaceChild(await this.plot(selected_var_name, selected_functions), $svg);
    }
})

const ResultPlot = new Component('#xy', {
    setup: async function() {
        let response = await fetch("results.json", {
            headers: {"content-type": "text/plain"},
            method: "GET",
            mode: "cors",
        });
        let stub = this.elt.querySelector("svg");
        let data = (await response.json()).tests;
        this.elt.replaceChild(this.plot(data), stub)
    },
    plot: function(tests) {
        const out = Plot.plot({
            marks: [
                Plot.line([[0, 0], [1, 1]], {stroke: '#ddd'}),
                on(Plot.dot(tests, {
                    x: d => 1 - d.start/64, y: d => 1 - d.end/64,
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
})

const MergedCostAccuracy = new Component('#pareto', {
    setup: async function() {
        let response = await fetch('results.json', {
            headers: {"content-type": "text/plain"},
            method: "GET",
            mode: 'cors'
        });
        let stub = this.elt.querySelector("svg");
        let json = await response.json();
        const [initial, frontier] = json["merged-cost-accuracy"];
        this.elt.replaceChild(this.plot(initial, frontier), stub)
    },

    plot: function(initial, frontier) {
        const out = Plot.plot({
            marks: [
                Plot.dot([initial], {
                    stroke: "#d00",
                    symbol: "square",
                    strokeWidth: 2,
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

const CostAccuracy = new Component('#cost-accuracy', {
    setup: async function() {
        const $svg = this.elt.querySelector("svg");
        const $tbody = this.elt.querySelector("tbody");

        let response = await fetch("../results.json", {
            headers: {"content-type": "text/plain"},
            method: "GET",
            mode: "cors",
        });
        let results_json = await response.json();
        
        // find right test by iterating through results_json
        for (let test of results_json.tests) {
            if (test.name == this.elt.dataset.benchmarkName) {
                let [initial_pt, best_pt, rest_pts] = test["cost-accuracy"];
                let target_pt = test["target"] && [this.elt.dataset.targetCost, test["target"]]
                rest_pts = [best_pt].concat(rest_pts)
                $svg.replaceWith(await this.plot(test, initial_pt, target_pt, rest_pts));
                $tbody.replaceWith(await this.tbody(test, initial_pt, target_pt, rest_pts));
                break;
            }
        }
    },

    plot: async function(benchmark, initial_pt, target_pt, rest_pts) {
        const bits = benchmark["bits"];

        // The line differs from rest_pts in two ways:
        // - We filter to the actual pareto frontier, in case points moved
        // - We make a broken line to show the real Pareto frontier
        let line = []
        let last = null;
        for (let pt of rest_pts) {
            if (!last || pt[1] > last[1]) {
                if (last) line.push([pt[0], last[1]]);
                line.push([pt[0], pt[1]]);
                last = pt;
            }
        }

        const out = Plot.plot({
            marks: [
                Plot.line(line, {
                    x: d => initial_pt[0]/d[0],
                    y: d => 1 - d[1]/bits,
                    stroke: "#00a", strokeWidth: 1, strokeOpacity: .2,
                }),
                Plot.dot(rest_pts, {
                    x: d => initial_pt[0]/d[0],
                    y: d => 1 - d[1]/bits,
                    fill: "#00a", r: 3,
                }),
                Plot.dot([initial_pt], {
                    x: d => initial_pt[0]/d[0],
                    y: d => 1 - d[1]/bits,
                    stroke: "#d00", symbol: "square", strokeWidth: 2
                }),
                target_pt && Plot.dot([target_pt], {
                    x: d => initial_pt[0]/d[0],
                    y: d => 1 - d[1]/bits,
                    stroke: "#080", symbol: "circle", strokeWidth: 2
                }),
            ].filter(x=>x),
            marginBottom: 0,
            marginRight: 0,
            width: '400',
            height: '200',
            x: { line: true, nice: true, tickFormat: c => c + "×" },
            y: { nice: true, line: true, domain: [0, 1], tickFormat: "%" },
        })
        out.setAttribute('viewBox', '0 0 420 220')
        return out
    },

    tbody: async function(benchmark, initial_pt, target_pt, rest_pts) {
        const bits = benchmark["bits"];
        const initial_accuracy = 100*(1 - initial_pt[1]/bits);

        return Element("tbody", [
            Element("tr", [
                Element("th", "Initial program"),
                Element("td", initial_accuracy.toFixed(1) + "%"),
                Element("td", "1.0×")
            ]),
            rest_pts.map((d, i) => {
                let accuracy = 100*(1 - d[1]/bits);
                let speedup = initial_pt[0]/d[0];
                return Element("tr", [
                    Element("th",
                        rest_pts.length > 1 ?
                            Element("a", { href: "#alternative" + (i + 1)},
                                "Alternative " + (i + 1)) 
                            // else
                            : "Alternative " + (i + 1)
                    ),
                    Element("td", { className: accuracy >= initial_accuracy ? "better" : "" },
                            accuracy.toFixed(1) + "%"),
                    Element("td", { className: speedup >= 1 ? "better" : "" },
                            speedup.toFixed(1) + "×")
            ])}),
            target_pt && Element("tr", [
                Element("th", "Developer target"),
                Element("td", 100 * (1 - target_pt[1]/bits).toFixed(1) + "%"),
                Element("td", (initial_pt[0] / target_pt[0]).toFixed(1) + "×"),
            ]),
        ]);
    }
});

var RenderMath = new Component(".math", {
    depends: function() {
        if (typeof window.renderMathInElement === "undefined") throw "KaTeX unavailable";
    },
    setup: function() {
        renderMathInElement(this.elt);
    },
});

var Timeline = new Component(".timeline", {
    setup: function() {
        var ts = this.elt.querySelectorAll(".timeline-phase");
        for (var i = 0; i < ts.length; i++) {
            var timespan = +ts[i].getAttribute("data-timespan");
            var type = ts[i].getAttribute("data-type");
            ts[i].style.flexGrow = timespan;
            ts[i].title = type + " (" + Math.round(timespan/100)/10 + "s)";
        }
    }
});

var Bogosity = new Component(".bogosity", {
    setup: function() {
        var ts = this.elt.children;
        for (var i = 0; i < ts.length; i++) {
            var timespan = +ts[i].getAttribute("data-timespan");
            ts[i].style.flexGrow = timespan;
        }
    }
});

var Implementations = new Component(".programs", {
    setup: function() {
        this.dropdown = this.elt.querySelector("select");
        this.programs = this.elt.querySelectorAll(".implementation");
        this.elt.addEventListener("change", this.change);
        this.change();
    },
    change: function() {
        var lang = this.dropdown.options[this.dropdown.selectedIndex].text;
        for (var i = 0; i < this.programs.length; i++) {
            var $prog = this.programs[i];
            if ($prog.dataset["language"] == lang) {
                $prog.style.display = "block";
            } else {
                $prog.style.display =  "none";
            }
        }
    },
});

function pct(val, base) {
    return Math.floor(val/base * 10000) / 100 + "%";
}

function time(s) {
    return Math.floor(s / 1000 * 100) / 100 + "s";
}

function path(p) {
    if (!p) {
        return "???";
    } else if (p[0] == "/") {
        var r = p.substr(p.toLowerCase().indexOf("/racket") + 1);
        var ds = r.split("/");
        if (ds[1] == "share" && ds[2] == "pkgs") {
            return "/" + ds.slice(3).join("/");
        } else if (ds[1] == "collects") {
            return "/" + ds.slice(2).join("/");
        } else {
            return "/" + ds.join("/");
        }
    } else {
        return p;
    }
}

var Profile = new Component("#profile", {
    setup: function() {
        var text = this.elt.querySelector(".load-text");
        fetch("profile.json")
            .then(response => response.json())
            .catch(function(error) { text.textContent = "Error loading profile data" })
            .then(data => this.render(data))
    },
    render: function(json) {
        this.json = json;
        this.search = Element("input", {
            placeholder: "Search for a function...",
            autocomplete: "off",
            name: "profilefn",
        }, []);
        this.search.setAttribute("list", "profilefns");
        var form = Element("form", { method: "GET", action: "" }, [
            this.search,
            Element("datalist", { id: "profilefns" }, [
                json.nodes.map(n => n.id && Element("option", n.id))
            ]),
        ]);
        form.addEventListener("submit", this.doSearch);
        this.elt.appendChild(form);
        this.elt.appendChild(this.mkNode(json.nodes[json.nodes[0].callees[0].callee]));
        this.elt.classList.add("loaded");
    },
    mkNode: function(node) {
        var that = this;
        var nelt = Element("div", { className: "node" }, [
            Element("a", { className: "name delete" }, node.id || "???"),
            Element("span", { className: "path" }, path(node.src)),
            Element("span", {
                className: "pct",
                title: "Self-time: " + pct(node.self, that.json.cpu_time) }, [
                    time(node.total),
                ]),
        ]);
        var elt = Element("div", { className: "profile-row" }, [
            node.callers.sort((e1, e2) => e1.caller_time - e2.caller_time).map(function(edge) {
                var other = that.json.nodes[edge.caller];
                elt = Element("div", { className: "edge" }, [
                    Element("a", { className: "name" }, other.id || "???"),
                    Element("span", { className: "path" }, path(other.src)),
                    Element("span", { className: "pct" }, pct(edge.caller_time, node.total)),
                ]);
                elt.children[0].addEventListener("click", that.addElt(other));
                return elt;
            }),
            nelt,
            node.callees.sort((e1, e2) => e2.callee_time - e1.callee_time).map(function(edge) {
                var other = that.json.nodes[edge.callee];
                elt = Element("div", { className: "edge" }, [
                    Element("a", { className: "name" }, other.id || "???"),
                    Element("span", { className: "path" }, path(other.src)),
                    Element("span", { className: "pct" }, pct(edge.callee_time, node.total)),
                ]);
                elt.children[0].addEventListener("click", that.addElt(other));
                return elt;
            }),
        ]);
        nelt.children[0].addEventListener("click", function() { elt.remove(); });
        return elt;
    },
    addElt: function(other) {
        var that = this;
        return function() {
            var newelt = that.mkNode(other)
            that.elt.appendChild(newelt);
            newelt.scrollTo();
            return newelt;
        }
    },
    doSearch: function(e) {
        e.preventDefault();
        var term = this.search.value;
        var elt = this.addElt(this.json.nodes.find(n => n.id == term))();
        elt.scrollTo();
        this.search.value = "";
        return false;
    }
})

function makelabel(i, base, factor) {
    var num = i;
    var den = 1;

    if (base > 0) num *= Math.pow(10, base);
    else if (base < 0) den *= Math.pow(10, -base);

    if (factor > 0) num *= Math.pow(2, factor);
    if (factor < 0) den *= Math.pow(2, -factor);

    return num / den;
}

function histogram(id, data, options) {
    var width = options?.width ?? 676;
    var height = options?.height ?? 60;
    var margin = 5;
    var labels = 10;
    var ticks = 5;
    var bucketnum = options?.buckets ?? 25;
    var bucketwidth = Math.round(width / bucketnum);

    var proportional = options?.proportional ?? true;

    var canvas = document.getElementById(id);
    if (data.length == 0) { return canvas.remove(); } // Early exit

    canvas.setAttribute("width", margin + width + margin + "px");
    canvas.setAttribute("height", labels + margin + height + ticks + margin + labels + "px");
    var ctx = canvas.getContext("2d");
      
    ctx.beginPath();
    ctx.strokeStyle = "black";
    ctx.moveTo(margin, labels + margin + height);
    ctx.lineTo(margin + width, labels + margin + height);
    ctx.stroke();
    
    var xma = options?.max ?? Math.max.apply(null, data);
      
    var buckets = Array(bucketnum);
    var sum = 0;
    buckets.fill(0);
    for (var i = 0; i < data.length; i++) {
        var j = Math.floor(data[i] / xma * buckets.length);
        var x = proportional ? data[i] : 1;
        buckets[Math.min(j, buckets.length-1)] += x;
        sum += x;
    }
    var yma = Math.max.apply(null, buckets);
    
    ctx.fillStyle = "rgba(0, 0, 0, .2)";
    for (var i = 0; i < buckets.length; i++) {
        ctx.fillRect(margin + i/buckets.length*width, labels + margin + height, width/buckets.length, -height*buckets[i]/yma);
    }

    ctx.fillStyle = "black";
    ctx.textBaseline = "bottom";
    ctx.textAlign = "center";
    for (var i = 0; i < buckets.length; i++) {
        if (buckets[i] == 0) continue;
        ctx.fillText(Math.round(buckets[i] / sum * 100) + "%", margin + (i + .5)/buckets.length * width, labels + height*(1 - buckets[i]/yma));
    }
    
    ctx.textBaseline = "top";
    var base = Math.round(Math.log10(xma)) - 1
    var step = Math.pow(10, base);

    var factor;
    if (xma / step > 20) factor = +1;
    else if (xma / step < 10) factor = -1;
    else factor = 0;

    step *= Math.pow(2, factor);

    for (var i = 0; i < 10 * Math.sqrt(10); i++) {
        var pos = i * step;
        if (pos > xma) break;
        ctx.beginPath();
        ctx.moveTo(pos / xma * width + margin, labels + margin + height);
        ctx.lineTo(pos / xma * width + margin, labels + margin + height + ticks);
        var label = makelabel(i, base, factor);
        ctx.fillText(label, pos / xma * width + margin, labels + margin + height + ticks + margin);
        ctx.stroke();
    }
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
