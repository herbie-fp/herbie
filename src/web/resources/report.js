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


// Cicular color wheel representing error values limited to size 10
const colors = [
    { line: { stroke: '#d00' }, dot: { stroke: '#d002'} },
    { line: { stroke: '#00a' }, dot: { stroke: '#00a2'} },
    { line: { stroke: '#080' }, dot: { stroke: '#0802'} },
    { line: { stroke: '#0d0' }, dot: { stroke: '#0d02'} },
    { line: { stroke: '#a00' }, dot: { stroke: '#a002'} },
    { line: { stroke: '#0a0' }, dot: { stroke: '#0a02'} },
    { line: { stroke: '#00d' }, dot: { stroke: '#00d2'} },
    { line: { stroke: '#008' }, dot: { stroke: '#0082'} },
    { line: { stroke: '#d80' }, dot: { stroke: '#d802'} },
    { line: { stroke: '#00f' }, dot: { stroke: '#00f2'} },
    { line: { stroke: '#f00' }, dot: { stroke: '#f002'} }
];

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
        const toggle = (option, options) => options.includes(option) ? options.filter(o => o != option) : [...options, option]

        // this.points_json.error is a dictionary where keys are indices "1", "2", ..., "n".
        // The values for each key is broken up into two parts : error_type and actual error values
        // error_type is the first element in the list value with 1...n being the remianing values
        // Types of error type is "start", "end", "target1", "target2", ..., "targetm" 

        var curr_list = []
        let i = 0

        for (const key in this.points_json.error) {
            const error_type = this.points_json.error[key][0]
            const line = colors[i % colors.length].line

            let description

            if (error_type === "start") {
                description = "Initial program"
            } else if (error_type === "end") {
                description = "Most accurate alternative"
            } else {
                description = "Developer Target " + error_type.slice("target".length)
            }

            curr_list.push( Element("label", [
                Element("input", {
                    type: "checkbox",
                    style: "accent-color: " + line.stroke,
                    checked: selected_functions.includes(error_type),
                    onclick: (e) => this.render(selected_var_name, toggle(error_type, selected_functions))
                }, []),
                Element("span", { className: "functionDescription" }, [
                    " ", description]),
            ]))

            i += 1
        }

        $elt.replaceChildren.apply(
            $elt,
            curr_list,
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
        const functionMap = new Map()

        for (const key in this.points_json.error) {
            if (this.points_json.error[key][1] !== false) {
                // Error type -> Actual Error points
                functionMap.set(this.points_json.error[key][0], this.points_json.error[key].slice(1))
            }
        }

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
            error: Object.fromEntries(function_names.map(name => ([name, functionMap.get(name)[i]])))
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
        let i = 0
        for (let [name, _] of functionMap) {
            const line = colors[i % colors.length].line
            const dot = colors[i % colors.length].dot

            i += 1

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

                let target_pts = test["target"]
                rest_pts = [best_pt].concat(rest_pts)

                $svg.replaceWith(await this.plot(test, initial_pt, target_pts, rest_pts));
                $tbody.replaceWith(await this.tbody(test, initial_pt, target_pts, rest_pts));
                break;
            }
        }
    },

    plot: async function(benchmark, initial_pt, target_pts, rest_pts) {
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
                target_pts && Plot.dot(target_pts, {
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

    tbody: async function(benchmark, initial_pt, target_pts, rest_pts) {
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

            target_pts && target_pts.map((d, i) => {
                let accuracy = 100*(1 - d[1]/bits);
                let speedup = initial_pt[0]/d[0];
                return Element("tr", [
                    Element("th", 
                        Element("a", { href: "#target" + (i + 1)},
                            "Developer Target " + (i + 1))),
                    Element("td", { className: accuracy >= initial_accuracy ? "better" : "" },
                            accuracy.toFixed(1) + "%"),
                    Element("td", { className: speedup >= 1 ? "better" : "" },
                            speedup.toFixed(1) + "×")
            ])}),
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

function histogram2D(id, xdata, ydata, options) {
    var width = options?.width ?? 676;
    var height = options?.height ?? 60;
    var margin = 5;
    var labels = 10;
    var ticks = 5;
    var bucketnum = options?.buckets ?? 25;
    var bucketwidth = Math.round(width / bucketnum);

    var proportional = options?.proportional ?? true;

    var canvas = document.getElementById(id);
    if (xdata.length == 0 || xdata.length != ydata.length) { return canvas.remove(); } // Early exit

    canvas.setAttribute("width", margin + width + margin + "px");
    canvas.setAttribute("height", labels + margin + height + ticks + margin + labels + "px");
    var ctx = canvas.getContext("2d");
      
    ctx.beginPath();
    ctx.strokeStyle = "black";
    ctx.moveTo(margin, labels + margin + height);
    ctx.lineTo(margin + width, labels + margin + height);
    ctx.stroke();
    
    var xma = options?.max ?? Math.max.apply(null, xdata);
      
    var buckets = Array(bucketnum);
    var sum = 0;
    buckets.fill(0);
    for (var i = 0; i < xdata.length; i++) {
        var j = Math.floor(xdata[i] / xma * buckets.length);
        var x = proportional ? xdata[i] : 1;
        buckets[Math.min(j, buckets.length-1)] += ydata[i];
        sum += ydata[i];
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
