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

const ClientGraph = new Component('#graphs', {
    setup: async () => {
        const points_json = await (async () => {
            const get_points_store = {}
            
            const get_points_memo = async () => {
                if (get_points_store.value) { return get_points_store.value }
                const ps = await get_json('points.json')
                get_points_store.value = ps
                return get_points_store.value
            }
            const get_json = url => fetch(url, {
                // body: `_body_`,
                headers: {"content-type": "text/plain"},
                method: "GET",
                mode: 'cors'
                }).then(async response => {
                //await new Promise(r => setTimeout(() => r(), 200) )  // model network delay
                return await response.json()
            })
            return get_points_memo()
        })()
        
        const plot = async (varName, function_names) => {
            const functions = [
                { name: 'start', line: { stroke: '#aa3333ff' }, area: { fill: "#c001"}, dot: { stroke: '#ff000035'} },
                { name: 'end', line: { stroke: '#0000ffff' }, area: { fill: "#00c1"}, dot: { stroke: '#0000ff35'} },
                { name: 'target', line: { stroke: 'green' }, dot: { stroke: '#00ff0035'}}
            ].filter(o => function_names.includes(o.name))
            const index = all_vars.indexOf(varName)
            // NOTE ticks and splitpoints include all vars, so we must index
            const { bits, points, error, ticks_by_varidx, splitpoints_by_varidx } = points_json
            const ticks = ticks_by_varidx[index]
            if (!ticks) {
                return html(`<div>The function could not be plotted on the given range for this input.</div>`)
            }
            const tick_strings = ticks.map(t => t[0])
            const tick_ordinals = ticks.map(t => t[1])
            const tick_0_index = tick_strings.indexOf("0")
            const splitpoints = splitpoints_by_varidx[index]
            const grouped_data = points.map((p, i) => ({
                input: p,
                error: Object.fromEntries(function_names.map(name => ([name, error[name][i]])))
            }))
            const domain = [Math.min(...tick_ordinals), Math.max(...tick_ordinals)]

            async function extra_axes_and_ticks() {
                return [
                    ...splitpoints.map(p => Plot.ruleX([p], { stroke: "lightgray", strokeWidth: 4 })),
                    ...(tick_0_index > -1 ? [Plot.ruleX([tick_ordinals[tick_0_index]])] : []),
                ]
            }


            async function line_and_dot_graphs({ name, fn, line, dot, area }) {
                const key_fn = fn => (a, b) => fn(a) - fn(b)
                const index = all_vars.indexOf(varName)
                const data = grouped_data.map(({ input, error }) => ({
                        x: input[index],
                        y: error[name]
                })).sort(key_fn(d => d.x))
                    .map(({ x, y }, i) => ({ x, y, i }))
                // const sliding_window = (A, size) => [...new Array(Math.max(A.length - size, 0))].map((_, i) => {
                //     const half = Math.floor(size / 2)
                //     i = i + half
                //     const slice = A.slice(i - half, i - half + size).sort(key_fn(o => o.y))
                //     const x = A[i].x
                //     const top = slice[Math.floor(slice.length * .95)].y
                //     const top_q = slice[Math.floor(slice.length * .75)].y
                //     const bottom = slice[Math.floor(slice.length * .05)].y
                //     const bottom_q = slice[Math.floor(slice.length * .25)].y
                //     const middle = slice[Math.floor(slice.length * .5)].y
                //     const average = slice.reduce((acc, e) => e.y + acc, 0) / slice.length
                //     return { x, top, middle, bottom, average, top_q, bottom_q }
                // })
                const sliding_window = (A, size) => {
                    const half = Math.floor(size / 2)
                    const running_sum = A.reduce((acc, v) => (acc.length > 0 ? acc.push(v.y + acc[acc.length - 1]) : acc.push(v.y), acc), [])
                    const xs = 
                    console.log('running', running_sum)
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
                }
                const compress = (L, out_len, chunk_compressor = points => points[0]) => L.reduce((acc, pt, i) => i % Math.floor(L.length / out_len) == 0 ? (acc.push(chunk_compressor(L.slice(i, i + Math.floor(L.length / out_len)))), acc) : acc, [])
                const bin_size = 128
                const sliding_window_data = compress(
                    sliding_window(data, bin_size), 800, points => ({
                        average: points.reduce((acc, e) => e.average + acc, 0) / points.length,
                        x: points.reduce((acc, e) => e.x + acc, 0) / points.length
                    }))
                return [
                    Plot.line(sliding_window_data, {
                        x: "x",
                        y: "average",
                        strokeWidth: 2, ...line,
                    }),
                    Plot.dot(compress(data, 800), {x: "x", y: "y", r: 1.3,
                        title: d => `x: ${d.x} \n i: ${d.i} \n bits of error: ${d.y}`,
                        ...dot
                    }),
                ]
            }
            const out = Plot.plot({
                width: '800',
                height: '400',                
                    x: {
                        tickFormat: d => tick_strings[tick_ordinals.indexOf(d)],
                        ticks: tick_ordinals, label: `value of ${varName}`,
                        labelAnchor: 'center', /*labelOffset: [200, 20], tickRotate: 70, */
                        domain,
                        grid: true
                    },
                    y: {
                        label: "Bits of error", domain: [0, bits],
                        ticks: new Array(bits / 4 + 1).fill(0).map((_, i) => i * 4),
                        tickFormat: d => d % 8 != 0 ? '' : d
                    },
                    marks: await Promise.all([...await extra_axes_and_ticks(),
                        ...functions.map(async config =>
                                        await line_and_dot_graphs(config)).flat()])
            })
            out.setAttribute('viewBox', '0 0 800 430')
            return out
        }
        function html(string) {
            const t = document.createElement('template');
            t.innerHTML = string;
            return t.content;
        }
        const all_vars = points_json.vars
        async function render(selected_var_name, selected_functions) {
            const all_fns = ['start', 'end', 'target'].filter(name => points_json.error[name] != false)
            const options_view = html(`
                <div id="plot_options">
                <div id="variables">
                    Bits of error vs. ${all_vars.map(v => `<span class="variable ${selected_var_name == v ? 'selected' : ''}">${v}</span>`).join('')}
                </div>
                <div id="functions">
                    ${all_fns.map(fn => `<div id="function_${fn}" class="function ${selected_functions.includes(fn) ? 'selected' : ''}"></div>`).join('')}
                </div>
                </div>
            `)
            const toggle = (option, options) => options.includes(option) ? options.filter(o => o != option) : [...options, option]
            options_view.querySelectorAll('.variable').forEach(e => e.onclick = () => {
                render(e.textContent, selected_functions)
            })
            options_view.querySelectorAll('.function').forEach(e => e.onclick = () => {
                render(selected_var_name, toggle(e.id.split('_').slice(1).join('_'), selected_functions))
            })
            document.querySelector('#graphs-content').replaceChildren(await plot(selected_var_name, selected_functions), options_view)
        }
        render(all_vars[0], ['start', 'end'])
    }
})

const ResultPlot = new Component('#xy', {
    setup: async function() {
        console.log(this);
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
        this.elt.replaceChild(this.plot(json["cost-accuracy"], json.tests.length), stub)
    },

    plot: function(json, n) {
        const sortLine = [...json[2]];
        sortLine.sort((a, b) => {return a[0]-b[0]});
        const ymax = 64 * n;
        const out = Plot.plot({
            marks: [
                Plot.line(sortLine, {x: d => d[0], y: d => 1 - d[1] / ymax,
                                     stroke: "#00a", strokeWidth: 2}),
                Plot.dot(json[1], {x: json[1][0], y: json[1][1] / ymax,
                                   stroke: "#d00", symbol: "square", strokeWidth: 2})
            ],
            width: '400',
            height: '400',                
            x: { line: true, },
            y: { line: true, nice: true, domain: [0, 1], tickFormat: "%", },
            marginBottom: 0,
            marginRight: 0,
        })
        out.setAttribute('viewBox', '0 0 420 420')
        return out;
    }
})

const CostAccuracy = new Component('#cost-accuracy', {
    setup: async () => {
        const content = document.querySelector('#pareto-content');
        
        const results_json = await (async () => {
            const get_results_store = {}
            
            const get_results_memo = async () => {
                if (get_results_store.value) { return get_results_store.value }
                const ps = await get_json('../results.json');
                get_results_store.value = ps;
                return get_results_store.value;
            }
            const get_json = url => fetch(url, {
                // body: `_body_`,
                headers: {"content-type": "text/plain"},
                method: "GET",
                mode: 'cors'
                }).then(async response => {
                //await new Promise(r => setTimeout(() => r(), 200) )  // model network delay
                return await response.json()
            })
            return get_results_memo()
        })()

        const plot = async () => {
            // NOTE ticks and splitpoints include all vars, so we must index
            const tests = results_json.tests;
            
            let benchmark;

            // find right test by iterating through results_json
            for (let test of tests) {
                console.log(test);
                if (test.name == content.dataset.benchmarkName) {
                    benchmark = test;
                    break;
                }
            }

            console.log(benchmark);

            const costAccuracy = benchmark["cost-accuracy"];
            
            // find maximum x and y values
            let xmax = costAccuracy[0][0];
            let ymax = costAccuracy[0][1];

            if (costAccuracy[1][0] > xmax) xmax = costAccuracy[1][0];
            if (costAccuracy[1][1] > ymax) ymax = costAccuracy[1][1];
            
            for (let point of costAccuracy[2]) {
                if (point[0] > xmax) xmax = point[0];
                if (point[1] > ymax) ymax = point[1];
            }

            // ceiling ymax to nearest multiple of 16
            const range = Math.ceil(ymax/16) * 16;

            // composite array for both best and other points so the line graph
            // contains the best point as well.
            const allpoints = [costAccuracy[1], ...costAccuracy[2]];
            allpoints.sort((a, b) => {return a[0]-b[0]});
            console.log(allpoints);

            const out = Plot.plot({
                marks: [
                    Plot.dot(costAccuracy[0], {x: costAccuracy[0][0], y: costAccuracy[0][1], fill: "black"}),
                    Plot.dot(costAccuracy[1], {x: costAccuracy[1][0], y: costAccuracy[1][1], fill: "red", stroke: "blue", title: d => `x: ${costAccuracy[1][0]} y: ${costAccuracy[1][1]} best`}),
                    Plot.dot(costAccuracy[2], {x: d => d[0], y: d => d[1], fill: "red", stroke: "black", title: d => `x: ${d[0]} y: ${d[1]} exp: ${d[2]}`}),
                    Plot.line(allpoints, {x: d => d[0], y: d => d[1], stroke: "red"})
                ],
                grid: true,
                width: '800',
                height: '400',                
                    x: {
                        label: `Cost`,
                        domain: [0, 2 * xmax]
                    },
                    y: {
                        label: "Bits of error",
                        domain: [0, range],
                        ticks: new Array(range / 4 + 1).fill(0).map((_, i) => i * 4),
                        tickFormat: d => d % 8 != 0 ? '' : d
                    },
            })
            out.setAttribute('viewBox', '0 0 800 430')
            return out
        }
        function html(string) {
            const t = document.createElement('template');
            t.innerHTML = string;
            return t.content;
        }
        async function render() {
            const options_view = html(`
                <div id="plot_options">
                </div>
            `)
            const toggle = (option, options) => options.includes(option) ? options.filter(o => o != option) : [...options, option]

            content.replaceChildren(await plot(), options_view)
        }
        render()
    }
})
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

var Implementations = new Component("#program", {
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
                this.arrow($prog);
            } else {
                $prog.style.display =  "none";
            }
        }
    },
    arrow: function($prog) {
        var progs = $prog.querySelectorAll(".program");
        $prog.classList.add("horizontal");
        for (var i = 0; i < progs.length; i++) {
            var progBot = progs[i].offsetTop + progs[i].offsetHeight;
            if (progs[i].offsetTop >= progBot) {
                return $prog.classList.remove("horizontal");
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
