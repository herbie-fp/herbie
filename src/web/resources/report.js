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
        // get D3
        const d3 = await import('https://cdn.skypack.dev/d3@6')
        const Plot = await import("https://cdn.skypack.dev/@observablehq/plot@0.4")
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
        
        // const addTooltips = (() => {
        //     /* addTooltips code adapted from https://observablehq.com/@mkfreeman/plot-tooltip */
        //     const hover = (tip, pos, text) => {
        //         const side_padding = 10;
        //         const vertical_padding = 5;
        //         const vertical_offset = 15;

        //         // Empty it out
        //         tip.selectAll("*").remove();

        //         // Append the text
        //         tip
        //         .style("text-anchor", "middle")
        //         .style("pointer-events", "none")
        //         .attr("transform", `translate(${pos[0]}, ${pos[1] + 7})`)
        //         .selectAll("text")
        //         .data(text)
        //         .join("text")
        //         .style("dominant-baseline", "ideographic")
        //         .text((d) => d)
        //         .attr("y", (d, i) => (i - (text.length - 1)) * 15 - vertical_offset)
        //         .style("font-weight", (d, i) => (i === 0 ? "bold" : "normal"));

        //         const bbox = tip.node().getBBox();

        //         // Add a rectangle (as background)
        //         tip
        //         .append("rect")
        //         .attr("y", bbox.y - vertical_padding)
        //         .attr("x", bbox.x - side_padding)
        //         .attr("width", bbox.width + side_padding * 2)
        //         .attr("height", bbox.height + vertical_padding * 2)
        //         .style("fill", "white")
        //         .style("stroke", "#d3d3d3")
        //         .lower();
        //     }
        //     // To generate a unique ID for each chart so that they styles only apply to that chart
        //     const id_generator = () => {
        //         var S4 = function () {
        //             return (((1 + Math.random()) * 0x10000) | 0).toString(16).substring(1);
        //         };
        //         return "a" + S4() + S4();
        //         }
        //     const addTooltips = (chart, hover_styles = { fill: "blue", opacity: 0.5 }) => {
        //         let styles = hover_styles;
        //         const line_styles = {
        //         stroke: "blue",
        //         "stroke-width": 3
        //         };
        //         // Workaround if it's in a figure
        //         const type = d3.select(chart).node().tagName;
        //         let wrapper =
        //         type === "FIGURE" ? d3.select(chart).select("svg") : d3.select(chart);

        //         // Workaround if there's a legend....
        //         const numSvgs = d3.select(chart).selectAll("svg").size();
        //         if (numSvgs === 2)
        //         wrapper = d3
        //             .select(chart)
        //             .selectAll("svg")
        //             .filter((d, i) => i === 1);
        //         wrapper.style("overflow", "visible"); // to avoid clipping at the edges

        //         // Set pointer events to visibleStroke if the fill is none (e.g., if its a line)
        //         wrapper.selectAll("path").each(function (data, index, nodes) {
        //         // For line charts, set the pointer events to be visible stroke
        //         if (
        //             d3.select(this).attr("fill") === null ||
        //             d3.select(this).attr("fill") === "none"
        //         ) {
        //             d3.select(this).style("pointer-events", "visibleStroke");
        //             styles = hover_styles.fill == 'blue' && hover_styles.opacity == .5 //_.isEqual(hover_styles, { fill: "blue", opacity: 0.5 })
        //             ? line_styles
        //             : hover_styles;
        //         }
        //         });

        //         const tip = wrapper
        //         .selectAll(".hover-tip")
        //         .data([""])
        //         .join("g")
        //         .attr("class", "hover")
        //         .style("pointer-events", "none")
        //         .style("text-anchor", "middle");

        //         // Add a unique id to the chart for styling
        //         const id = id_generator();

        //         // Add the event listeners
        //         d3.select(chart)
        //         .classed(id, true) // using a class selector so that it doesn't overwrite the ID
        //         .selectAll("title")
        //         .each(function () {
        //             // Get the text out of the title, set it as an attribute on the parent, and remove it
        //             const title = d3.select(this); // title element that we want to remove
        //             const parent = d3.select(this.parentNode); // visual mark on the screen
        //             const t = title.text();
        //             if (t) {
        //             parent.attr("__title", t).classed("has-title", true);
        //             title.remove();
        //             }
        //             // Mouse events
        //             parent
        //             .on("mousemove", function (event) {
        //                 const text = d3.select(this).attr("__title");
        //                 const pointer = d3.pointer(event, wrapper.node());
        //                 if (text) tip.call(hover, pointer, text.split("\n"));
        //                 else tip.selectAll("*").remove();

        //                 // Raise it
        //                 d3.select(this).raise();
        //                 // Keep within the parent horizontally
        //                 const tipSize = tip.node().getBBox();
        //                 if (pointer[0] + tipSize.x < 0)
        //                 tip.attr(
        //                     "transform",
        //                     `translate(${tipSize.width / 2}, ${pointer[1] + 7})`
        //                 );
        //                 else if (pointer[0] + tipSize.width / 2 > wrapper.attr("width"))
        //                 tip.attr(
        //                     "transform",
        //                     `translate(${wrapper.attr("width") - tipSize.width / 2}, ${
        //                     pointer[1] + 7
        //                     })`
        //                 );
        //             })
        //             .on("mouseout", function (event) {
        //                 tip.selectAll("*").remove();
        //                 // Lower it!
        //                 d3.select(this).lower();
        //             });
        //         });

        //         // Remove the tip if you tap on the wrapper (for mobile)
        //         wrapper.on("touchstart", () => tip.selectAll("*").remove());
        //         // Add styles
        //         const style_string = Object.keys(styles)
        //         .map((d) => {
        //             return `${d}:${styles[d]};`;
        //         })
        //             .join("");
                
        //         function html(string) {
        //             const t = document.createElement('template');
        //             t.innerHTML = string;
        //             return t.content;
        //         }

        //         // Define the styles
        //         const style = html(`<style>
        //             .${id} .has-title {
        //             cursor: pointer; 
        //             pointer-events: all;
        //             }
        //             .${id} .has-title:hover {
        //             ${style_string}
        //         }
        //         </style>`);
        //         chart.appendChild(style);
        //         return chart;
        //     }
        //     return addTooltips
        // })()
        const plot = async (varName, function_names) => {
            const functions = [
                { name: 'start', line: { stroke: '#aa3333ff' }, area: { fill: "#c001"}, dot: { stroke: '#ff000007'} },
                { name: 'end', line: { stroke: '#0000ffff' }, area: { fill: "#00c1"}, dot: { stroke: '#0000ff07'} },
                { name: 'target', line: { stroke: 'green' }, dot: { stroke: '#00ff0005'}}  // TODO add target support
            ].filter(o => function_names.includes(o.name))
            const index = all_vars.indexOf(varName)
            // NOTE ticks and splitpoints include all vars, so we must index
            const { bits, points, error, ticks_by_varidx, splitpoints_by_varidx } = points_json
            const ticks = ticks_by_varidx[index]
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
                const sliding_window = (A, size) => [...new Array(Math.max(A.length - size, 0))].map((_, i) => {
                    const half = Math.floor(size / 2)
                    i = i + half
                    const slice = A.slice(i - half, i - half + size).sort(key_fn(o => o.y))
                    const x = A[i].x
                    const top = slice[Math.floor(slice.length * .95)].y
                    const top_q = slice[Math.floor(slice.length * .75)].y
                    const bottom = slice[Math.floor(slice.length * .05)].y
                    const bottom_q = slice[Math.floor(slice.length * .25)].y
                    const middle = slice[Math.floor(slice.length * .5)].y
                    const average = slice.reduce((acc, e) => e.y + acc, 0) / slice.length
                    return { x, top, middle, bottom, average, top_q, bottom_q }
                })
                const bin_size = 128
                const sliding_window_data = sliding_window(data, bin_size)
                return [
                    // Plot.areaY(sliding_window_data, {
                    //     x: d => d.x, 
                    //     y1: "bottom", 
                    //     y2: "top",
                    //     ...area
                    // }),
                    // Plot.areaY(sliding_window_data, {
                    //     x: d => d.x, 
                    //     y1: "bottom_q", 
                    //     y2: "top_q",
                    //     ...area
                    // }),
                    Plot.line(sliding_window_data, {
                        x: "x",
                        y: "average",
                        strokeWidth: 1.3, ...line,
                    }),
                    // Plot.line(sliding_window_data, {
                    //     x: "x",
                    //     y: "average",
                    //     strokeWidth: 1.3, ...line,
                    // }),
                    // Plot.line(chunk(sliding_window_data.map(({ top }) => top), sliding_chunksize).map(average_chunk), {
                    //     x: d => d.x, y: "err", strokeWidth: 1.3, ...line, title: d => 'test'
                    // }),
                    // Plot.line(chunk(sliding_window_data.map(({ bottom }) => bottom), sliding_chunksize).map(average_chunk), {
                    //     x: d => d.x, y: "err", strokeWidth: 1.3, ...line, title: d => 'test'
                    // }),
                    // Plot.line(chunk(sliding_window_data.map(({ middle }) => middle), sliding_chunksize).map(average_chunk), {
                    //     x: d => d.x, y: "err", strokeWidth: 1.3, ...line, title: d => 'test'
                    // }),
                    Plot.dot(data, {x: "x", y: "y", r: 1.3,
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
            document.querySelector('#graphs').replaceChildren(await plot(selected_var_name, selected_functions), options_view)
        }
        render(all_vars[0], ['start', 'end'])
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
                    pct(node.total, that.json.total_time)
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
