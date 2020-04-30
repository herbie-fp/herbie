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

var FigureColors = new Component("#graphs figure", {
    setup: function() {
        this.caption = this.elt.querySelector("figcaption");
        var imgs = [].slice.call(this.elt.querySelectorAll("img"));
        var names = imgs.map(function(i) { return i.getAttribute("data-name"); });
        var buttons = names.filter(function(i) { return i; }).map(this.mkbutton);
        var caption_text = this.elt.querySelector("figcaption p");
        this.caption.insertBefore(Element("div", buttons), caption_text);
    },
    mkbutton: function(name) {
        var title = "Click to toggle " + name.toLowerCase() + " graph";
        var control = Element("button", { className: name, title: title}, name);
        control.addEventListener("click", this.toggler(control, name));
        return control;
    },
    toggler: function(button, name) {
        var figure = this.elt;
        var img = figure.querySelector("img[data-name=" + name + "]");
        return function() {
            if (button.classList.contains("inactive")) {
                button.classList.remove("inactive");
                img.style.display = "";
            } else {
                button.classList.add("inactive");
                img.style.display = "none";
            }
        }
    },
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

var FigureTabs = new Component("#graphs > div", {
    setup: function() {
        var figures = this.elt.getElementsByTagName("figure");
        var figure_array = {};
        var default_figure = null;
        for (var i = 0; i < figures.length; i++) {
            var idx = figures[i].id;
            var variable = figures[i].getElementsByTagName("var")[0].innerText;
            if (figures[i].classList.contains("default")) default_figure = figures[i];
            figure_array[idx] = { elt: figures[i], name: variable };
            figures[i].style.display = "none";
            figures[i].querySelector("figcaption > p").style.display = "none";
        }
        if (default_figure === null && figures.length > 0) default_figure = figures[0];
        
        var buttons = Object.keys(figure_array).map(function(idx) {
            return Element("li", { id: "tab-" + idx }, figure_array[idx].name);
        });

        var tab_bar = Element("ul", { className: "tabbar" }, [
            Element("p", "Bits error vs value of"),
            buttons,
        ]);
        this.elt.appendChild(tab_bar);

        for (var i = 0; i < buttons.length; i++) {
            buttons[i].addEventListener("click", this.toggle.bind(this, buttons[i].id));
        }
    
        if (default_figure) this.toggle("tab-" + default_figure.id);
    },
    toggle: function(tabid) {
        var id = tabid.substr(4);
        var tab = document.getElementById(tabid);
        var pane = document.getElementById(id);

        var old_tab = tab.parentNode.getElementsByClassName("selected");
        if (old_tab.length > 0) {
            var old_pane = document.getElementById(old_tab[0].id.substr(4));
            old_pane.style.display = "none";
            old_tab[0].classList.remove("selected")
        }

        tab.classList.add("selected");
        pane.style.display = "block";
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
        var total_time = 0;
        for (var i = 0; i < ts.length; i++) {
            total_time += +ts[i].getAttribute("data-timespan");
        }
        var total_width = ts[0].parentNode.offsetWidth;
        for (var i = 0; i < ts.length; i++) {
            ts[i].style.borderLeftWidth = (+ts[i].getAttribute("data-timespan")) / total_time * total_width + "px";
            var s = ts[i].getAttribute("data-type") + " (" + Math.round(+ts[i].getAttribute("data-timespan")/100)/10 + "s)";
            ts[i].title = s;
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
        fetch("profile.json")
            .then(response => response.json())
            .catch((error) => this.elt.remove())
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
        this.elt.appendChild(this.mkNode(json.nodes[1]));
        this.elt.classList.add("loaded");
    },
    mkNode: function(node) {
        var that = this;
        return Element("div", { className: "profile-row" }, [
            node.callers.sort((e1, e2) => e1.caller_time - e2.caller_time).map(function(edge) {
                var other = that.json.nodes[edge.caller];
                elt = Element("div", { className: "edge" }, [
                    Element("a", { className: "name" }, other.id || "???"),
                    Element("span", { className: "path" }, path(other.src)),
                    Element("span", { className: "pct" }, pct(edge.caller_time, node.total)),
                ]);
                elt.addEventListener("click", that.addElt(other));
                return elt;
            }),
            Element("div", { className: "node" }, [
                Element("span", { className: "name" }, node.id || "???"),
                Element("span", { className: "path" }, path(node.src)),
                Element("span", {
                    className: "pct",
                    title: "Self-time: " + pct(node.self, that.json.cpu_time) }, [
                        pct(node.total, that.json.total_time)
                    ]),
            ]),
            node.callees.sort((e1, e2) => e2.callee_time - e1.callee_time).map(function(edge) {
                var other = that.json.nodes[edge.callee];
                elt = Element("div", { className: "edge" }, [
                    Element("a", { className: "name" }, other.id || "???"),
                    Element("span", { className: "path" }, path(other.src)),
                    Element("span", { className: "pct" }, pct(edge.callee_time, node.total)),
                ]);
                elt.addEventListener("click", that.addElt(other));
                return elt;
            }),
        ]);
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

function histogram(id, data) {
    var width = 676;
    var height = 60
    var margin = 5;
    var labels = 10;
    var ticks = 5;
    var bucketwidth = 25;

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
    
    var xma = Math.max.apply(null, data);
      
    var buckets = Array(Math.round(width / bucketwidth));
    var sum = 0;
    buckets.fill(0);
    for (var i = 0; i < data.length; i++) {
        var j = Math.floor(data[i] / xma * buckets.length);
        buckets[Math.min(j, buckets.length-1)] += data[i];
        sum += data[i];
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
        ctx.fillText(Math.round(buckets[i] / sum * 100) + "%", margin + (i + .5)/buckets.length * width, labels + height*(1 - buckets[i]/yma));
    }
    
    ctx.textBaseline = "top";
    var step = Math.pow(10, Math.round(Math.log10(xma)) - 1);
    if (xma / step > 20) step *= 2;
    if (xma / step < 10) step /= 2;
    for (var i = 0; i < 10 * Math.sqrt(10); i++) {
        var pos = i * step;
        if (pos > yma) break;
        ctx.beginPath();
        ctx.moveTo(pos / xma * width + margin, labels + margin + height);
        ctx.lineTo(pos / xma * width + margin, labels + margin + height + ticks);
        ctx.fillText(pos, pos / xma * width + margin, labels + margin + height + ticks + margin);
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
