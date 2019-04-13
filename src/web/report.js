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

var TogglableFlags = new Component("#flag-list", {
    setup: function() {
        this.elt.classList.add("changed-flags");
        this.button = document.createElement("a");
        this.button.setAttribute("id", "flag-list-toggle");
        this.button.innerText = "see all";
        this.button.addEventListener("click", this.toggle);
        this.elt.insertBefore(this.button, this.elt.children[0]);
    },
    toggle: function() {
        this.elt.classList.toggle("changed-flags");
        var changed_only = this.elt.classList.contains("changed-flags");
        this.button.innerText = changed_only ? "see all" : "see diff";
    }
});

var Figure = new Component("#graphs figure", {
    setup: function() {
        setup_figure(this.elt);
    },
});

function figure_names(figure) {
    var imgs = figure.querySelectorAll("img");
    var names = {};
    for (var i = 0; i < imgs.length; i++) {
        if (!imgs[i].getAttribute("data-name")) continue;
        names[imgs[i].getAttribute("data-name")] = imgs[i];
    }
    return names;
}

function toggle_figure(figure, name) {
    var img = figure.querySelector("img[data-name=" + name + "]");
    var button = figure.querySelector("button." + name);
    if (button.classList.contains("inactive")) {
        button.classList.remove("inactive");
        img.style.display = "";
    } else {
        button.classList.add("inactive");
        img.style.display = "none";
    }
}

function setup_figure(figure) {
    var names = figure_names(figure);
    var caption_text = figure.querySelector("figcaption p");
    for (var name in names) {
        var control = document.createElement("button");
        control.className = name;
        control.textContent = name;
        control.title = ("Click to toggle " + name.toLowerCase() + " graph");
        (function(name){
            control.addEventListener("click", function() {
                toggle_figure(figure, name);
            });
        })(name);
        figure.querySelector("figcaption").insertBefore(control, caption_text);
    }
}

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
        setup_figure_tabs(this.elt);
    },
});

function select_tab(id) {
    var tab = document.getElementById("tab-" + id);
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

function setup_figure_tabs(figure_container) {
    var figures = figure_container.getElementsByTagName("figure");
    var figure_array = {};
    var default_figure = null;
    for (var i = 0; i < figures.length; i++) {
        var idx = figures[i].id;
        var variable = figures[i].getElementsByTagName("var")[0].innerText;
        if (figures[i].classList.contains("default")) default_figure = idx;
        figure_array[idx] = { elt: figures[i], name: variable };
        figures[i].style.display = "none";
        figures[i].querySelector("figcaption > p").style.display = "none";
    }
    if (default_figure === null && figures.length > 0) default_figure = figures[0].id;

    var tab_bar = document.createElement("ul");
    tab_bar.classList.add("tabbar");
    var p = document.createElement("p");
    p.innerText = "Bits error vs value of"
    tab_bar.appendChild(p)
    figure_container.appendChild(tab_bar);
    for (var idx in figure_array) {
        var tab_button = document.createElement("li");
        tab_button.id = "tab-" + idx;
        tab_button.innerText = figure_array[idx].name;
        tab_button.addEventListener("click", function() {
            select_tab(this.id.substr(4));
        });
        tab_bar.appendChild(tab_button);
    }

    if (default_figure) select_tab(default_figure);
}

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

var ProgramText = new Component("#program", {
    setup: function() {
        this.dropdown = this.elt.querySelector("#language");
        this.programs = this.elt.querySelectorAll("#program > div");
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

function histogram(id, data) {
    var width = 676;
    var height = 60
    var margin = 5;
    var labels = 10;
    var ticks = 5;
    var bucketwidth = 25;

    var canvas = document.getElementById(id);
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
