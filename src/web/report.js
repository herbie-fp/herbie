function toggle_flag_list() {
    var flags = document.getElementById("flag-list");
    flags.classList.toggle("changed-flags");
    var changed_only = flags.classList.contains("changed-flags");
    var button = document.getElementById("flag-list-toggle");
    button.innerText = changed_only ? "see all" : "see diff";
}

function togglable_flags() {
    var flags = document.getElementById("flag-list");
    flags.classList.add("changed-flags");
    var button = document.createElement("a");
    button.setAttribute("id", "flag-list-toggle");
    button.innerText = "see all";
    button.addEventListener("click", toggle_flag_list);
    flags.insertBefore(button, flags.children[0]);
}

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

function submit_inputs() {
    var originalOutputElem = document.querySelector('#try-original-output');
    var herbieOutputElem = document.querySelector('#try-herbie-output');
    var inputs = document.querySelectorAll('#try-inputs input');
    var inputVals = [];
    for (var i = 0; i < inputs.length; i++) {
        var val = parseFloat(inputs[i].value);
        if (isNaN(val)) {
            if (inputs[i].value.length != 0) {
                // Don't update error message if there is no input
                document.querySelector('#try-result').className = 'error'
            }
            return;
        } else {
            document.querySelector('#try-result').className = 'no-error'
            inputVals.push(val);
        }
    }
    originalOutputElem.innerHTML = start.apply(null, inputVals);
    herbieOutputElem.innerHTML = end.apply(null, inputVals);
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

function setup_timeline() {
    var ts = document.getElementsByClassName("timeline-phase");
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

function setup_program_arrow() {
    var progelt = document.getElementById("program");
    var progs = progelt.getElementsByClassName("program");
    var arrs = progelt.getElementsByClassName("arrow");

    progelt.classList.add("horizontal");
    var progBot = progs[0].offsetTop + progs[0].offsetHeight;
    for (var i in progs) {
        if (progs[i].offsetTop >= progBot) {
            return progelt.classList.remove("horizontal");
        }
    }
}
  
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

function load_graph() {
    var figs = document.querySelectorAll("#graphs figure");
    for (var i = 0; i < figs.length; i++) {
        setup_figure(figs[i]);
    }
    setup_figure_tabs(document.querySelector("#graphs div"));
    // Run the program_arrow after rendering happens
    var es = document.querySelectorAll('.math');
    for (var i = 0; i < es.length; i++) renderMathInElement(es[i]);
    // Submit the default vals in the "Try it out" section
    submit_inputs()
}

function report() { togglable_flags();}
function graph() { load_graph(); }
function index() { }
function timeline() { setup_timeline(); }
