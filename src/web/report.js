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
    for (var i = 0; i < ts.length; i++) {
        ts[i].style.width = (+ts[i].getAttribute("data-timespan")) / total_time * 100 + "%";
        var s = ts[i].getAttribute("data-type") + " (" + Math.round(+ts[i].getAttribute("data-timespan")/100)/10 + "s)";
        for (var j in ts[i].dataset) {
            if (j == "type" || j == "timespan") continue;
            s += "\n" + j + ": " + ts[i].dataset[j];
        }
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

function load_graph() {
    var figs = document.querySelectorAll("#graphs figure");
    for (var i = 0; i < figs.length; i++) {
        setup_figure(figs[i]);
    }
    setup_figure_tabs(document.querySelector("#graphs div"));
    setup_timeline();
    // Run the program_arrow after rendering happens
    var es = document.querySelectorAll('.math');
    for (var i = 0; i < es.length; i++) renderMathInElement(es[i]);
    // Submit the default vals in the "Try it out" section
    submit_inputs()
}

function load_report() {
    togglable_flags();
}

function load_index() {
    // Nothing
}

function report() {load_report();}
function graph() {load_graph();}
function index() {load_index();}
