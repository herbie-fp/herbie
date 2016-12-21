function tr_click() {
    this.querySelector("td a").click();
}

function load_report() {
    var trs = document.querySelectorAll("tbody tr");
    for (var i = 0; i < trs.length; i++) {
        trs[i].addEventListener("click", tr_click);
    }
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
    for (var name in names) {
        var control = document.createElement("button");
        control.className = name;
        control.textContent = name;
        (function(name){
            control.addEventListener("click", function() {
                toggle_figure(figure, name);
            });
        })(name);
        figure.querySelector("figcaption").appendChild(control);
    }
}

function setup_timeline() {
    var ts = document.getElementsByClassName("timeline-phase");
    var total_time = 0;
    for (var i = 0; i < ts.length; i++) {
        total_time += +ts[i].getAttribute("data-timespan");
    }
    for (var i = 0; i < ts.length; i++) {
        ts[i].style.width = (+ts[i].getAttribute("data-timespan")) / total_time * 100 + "%";
        ts[i].title = ts[i].getAttribute("data-type");
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
    setup_timeline();
    // Run the program_arrow after rendering happens
    MathJax.Hub.Queue(setup_program_arrow);
}

function report() {load_report();}
function graph() {load_graph();}
function index() {load_report();}
