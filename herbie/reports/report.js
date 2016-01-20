function tr_click() {
    this.querySelector("td a").click();
}

function load_report() {
    var trs = document.querySelectorAll("tbody tr");
    for (var i = 0; i < trs.length; i++) {
        trs[i].addEventListener("click", tr_click);
    }
}

window.addEventListener("load", load_report);

function figure_names(figure) {
    var imgs = figure.querySelectorAll("img");
    var names = {};
    for (var i = 0; i < imgs.length; i++) {
        if (!imgs[i].getAttribute("data-name")) continue;
        names[imgs[i].getAttribute("data-name")] = imgs[i];
    }
    return names
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

function load_graph() {
    var figs = document.querySelectorAll("#graphs figure");
    for (var i = 0; i < figs.length; i++) {
        setup_figure(figs[i]);
    }
}

window.addEventListener("load", load_graph);

