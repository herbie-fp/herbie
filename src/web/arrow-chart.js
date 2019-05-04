function sort_by(type) {
    return function(a, b) {
        return b[type] - a[type];
    }
}

function r10(d) {
    return "" + (Math.round(d * 10) / 10);
}

function make_graph(node, data, start, end) {
    var margin = 10;
    var barheight = 10;
    var width = 450;
    var textbar = 20;
    var precision = 64;
    var precision_step = 8;

    var len = data.length;

    var svg = d3.select(node)
        .attr("width", width + 2 * margin)
        .attr("height", len * barheight + 2 * margin + textbar)
        .append("g").attr("transform", "translate(" + margin + "," + margin + ")");

    for (var i = 0; i <= precision; i += precision_step) {
        svg.append("line")
            .attr("class", "gridline")
            .attr("x1", i / precision * width)
            .attr("x2", i / precision * width)
            .attr("y1", 0)
            .attr("y2", len * barheight);

        svg.append("text").text(i)
            .attr("x", i / precision * width)
            .attr("width", 80)
            .attr("y", len * barheight + textbar);
    }

    var bar = svg.selectAll("g").data(data).enter();

    function line_y(d, i) { return (i + .5) * barheight; }
    function title(d, i) { return d.name + " (" + r10(d[start]) + " to " + r10(d[end]) + ")"; }

    bar.append("line")
        .attr("class", "guide")
        .attr("x1", 0)
        .attr("x2", function(d) { return (precision - Math.max(d[start], d[end])) / precision * width })
        .attr("y1", line_y)
        .attr("y2", line_y);

    var g = bar.append("g").attr("title", title).attr("data-id", function(d){ return d.id })
        .attr("class", function(d) {
            return d[start] > d[end] + 1 ? "arrow good" : d[start] < d[end] - 1 ? "arrow bad" : "arrow nodiff" });

    g.append("a")
        .attr("xlink:href", function(d) { return "./" + d.link + "/graph.html" })
        .append("line")
        .attr("x1", function(d) {return (precision - Math.max(d[start], d[end])) / precision * width})
        .attr("x2", function(d) { return (precision - Math.min(d[start], d[end])) / precision * width })
        .attr("y1", line_y)
        .attr("y2", line_y);

    g.append("g").attr("transform", function(d, i) {
        return "translate(" + ((precision - d[end]) / precision * width) + ", " + line_y(d, i) + ")";
    })
        .append("polygon").attr("points", function(d) {
            if (d[start] > d[end] + 1) {
                return "-5,-3,-5,3,0,0";
            } else if (d[start] < d[end] - 1) {
                return "5,-3,5,3,0,0";
            } else {
                return "0,-3,3,0,0,3,-3,0";
            }
        });
}

function draw_results(node) {
    d3.json("results.json", function(err, data) {
        if (err) return console.error(err);
        precision = data.bit_width;
        precision_step = Math.round(precision / 8);

        data = data.tests;
        for (var i = 0; i < data.length; i++) {
            data[i].id = i
        }
    
        data.sort(sort_by("start"));
        make_graph(node, data, "start", "end");


        var length = data.length;
        var BADGES = new Array(length);
        var ARROWS = new Array(length);

        BADGES.container = document.querySelector("#test-badges");
        ARROWS.container = document.querySelector("#graph");

        var arrows = document.querySelectorAll(".arrow");
        for (var i = 0; i < arrows.length; i++) {
            var idx = +arrows[i].attributes["data-id"].value;
            ARROWS[idx] = arrows[i]
        }

        var badges = document.querySelectorAll(".badge");
        for (var i = 0; i < badges.length; i++) {
            var idx = +badges[i].attributes["data-id"].value;
            var url = document.getElementById("link" + idx).href;
            var a = document.createElement("a");
            // Only a text thing
            var t = badges[i].childNodes[0];
            a.href = url;
            a.appendChild(t);
            badges[i].appendChild(a);
            BADGES[idx] = badges[i];
        }

        function clear() {
            var ho = document.querySelector(".highlight-one");
            if (ho) ho.classList.remove("highlight-one");
            var h = document.querySelector(".highlight");
            if (h) h.classList.remove("highlight");
        }

        for (var i = 0; i < length; i++) {
            (function (i) {
                BADGES[i].addEventListener("mouseover", function() {
                    clear();
                    ARROWS[i].classList.add("highlight");
                    ARROWS.container.classList.add("highlight-one");
                });
                BADGES[i].addEventListener("mouseout", clear);

                ARROWS[i].addEventListener("mouseover", function() {
                    clear();
                    BADGES[i].classList.add("highlight");
                    BADGES.container.classList.add("highlight-one");
                });
                ARROWS[i].addEventListener("mouseout", clear);
            })(i);
        }
    });
}

var ArrowChart = new Component("svg.arrow-chart", {
    setup: function() { draw_results(this.elt); }
});
