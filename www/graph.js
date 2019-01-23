margin = 10;
barheight = 10;
width = 505;
textbar = 20;

function sort_by(type) {
    return function(a, b) {
        return b[type] - a[type];
    }
}

function r10(d) {
    return "" + (Math.round(d * 10) / 10);
}

function make_graph(node, data, start, end) {
    var len = data.length;
    var precision = 64; // TODO

    var a = d3.selectAll("script");
    var script = a[0][a[0].length - 1];

    var svg = node
        .append("g").attr("transform", "translate(" + margin + "," + margin + ")");

    for (var i = 0; i <= precision; i += 4) {
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
    function title(d, i) { return d.name + " (" + r10(precision - d[start]) + " to " + r10(precision - d[end]) + ")"; }

    bar.append("line")
        .attr("class", "guide")
        .attr("x1", 0)
        .attr("x2", function(d) { return (precision - Math.max(d[start], d[end])) / precision * width })
        .attr("y1", line_y)
        .attr("y2", line_y);

    var g = bar.append("g").attr("title", title)
        .attr("class", function(d) { return d[start] > d[end] ? "good" : "bad" });

    g.append("line")
        .attr("x1", function(d) {return (precision - Math.max(d[start], d[end])) / precision * width})
        .attr("x2", function(d) { return (precision - Math.min(d[start], d[end])) / precision * width })
        .attr("y1", line_y)
        .attr("y2", line_y);

    g.append("g").attr("transform", function(d, i) {
        return "translate(" + ((precision - Math.min(d[start], d[end])) / precision * width) + ", " + line_y(d, i) + ")";
    })
        .append("polygon").attr("points", "0,-3,0,3,5,0");
}

function draw_results(node) {
    d3.json("results.json", function(err, data) {
        if (err) return console.error(err);
        data = data.tests;
    
        data.sort(sort_by("start"));
        make_graph(node, data, "start", "end");
    });
}
