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

    var svg = node.append("svg")
        .attr("width", width + 2 * margin)
        .attr("height", len * barheight + 2 * margin + textbar)
        .append("g").attr("transform", "translate(" + margin + "," + margin + ")");

    for (var i = 0; i <= precision; i += 4) {
        svg.append("rect")
            .attr("class", "gridline")
            .attr("x", i / precision * width)
            .attr("height", len * barheight)
            .attr("width", 1);

        svg.append("text").text(i)
            .attr("x", i / precision * width)
            .attr("width", 80)
            .attr("y", len * barheight + textbar);
    }

    var bar = svg
        .selectAll("g").data(data).enter();

    bar.append("rect")
        .attr("class", "guide")
        .attr("width", function(d) { return (precision - Math.max(d[start], d[end])) / precision * width })
        .attr("height", 1)
        .attr("y", function(d, i) { return i * barheight + 0.5; })
        .attr("title", function(d) { return d.name + " (" + r10(precision - d[start]) + " to " + r10(precision - d[end]) + ")"; });

    bar.append("rect")
        .attr("class", function(d) { return d[start] > d[end] ? "good" : "bad" })
        .attr("width", function(d) { return Math.abs(d[start] - d[end]) / precision * width })
        .attr("height", 2)
        .attr("x", function(d) {return (precision - Math.max(d[start], d[end])) / precision * width})
        .attr("y", function(d, i) { return i * barheight; })
        .attr("title", function(d) { return d.name + " (" + r10(precision - d[start]) + " to " + r10(precision - d[end]) + ")"; });

    bar.append("g").attr("transform", function(d, i) {
        return "translate(" + ((precision - Math.min(d[start], d[end])) / precision * width) + ", " + (i * barheight + 1) + ")";
    })
        .append("polygon").attr("points", "0,-5,0,5,5,0")
        .attr("class", function(d) { return d[start] > d[end] ? "good" : "bad" });
}

function draw_results(node) {
    d3.json("results.json", function(err, data) {
        if (err) return console.error(err);
        data = data.tests;
    
        data.sort(sort_by("start"));
        make_graph(node, data, "start", "end");
    });
}
