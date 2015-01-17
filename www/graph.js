margin = 10;
barheight = 10;
width = 505;
textbar = 20;

function sort_by(type) {
    return function(a, b) {
        return b[type][0] - a[type][0];
    }
}

function r10(d) {
    return "" + (Math.round(d * 10) / 10);
}

function make_graph(node, data, type) {
    var len = data.length;
    var precision = (type.substring(0,"single".length) == "single") ? 32 : 64;

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
        .attr("width", function(d) { return (precision - Math.max(d[type][0], d[type][1])) / precision * width })
        .attr("height", 1)
        .attr("y", function(d, i) { return i * barheight + 0.5; })
        .attr("title", function(d) { return d.name + " (" + r10(precision - d[type][0]) + " to " + r10(precision - d[type][1]) + ")"; });

    bar.append("rect")
        .attr("class", function(d) { return d[type][0] > d[type][1] ? "good" : "bad" })
        .attr("width", function(d) { return Math.abs(d[type][0] - d[type][1]) / precision * width })
        .attr("height", 2)
        .attr("x", function(d) {return (precision - Math.max(d[type][0], d[type][1])) / precision * width})
        .attr("y", function(d, i) { return i * barheight; })
        .attr("title", function(d) { return d.name + " (" + r10(precision - d[type][0]) + " to " + r10(precision - d[type][1]) + ")"; });

    bar.append("g").attr("transform", function(d, i) {
        return "translate(" + ((precision - Math.min(d[type][0], d[type][1])) / precision * width) + ", " + (i * barheight + 1) + ")";
    })
        .append("polygon").attr("points", "0,-5,0,5,5,0")
        .attr("class", function(d) { return d[type][0] > d[type][1] ? "good" : "bad" });
}

function draw_results(node) {
    d3.json("results.json", function(err, data) {
        if (err) return console.error(err);
    
        data.sort(sort_by("doubleAvg"));
        make_graph(node, data, "doubleAvg");
    });
}
