margin = 10;
barheight = 10;
width = 505;
textbar = 20;

function sort_by(i1, i2) {
    return function(a, b) {
        return b[i1][i2] - a[i1][i2];
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
        .attr("width", width + 2 * margin)
        .attr("height", len * barheight + 2 * margin + textbar)
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
    function title(d, i) { return d.name + " (" + r10(precision - d["Old"][start]) + " to " + r10(precision - d["Old"][end]) + ")"; }

    bar.append("line")
        .attr("class", "guide")
        .attr("x1", 0)
        .attr("x2", function(d) { return (precision - Math.max(d["Old"][start], d["Old"][end])) / precision * width })
        .attr("y1", line_y)
        .attr("y2", line_y);

    var g = bar.append("g").attr("title", title);

    g.append("line").attr("class", "old")
        .attr("x1", function(d) {return (precision - d["Old"][start]) / precision * width})
        .attr("x2", function(d) { return (precision - d["Old"][end]) / precision * width })
        .attr("y1", line_y)
        .attr("y2", line_y);

    g.append("line").attr("class", "new")
        .attr("x1", function(d) {return (precision - d["Old"][end]) / precision * width})
        .attr("x2", function(d) { return (precision - d["New"][end]) / precision * width })
        .attr("y1", line_y)
        .attr("y2", line_y);

    g.append("g")
        .attr("class", function(d) { return d["New"][end] < d["Old"][end] - .5 ? "new" : "old" })
        .attr("transform", function(d, i) {
        return "translate(" + ((precision - d["New"][end]) / precision * width) + ", " + line_y(d, i) + ")";
    })
        .append("polygon").attr("points", "0,-3,0,3,5,0");
}

function draw_results(node) {
    d3.json("results-old.json", function(err, old_data) {
        d3.json("results-new.json", function(err, new_data) {
            if (err) return console.error(err);
            data = [];
            old_data.tests.sort(function(a, b) { return (a.input < b.input) ? -1 : (a.input == b.input) ? 0 : 1});
            new_data.tests.sort(function(a, b) { return (a.input < b.input) ? -1 : (a.input == b.input) ? 0 : 1});
            for (var i = 0; i < old_data.tests.length; i++) {
                data.push({Old: old_data.tests[i], New: new_data.tests[i]});
            }
    
            data.sort(sort_by("Old", "start"));
            make_graph(node, data, "start", "end");
        });
    });
}
