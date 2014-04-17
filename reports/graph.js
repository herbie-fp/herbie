var filename = "data.json"

var data = [3, 6, 2, 7, 5, 2, 1, 3, 8, 9, 2, 5, 7],
w = 800,
h = 400,
margin = 100

var vis = d3.select("body")
    .append("svg:svg")
    .attr("width", w)
    .attr("height", h)

var g = vis.append("svg:g")
    .attr("transform", "translate(0, 400)");

d3.json(filename, function(error,dat){
    if (error) return console.warn(error);
    
    var filedata = dat;
    var preerrors = filedata[0];
    var posterrors = filedata[1];
    var maxX = d3.max(preerrors, function(p){return p[0];});
    var minX = d3.min(preerrors, function(p){return p[0];});
    var maxY = d3.max(preerrors, function(p){return p[1];});
    var minY = 1;
    var y = d3.scale.log().clamp(true)
	.domain([0.1, maxY])
	.range([0 + margin, h - margin]);
    var x = d3.scale.log().clamp(true)
	.domain([minX , maxX])
	.range([0 + margin, w - margin]);
    
    var line = d3.svg.line()
	.x(function(d) { return x(d[0]); })
	.y(function(d) { return -1 * y(d[1]); });
    
    g.append("svg:path").attr("d", line(preerrors))
	.attr("stroke", "blue");
    g.append("svg:path").attr("d", line(posterrors))
	.attr("stroke", "green");

    g.append("svg:line")
    	.attr("x1", x(0))
    	.attr("y1", -1 * 25)
    	.attr("x2", w)
    	.attr("y2", -1 * 25)
    g.append("svg:line").
    	attr("x1", x(0)).
    	attr("y1", -1 * 25).
    	attr("x2", x(0)).
    	attr("y2", -1 * h)

    var xticks = []
    var xt = x.ticks();
    for (var i = 0; i < xt.length / 50; i++) {
        xticks.push(xt[50*i]);
    }

    var yticks = []
    var yt = y.ticks();
    for (var i = 0; i < yt.length / 20; i++) {
        yticks.push(yt[20*i]);
    }

    g.selectAll(".xLabel").
    	data(xticks).
    	enter().append("svg:text").
    	attr("class", "xLabel").
    	text(String).
    	attr("x", function(d) {return x(d) * 5}).
    	attr("y", 0).
    	attr("text-anchor", "right")

    g.selectAll(".yLabel").
    	data(yticks).
    	enter().append("svg:text").
    	attr("class", "yLabel").
    	text(String).
    	attr("x", 0).
    	attr("y", function(d) {return -1 * y(d) }).
    	attr("text-anchor", "right").
    	attr("dy", 4)

    g.selectAll(".xTicks").
    	data(xticks).
    	enter().append("svg:line").
    	attr("class", "xTicks").
    	attr("x1", function(d) { return x(d); }).
    	attr("y1", -1 * y(0)).
    	attr("x2", function(d) { return x(d); }).
    	attr("y2", -1 * y(0) - 5)

    g.selectAll(".yTicks").
    	data(yticks).
    	enter().append("svg:line").
    	attr("class", "yTicks").
    	attr("y1", function(d) { return -1 * y(d+0.1); }).
    	attr("x1", x(0) - 5).
    	attr("y2", function(d) { return -1 * y(d+0.1); }).
    	attr("x2", x(0))

});

//g.append("svg:path").attr("d", line(data));

