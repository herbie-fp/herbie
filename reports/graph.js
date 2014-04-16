var filename = "test.json"

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
    
    var filedata = dat,
    preerrors = filedata[0],
    posterrors = filedata[1],
    maxX = d3.max(preerrors, function(p){return p[0];}),
    minX = d3.min(preerrors, function(p){return p[0];}),
    maxY = d3.max(preerrors, function(p){return p[1];}),
    minY = 0,
    y = d3.scale.log()
	.domain([0.1, maxY])
	.range([0 + margin, h - margin]),
    x = d3.scale.log().clamp(true)
	.domain([minX , maxX])
	.range([0 + margin, w - margin])
    
    var line = d3.svg.line()
	.x(function(d) { return x(d[0]); })
	.y(function(d) { return -1 * y(d[1]); })
    
    g.append("svg:path").attr("d", line(preerrors));
    //g.append("svg:path").attr("d", line(posterrors));

    // g.append("svg:line")
    // 	.attr("x1", x(0))
    // 	.attr("y1", -1 * 0)
    // 	.attr("x2", x(w))
    // 	.attr("y2", -1 * 0)
    // g.append("svg:line").
    // 	attr("x1", x(0)).
    // 	attr("y1", -1 * 0).
    // 	attr("x2", x(0)).
    // 	attr("y2", -1 * y(maxY))

    // // g.selectAll(".xLabel").
    // // 	data(x.ticks(5)).
    // // 	enter().append("svg:text").
    // // 	attr("class", "xLabel").
    // // 	text(String).
    // // 	attr("x", function(d) {return x(d) }).
    // // 	attr("y", 0).
    // // 	attr("text-anchor", "right")

    // g.selectAll(".yLabel").
    // 	data(y.ticks(4)).
    // 	enter().append("svg:text").
    // 	attr("class", "yLabel").
    // 	text(String).
    // 	attr("x", 0).
    // 	attr("y", function(d) {return -1 * y(d) }).
    // 	attr("text-anchor", "right").
    // 	attr("dy", 4)

    // g.selectAll(".xTicks").
    // 	data(x.ticks(5)).
    // 	enter().append("svg:line").
    // 	attr("class", "xTicks").
    // 	attr("x1", function(d) { return x(d); }).
    // 	attr("y1", -1 * y(0)).
    // 	attr("x2", function(d) { return x(d); }).
    // 	attr("y2", -1 * y(-0.3))

    // g.selectAll(".yTicks").
    // 	data(y.ticks(4)).
    // 	enter().append("svg:line").
    // 	attr("class", "yTicks").
    // 	attr("y1", function(d) { return -1 * y(d+0.1); }).
    // 	attr("x1", x(-0.3)).
    // 	attr("y2", function(d) { return -1 * y(d+0.1); }).
    // 	attr("x2", x(0))

});

//g.append("svg:path").attr("d", line(data));

