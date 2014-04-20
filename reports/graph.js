var filename = "data.json"

var w = 1200,
h = 400,
margin = 100

var vis = d3.select("body")
    .append("svg:svg")
    .attr("width", w)
    .attr("height", h)

var g = vis.append("svg:g")
    .attr("transform", "translate(0, 400)");

function takeEvery(data, n){
    var result = [];
    for(var i = 0; i < data.length; i+= n){
	result.push(data[i]);
    }
    return result;
}
function takeOnly(data, n){
    var result = [];
    var frequency = data.length / n;
    for(var i = 0; i < data.length; i += frequency){
	result.push(data[Math.round(i)]);
    }
    return result;
}

function drawLines(lines, colors, g){
    var negLinePoints = lines.map( function(p){return p[0]});
    var posLinePoints = lines.map( function(p){return p[1]});
    var allNegLinePoints = [];
    allNegLinePoints = allNegLinePoints.concat.apply(allNegLinePoints, negLinePoints);
    var allPosLinePoints = [];
    allPosLinePoints = allPosLinePoints.concat.apply(allPosLinePoints, posLinePoints);

    var maxPosX = d3.max(allPosLinePoints, function(p){return p[0];});
    var minPosX = d3.min(allPosLinePoints, function(p){return p[0];});
    var maxNegX = d3.max(allNegLinePoints, function(p){return p[0];});
    var minNegX = d3.min(allNegLinePoints, function(p){return p[0];});
    var maxY = d3.max(allNegLinePoints.concat(allPosLinePoints), function(p){return p[1]});
    var minY = 1; // Encoding domain knowledge.
    
    var y = d3.scale.log().clamp(true)
	.domain([minY, maxY])
	.range([0 + margin, h - margin]);
    var x = d3.scale.log().clamp(true)
	.domain([minPosX , maxPosX])
	.range([(w/2), w - margin]);
    var negX = d3.scale.log().clamp(true)
	.domain([minNegX, maxNegX])
	.range([0 + margin, (w/2)]);

    var line = d3.svg.line()
	.x(function(d) { return x(d[0]); })
	.y(function(d) { return -1 * y(d[1]); });
    var negLine = d3.svg.line()
	.x(function(d) { return negX(d[0]); })
	.y(function(d) { return -1 * y(d[1]); });

    var index;
    for(index = 0; index < posLinePoints.length; ++index){
	g.append("svg:path").attr("d", line(posLinePoints[index]))
	    .attr("stroke", colors[index]);
    }
    for(index = 0; index < negLinePoints.length; ++index){
	g.append("svg:path").attr("d", negLine(negLinePoints[index]))
	    .attr("stroke", colors[index]);
    }

    g.append("svg:line")
    	.attr("x1", negX(minNegX))
    	.attr("y1", -1 * y(minY))
    	.attr("x2", x(maxPosX))
    	.attr("y2", -1 * y(minY))
    g.append("svg:line").
    	attr("x1", x(0)).
    	attr("y1", -1 * y(minY)).
    	attr("x2", x(0)).
    	attr("y2", -1 * h)

    var xticks = takeOnly(x.ticks(), 8);
    var negXticks = takeOnly(negX.ticks(), 8);
    var yticks = takeOnly(y.ticks(), 8);

    g.selectAll(".xLabel").
    	data(xticks).
    	enter().append("svg:text").
    	attr("class", "xLabel").
    	text(d3.format(".0e")).
    	attr("x", function(d) {return x(d)}).
    	attr("y", 0 - (margin / 2)).
    	attr("text-anchor", "right")

    g.selectAll(".negXLabel").
    	data(negXticks).
    	enter().append("svg:text").
    	attr("class", "negXLabel").
    	text(d3.format(".0e")).
    	attr("x", function(d) {return negX(d)}).
    	attr("y", 0 - (margin / 2)).
    	attr("text-anchor", "right")

    g.selectAll(".yLabel").
    	data(yticks).
    	enter().append("svg:text").
    	attr("class", "yLabel").
    	text(d3.format("e")).
    	attr("x", 0 + (margin /2)).
    	attr("y", function(d) {return -1 * y(d) - 5 }).
    	attr("text-anchor", "right").
    	attr("dy", 4)

    g.selectAll(".xTicks").
    	data(xticks).
    	enter().append("svg:line").
    	attr("class", "xTicks").
    	attr("x1", function(d) { return x(d); }).
    	attr("y1", -1 * y(minY)).
    	attr("x2", function(d) { return x(d); }).
    	attr("y2", -1 * y(minY) - 5)

    g.selectAll(".negXTicks").
    	data(negXticks).
    	enter().append("svg:line").
    	attr("class", "negXTicks").
    	attr("x1", function(d) { return negX(d); }).
    	attr("y1", -1 * y(minY)).
    	attr("x2", function(d) { return negX(d); }).
    	attr("y2", -1 * y(minY) - 5)

    g.selectAll(".yTicks").
    	data(yticks).
    	enter().append("svg:line").
    	attr("class", "yTicks").
    	attr("y1", function(d) { return -1 * y(d); }).
    	attr("x1", x(0) - 5).
    	attr("y2", function(d) { return -1 * y(d); }).
    	attr("x2", x(0))

}

d3.json(filename, function(error,dat){
    if (error) return console.warn(error);
    
    filedata = dat;
    var preerrors = filedata[0];
    var posterrors = filedata[1];
    
    drawLines([preerrors, posterrors], ["yellow", "blue"], g);
});

