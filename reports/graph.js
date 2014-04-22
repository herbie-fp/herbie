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

function drawLines(lines, colors, names, g){
    var allLinePoints = []
    allLinePoints = allLinePoints.concat.apply(allLinePoints, lines);

    var maxPosX = d3.max(allLinePoints, function(p){return p[0];});
    var minPosX = d3.min(allLinePoints.filter(function(p) { return p[0] > 0; }), function(p) {return p[0]});
    var maxNegX = d3.max(allLinePoints.filter(function(p) { return p[0] < 0; }), function(p) {return p[0]});
    var minNegX = d3.min(allLinePoints, function(p){return p[0];});
    var maxY = d3.max(allLinePoints, function(p){return p[1]});
    var minY = d3.min(allLinePoints, function(p){return p[1]});

    // Scales for translating points.

    var posHeight = maxY - 1;
    var negHeight = -1 - minY;
    var totalHeight = posHeight + negHeight;
    var negScalar = negHeight / totalHeight;
    
    y = d3.scale.log().clamp(true)
	.domain([1, posHeight])
	.range([h * negScalar, h - margin]);
    negY = d3.scale.log().clamp(true)
	.domain([1 - negHeight, -1])
	.range([0 + margin, h * negScalar])
    var xRadius = Math.max(maxPosX - minPosX, maxNegX - minNegX);
    x = d3.scale.log().clamp(true)
	.domain([minPosX , minPosX + xRadius])
	.range([(w/2), w - margin]);
    negX = d3.scale.log().clamp(true)
	.domain([maxNegX - xRadius, maxNegX])
	.range([0 + margin, (w/2)]);

    // Line functions for drawing paths.
    
    var linefunc = d3.svg.line()
	.x(function(d) {
	    if (d[0] < 0){
		var result = negX(d[0]);
	    }else{
		var result = x(d[0]);
	    }
	    return result;
	})
	.y(function(d) {
	    return -1 * ((d[1] < 0) ? negY(d[1]) : y(d[1]));
	});

    // Draw the positive and negative paths for each given dataset.
    var index = 0;
    while (index < lines.length){
    	var lineData = lines[index];
    	var line = linefunc(lineData);
    	var color = colors[index];
    	g.append("svg:path").attr("d", line)
    	    .attr("stroke", color);
    	++index;
    }

    // Draw the axis
    
    g.append("svg:line")
    	.attr("x1", negX(maxNegX - xRadius))
    	.attr("y1", -1 * y(minY))
    	.attr("x2", x(minPosX + xRadius))
    	.attr("y2", -1 * y(minY))
    g.append("svg:line").
    	attr("x1", x(0)).
    	attr("y1", -1 * y(minY)).
    	attr("x2", x(0)).
    	attr("y2", -1 * h)

    // Draw the key
    var keyPosX = 50;
    var keyPosY = -h + 20;
    var verticleSpace = 20;
    var horizontalSpace = 10;
    for(var index = 0; index < lines.length; ++index){
    	g.append("svg:circle")
    	    .attr("cx", keyPosX)
    	    .attr("cy", keyPosY + (verticleSpace * index))
    	    .attr("r", (verticleSpace / 2) - 2)
    	    .attr("stroke", colors[index])
    	    .attr("fill", colors[index]);

    	g.append("svg:text")
    	    .attr("x", keyPosX + horizontalSpace)
    	    .attr("y", keyPosY + (verticleSpace * index) + 5)
    	    .attr("fill", "black")
    	    .text(names[index]);
    }

    // Get the intervals at which to draw labels and tick marks.
    
    var xticks = takeOnly(x.ticks(), 8);
    var negXticks = takeOnly(negX.ticks(), 8);
    var yticks = takeOnly(y.ticks(), 8);

    // Draw the labels

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
    	text(d3.format(".0e")).
    	attr("x", 0 + (margin /2)).
    	attr("y", function(d) {return -1 * y(d) - 5 }).
    	attr("text-anchor", "right").
    	attr("dy", 4)

    // Draw the tick marks
    
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
var filename = "data.json"

var w = 1200,
h = 400,
margin = 100

var vise = d3.select("body")
    .append("svg:svg")
    .attr("width", w)
    .attr("height", h)

var ge = vise.append("svg:g")
    .attr("transform", "translate(0, 400)");

var visf = d3.select("body")
    .append("svg:svg")
    .attr("width", w)
    .attr("height", h);

var gf = visf.append("svg:g")
    .attr("transform", "translate(0, 450)");

d3.json(filename, function(error,dat){
    if (error) return console.warn(error);
    
    filedata = dat;
    var preerrors = filedata[0];
    var posterrors = filedata[1];
    var improvement = filedata[2];
    var exacts = filedata[3];
    
    drawLines([preerrors, posterrors, improvement], ["yellow", "blue", "green"], ["preerrors", "posterrors", "improvement"], ge);
    drawLines([exacts], ["green"], ["exacts"], gf);
});

