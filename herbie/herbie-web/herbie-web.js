// Some configuration options which control the size and layout of
// things.
graph_width = 400;
graph_height = 300;
selection_height = 300;
calt_graph_width = 230;
calt_graph_height = 100;
calt_box_height = 450;
selection_margin = 20;
unselected_formula_margin = "30%";
loc_colors = ["blue", "orange", "green", "purple"];


// Some global state useful to keep all the phases in sync.
selected_loc = -1;
highlighted_loc = -1;
ranges = -1;
selected_calts = d3.set();
next_link = 0;
repick_link = 0;
candidate_data = 0;

init_json_path = d3.select(".placeholder").attr("data-json");

// Load the initial json and start rendering when it finishes loading.
d3.json(init_json_path, function(error, data){
    if (error){
        alert("You broke Herbie! Sorry about that. The project is still under development, and we're working out the kinks. Try a different formula.");
        return;
    }
    global_data = data;
    render_chosen_formula(data);
    next_link = data.next_link;
});

// Renders the currently selected formula with it's locations and
// ranges of error.
function render_chosen_formula(data){
    // Reset the loc to be considered "unselected"
    selected_loc = -1;
    // Create a div to contain the selected formula and it's graph.
    var sfbox = d3.select("body").append("div")
        .classed("selectedFormulaBox", true)
        .style("width", graph_width+"px")
        .style("margin-left", unselected_formula_margin);

    // Instructions
    sfbox.append("h3").classed("instr", true)
        .text("Hover over a circled node to see which input ranges have high error due\
 to that operation. Click to try to rewrite that operation.");
    
    // The texified formula
    sfbox.append("p").classed("formula", true)
        .text("$$"+data.formula+"$$");

    // An svg for drawing the selected formula's graph.
    var draw = sfbox
        .append("svg")
        .attr("width", graph_width+"px")
        .attr("height", graph_height+"px");

    // The graph of error that sits under it.
    var graph = draw
        .append("image")
        .classed("graph", true)
        .attr("xlink:href", data.error_graph)
        .attr("height", graph_height+"px")
        .attr("width", graph_width+"px");
    
    sfbox.append("p")
	.classed("axis_label", true)
	.style("text-align", "center")
	.append("b")
	.text("$$"+data.axis_label+"$$");

    // Create range indicators for each range in the data.
    draw.selectAll(".rangeIndicator")
        .data(data.loc_ranges)
        .enter().append("rect")
        .attr("class", function(d, i) {return "loc" + d.locid + "range"; })
        .classed("rangeIndicator", true)
        .attr("x", function(d, i) {return d.start*graph_width;})
        .attr("width", function(d, i) { return (d.end - d.start) * graph_width;})
        .attr("y", selection_margin)
        .attr("height", graph_height - selection_margin * 2)
        .attr("opacity", .3)
        .attr("fill", function(d, i) {return loc_colors[d.locid]; });

    // Save the ranges for use when rendering the calts.
    ranges = data.loc_ranges;

    // If this isn't the first time around, let them pick a different
    // alt.
    if (repick_link != 0){
	sfbox.append("p").classed("instr", true)
	    .classed("repick", true)
	    .text("Or,");
	sfbox.append("button")
	    .attr("name", "repick_alt")
	    .attr("class", "repick_button repick")
	    .text("Pick another candidate")
	    .on("click", repick);
    }
    

    // Add links to the math once it's finished rendering.
    MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
    MathJax.Hub.Queue(add_loc_links);
}

function add_loc_links(){
    // Place a box over the whole formula so that we can capture click events and route them properly.
    var formulaRect = d3.select(".formula")
	.node().getBoundingClientRect();
    var clickdiv = d3.select(".selectedFormulaBox")
        .append("div")
        .attr("class", "clickdiv")
        .style("background", "transparent")
        .style("position", "fixed")
        .style("width", formulaRect.width + "px")
        .style("height", formulaRect.height + "px")
        .style("left", formulaRect.x + "px")
        .style("top", formulaRect.y + "px");
    clickdiv.on('click', dispatch_location)
        .on('mousemove', update_highlights)
        .on('mouseout', function () {unhighlight_location(highlighted_loc);});
}
// Convert absolute page coordinates to the location which best fits
// them, returning negative one if there is no matching location. If
// multiple locations match, returns the one with the smallest total
// area.
function pos_to_loc(coordinates){
    var locations = d3.selectAll(".location");
    var smallestArea = Infinity;
    var locFound = -1;
    locations.each(function (d){
        locRect = d3.select(this)
	// Doing this span thing gets us the inner span which actually has
	// the most accurate bounding box.
	    .node().getBoundingClientRect();
        if (coordinates.x > locRect.x && coordinates.x < locRect.x + locRect.width &&
            coordinates.y > locRect.y && coordinates.y < locRect.y + locRect.height &&
            locRect.width * locRect.height < smallestArea){
            smallestArea = locRect.width * locRect.height;
            locFound = d3.select(this).attr("id");
        }
    });
    return locFound;
}
// Called on a mouse click, determines the proper location and selects
// it.
function dispatch_location(){
    var mousepos = {x: d3.event.pageX, y: d3.event.pageY };
    var loc = pos_to_loc(mousepos);
    if (loc == -1) return;
    select_location(loc);
}
// Called when the mouse moves over the formula, updates the
// highlighted location to be the one that best matches the mouse
// position.
function update_highlights(){
    var mousepos = {x: d3.event.pageX, y: d3.event.pageY };
    var loc = pos_to_loc(mousepos);
    if (loc == -1) unhighlight_location(highlighted_loc);
    else highlight_location(loc);
}
// Called when the mouse enters a location of error.
function highlight_location(loc_number){
    var affected_ranges = d3.selectAll(".loc"+loc_number+"range");
    affected_ranges
    // Grow the selection by ten percent on the top and bottom
        .attr("y", function() {
            var old = d3.select(this);
            return old.attr("y") - old.attr("height") * .05;
        })
        .attr("height", function() {
            var old = d3.select(this);
            return old.attr("height") * 1.1;
        })
    // Make it a little more opaque.
        .attr("opacity", 0.4);
    // If there was another location selected, deselect it before
    // finalizing this selection.
    if (highlighted_loc != -1)
        unhighlight_location(highlighted_loc);
    highlighted_loc = loc_number;
}

// Called when the mouse leaves a location of error.
function unhighlight_location(loc_number){
    // If we had already deselected this location, or never selected
    // it, don't do anything.
    if (highlighted_loc != loc_number)
        return;
    // Restore the highlight to it's old size and opacity.
    var affected_ranges = d3.selectAll(".loc"+loc_number+"range");
    affected_ranges
        .attr("height", function() {
            return d3.select(this).attr("height") / 1.1;
        })
        .attr("y", function() {
            var old = d3.select(this);
            return Number(old.attr("y")) + old.attr("height") * .05;
        })
        .attr("opacity", 0.3);
    // Set the highlighted location to nothing.
    highlighted_loc = -1;
}
// Called when the user clicks on a particular location.
function select_location(loc_number){
    if (selected_loc != -1) return;
    // Store that this was the selected loc for later.
    selected_loc = loc_number;
    // Select the selected formula box and have it slide over to the
    // side to make room for the calts.
    var sfbox = d3.select(".selectedFormulaBox");
    sfbox.transition()
        .duration(1000)
        .styleTween("margin-left", function(){ return d3.interpolate(unselected_formula_margin, "0%"); });
    d3.selectAll(".repick")
	.transition()
	.duration(100)
	.attr("opacity", 0)
	.remove();

    // Fade out the ranges that are not associated with the selected
    // location.
    var unused_selections = d3.selectAll(".rangeIndicator:not(.loc"+loc_number+"range)");
    unused_selections.transition()
        .duration(1000)
        .attr("opacity", 0)
        .remove();

    // Remove the old instructions.
    d3.select(".instr").transition()
        .duration(500)
        .attr("opacity", 0)
        .remove();

    // Shrink the range(s) back to it's original size.
    unhighlight_location(loc_number);
    d3.selectAll(".location")
        .on('click', null)
        .on('mouseover', null)
        .on('mouseout', null);
    // Load the response to the selection, and update the view when
    // it's done.
    d3.json(next_link + "?location-idx=" + loc_number, function(error, data){
        if (error){
            alert("You broke Herbie! Sorry about that. The project is still under development, and we're working out the kinks. Try a different formula.");
            return;
        }
	next_link = data.next_link;
        location_selected(data);
    });
}
// Called when we want to render phase 2 (calts) of the interface.
function location_selected(data){
    // Replace the formula in the selected formula box with a new
    // version from the server that only highlights the selected
    // location.
    d3.select(".formula")
        .text("$$"+data.selected_formula+"$$");
    d3.select(".selectedFormulaBox")
        .classed("phase2", true);
    d3.select("body").append("h3")
        .classed("instr", true)
        .classed("phase2", true)
        .text("Select the changes you want to keep")
        .style("text-align", "left");
    // Create boxes for the candidate children.
    var caltboxes = d3.select("body").selectAll(".caltdiv")
        .data(data.calts)
        .enter().append("div")
        .classed("caltdiv", true)
        .classed("phase2", true)
        .style("height", calt_box_height + "px");
    // Render the steps for each calt
    var stepboxes = caltboxes.selectAll(".stepdiv")
        .data(function(d, i){ return d.steps; })
        .enter().append("div")
        .classed("stepdiv", true);
    // For each step, print the label.
    stepboxes.append("p").classed("calt_step", true)
        .html(function(d,i) {
            if (d.rule == "simplify"){
                return "and simplify to:";
            }
	    if (d.rule == "taylor"){
		return "Series expand to get:"
	    }
            return "Apply the rule <br>\\("+ d.rule + "\\)<br> to get: ";
        })
	.style("font-size", 11);
    // For each step, print the resulting program.
    stepboxes.append("p").classed("calt_formula", true)
        .text(function(d,i) { return "$$" + d.prog + "$$"; })
	.style("font-size", 11);

    // Draw the graph svg
    var draws = caltboxes
        .append("svg")
        .attr("class", "calt_graph");
    
    // The actual graph of error
    var graph = draws
        .append("image")
        .classed("graph", true)
        .attr("xlink:href", function(d, i) { return d.graph; })
        .attr("height", "100%")
        .attr("width", "100%");

    // The range indicator(s) for the selected location
    draws.selectAll(".rangeIndicator")
        .data(ranges.filter(function (r) { return r.locid == selected_loc; }))
        .enter().append("rect")
        .classed("rangeIndicator", true)
        .attr("x", function(d, i) {return d.start*calt_graph_width;})
        .attr("width", function(d, i) { return (d.end - d.start) * calt_graph_width;})
        .attr("y", 0)
        .attr("height", calt_graph_height)
        .attr("opacity", .3)
        .attr("fill", loc_colors[selected_loc]);

    // Add the checkboxes
    var checkboxes = caltboxes.append("input")
        .attr("type", "checkbox")
        .attr("class", "selectCalt")
	.data(data.calts)
        .attr("id", function (d, i) {
	    return d.id;
	});

    // Give the checkboxes their behavior
    checkboxes.each(function (d, i) {
        var caltIdx = d3.select(this).attr("id");
        d3.select(this).on('change', function () {
            if (selected_calts.has(caltIdx))
                selected_calts.remove(caltIdx);
            else
                selected_calts.add(caltIdx);
        });
    });

    // Add the submit button
    d3.select("body").append("div")
        .attr("class", "finalize_box phase2")
        .append("button")
        .attr("name", "finalize_children")
        .attr("class", "finalize_button")
        .text("Accept")
        .on("click", select_children);
    
    // Typeset the formulas we've added.
    MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
}
function display_set(set){
    var result = "[";
    set.forEach(function (e){
        result += e + ",";
    });
    result += "]";
    return result;
}
function select_children(){
    // Fade out the phase 2 elements
    d3.selectAll(".phase2").transition()
        .duration(1000)
        .style("opacity", 0)
        .remove();
    // Load the server response for phase 3
    var response_params = "";
    selected_calts.forEach(function (idx) { response_params += "chosen-idx=" + idx + "&"; });
    d3.json(next_link + "?" + response_params,
            function(error, data){
                if (error){
                    alert("You broke Herbie! Sorry about that. The project is still under development, and we're working out the kinks. Try a different formula.");
                    return;
                }
		next_link = data.next_link;
                children_selected(data);
            });
}

function repick(){
    next_link = repick_link;
    d3.select(".selectedFormulaBox")
	.transition()
	.duration(100)
	.style("opacity", 0)
	.remove();
    children_selected(candidate_data);
}

function children_selected(data){
    candidate_data = data;
    // Render the current combination box.
    var comboBox = d3.select("body").append("div")
        .attr("class", "combo_box phase3");
    comboBox.append("p").text("Current Combination")
        .attr("class", "combo_label");
    comboBox.append("img")
        .attr("class", "combo_graph graph")
        .attr("src", data.combo_graph);
    comboBox.append("button")
        .attr("name", "finish")
        .attr("class", "finish_button")
        .text("Good enough")
        .on('click', function () { window.location.href = data.done_link; });

    // Create a box to hold all the candidates
    var candidatesBox = d3.select("body").append("div")
        .attr("class", "candidates_div phase3");
    candidatesBox.append("h3")
        .attr("class", "instr")
        .text("Or, pick the next program to start from");
    // Create boxes for each candidate
    var candidateBoxes = candidatesBox.selectAll(".candidate_box")
        .data(data.candidates).enter()
        .append("div")
        .attr("class", "candidate_box");

    // Add the choose button to each candidate.
    var buttons = candidateBoxes.append("div")
        .attr("class", "select_alt_box")
        .append("button")
        .attr("name", function (d, i) { return "selectChild" + d.id; })
        .attr("id", function (d, i) { return d.id; })
        .attr("class", "select_alt_button")
        .text("Choose");
    // Give the buttons their effects.
    buttons.each(function (d, i){
        var altId = d3.select(this).attr("id");
        d3.select(this).on('click', function () { select_next(altId); });
    });
    // Add the formula for each candidate.
    candidateBoxes.append("p")
        .text(function (d, i) {
            return "$$" + d.formula + "$$";
        });
    // Add the graph for each candidate.
    candidateBoxes.append("img")
        .attr("class", "candidate_graph graph")
        .attr("src", function (d, i) { return d.graph; });

    // Make everything invisible, and then slowly fade it in.
    d3.selectAll(".phase3")
        .style("opacity", 0)
        .transition()
        .delay(1000)
        .duration(1000)
        .style("opacity", 1);
    
    // Typeset the formulas we've added.
    MathJax.Hub.Queue(["Typeset",MathJax.Hub]);
}

function select_next(cand_id){
    // Fade out the elements, and when they're done fading out load
    // the json. This may cause unneccessary delays because the json
    // itself takes some time, which could be during the fade out, but
    // we'll cross that bridge when we come to it.

    var half_done = false;
    d3.selectAll(".phase3")
        .transition()
        .duration(1000)
        .style("opacity", 0)
        .remove()
        .each("end", function (d, i) {
            // Only invoke the function once.
            if (i == 0){
		// Super not thread safe, oh well.
		if (half_done) render_chosen_formula(global_data);
		else half_done = true;
            }
        });
    d3.json(next_link + "?cand-idx=" + cand_id, function(error, data){
        if (error){
            alert("You broke Herbie! Sorry about that. The project is still under development, and we're working out the kinks. Try a different formula.");
            return;
        }
        global_data = data;
	next_link = data.next_link;
	repick_link = data.repick_link;
	if (half_done) render_chosen_formula(data);
	else half_done = true;
    });
}
