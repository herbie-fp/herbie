
margin = 10;
width = 740;
height = 200;
labels = 40;
precision = 64;
precision_step = 8;

key = undefined;
used_branch = {};

function get_point(tr) {
    var tests = tr.children[3].textContent.split("/");
    var bits = tr.children[4].textContent.split("/");
    
    return {
        tests: { got: +tests[0], total: +tests[1]},
        bits: { got: +bits[0], total: +bits[1] },
        branch: tr.children[1].textContent,
        time: +tr.children[0].children[0].getAttribute("data-unix"),
        elt: tr,
    };
}

function get_data(tag, table) {
    var trs = table.querySelectorAll("tbody tr");
    var data = [];
    for (var i = 0; i < trs.length; i++) {
        var note = trs[i].getElementsByClassName("note")[0];
        var trtag = note && note.textContent;
        if (trtag) used_branch[trs[i].children[1].textContent] = true;
        if (trtag == tag) data.push(get_point(trs[i]));
    }
    data.sort(function(a,b) {return a.time - b.time});
    return data;
}

function step_size(max) {
    if (max > 400) {
        return Math.round(max / 400) * 100;
    } else if (max > 40) {
        return Math.round(max / 40) * 10;
    } else if (max > 4) {
        return Math.round(max / 4);
    } else if (max > .4) {
        return Math.round(max / .4) / 10;
    } else {
        throw "Data points error";
    }
}

function make_graph(node, data, type) {
    var len = data.length;
    var spacing = width / len;
    
    var max = 0;
    for (var i = 0; i < len; i++) {
        if (data[i][type].total > max) max = data[i][type].total;
    }
    var step = step_size(max);
    var steps = max ? Math.ceil(max / step) : 0;
    var max = steps * step;

    var svg = node
        .attr("width", width + 2 * margin + labels)
        .attr("height", height + 2 * margin)
        .append("g").attr("transform", "translate(" + (margin + labels) + "," + margin + ")");

    svg.append("line")
        .attr("class", "gridline")
        .attr("x1", 0)
        .attr("x2", width)
        .attr("y1", height)
        .attr("y2", height);

    svg.append("polygon").attr("class", "gridline").attr("points", "0,3,0,-3,5,0")
        .attr("transform", "translate(" + width + "," + height + ")");

    for (var i = 1; i <= steps; i++) {
        svg.append("line")
            .attr("class", "guide")
            .attr("x1", 0)
            .attr("x2", width)
            .attr("y1", height - (i / steps) * height)
            .attr("y2", height - (i / steps) * height);
        
        svg.append("text").text(i * step).attr("class", "guide")
            .attr("x", -5)
            .attr("y", height - (i / steps) * height + 6);
    }

    var bar = svg.selectAll("g").data(data).enter();

    var g = bar.append("g").attr("class", "arrow");

    g.append("line")
        .attr("stroke", function(d) { return key(d.branch) })
        .attr("x1", function(d, i) { return (i + .5) * spacing })
        .attr("x2", function(d, i) { return (i + .5) * spacing })
        .attr("y1", function(d) { return height - height * d[type].total / max })
        .attr("y2", function(d) { return height - height * (d[type].total - d[type].got) / max - 5 });

    g.append("polygon").attr("points", "-3,-5,3,-5,0,0")
        .attr("fill", function(d) { return key(d.branch) })
        .attr("transform", function(d, i) {
            return "translate(" + spacing*(i + .5) + ", " + (height - height * (d[type].total - d[type].got) / max) + ")";
        });

    g.append("line")
        .attr("stroke", function(d) { return key(d.branch) })
        .attr("x1", function(d, i) { return (i + .5) * spacing - 3 })
        .attr("x2", function(d, i) { return (i + .5) * spacing + 3 })
        .attr("y1", function(d) { return height - height * d[type].total / max })
        .attr("y2", function(d) { return height - height * d[type].total / max });

    g.append("line")
        .attr("stroke", function(d) { return key(d.branch) })
        .attr("x1", function(d, i) { return (i + .5) * spacing - 3 })
        .attr("x2", function(d, i) { return (i + .5) * spacing + 3 })
        .attr("y1", function(d) { return height - height * (d[type].total - d[type].got) / max })
        .attr("y2", function(d) { return height - height * (d[type].total - d[type].got) / max });
    
    g.on("click", function(d) {
        d.elt.click();
    });
}

function draw_results(node) {
    function draw(tag) {
        return function(evt) {
            node.selectAll("*").remove();
            var old = document.getElementsByClassName("selected")[0];
            if (old) old.classList.remove("selected");
            document.getElementById("suite-" + tag).classList.add("selected");
            make_graph(node, get_data(tag, document.getElementById("reports")), "bits");
        }
    }

    var branches = [];
    var toclinks = document.getElementById("toc").querySelectorAll("li a");
    for (var i = 0; i < toclinks.length; i++) {
        branches.push(toclinks[i].textContent);
    }
    key = d3.scale.category10().domain(branches);

    var types = {};
    var notes = document.getElementsByClassName("note");
    for (var i = 0; i < notes.length; i++) {
        types[notes[i].textContent] = (types[notes[i].textContent] || 0) + 1;
    }

    var best_type = null;
    var type_list = document.getElementById("suites");
    for (var type in types) {
        if (!best_type || types[type] > types[best_type]) best_type = type;
        var li = document.createElement("li");
        var a = document.createElement("a");
        a.href = "#" + type;
        a.textContent = type;
        a.addEventListener("click", draw(type));
        li.id = "suite-" + type;
        li.appendChild(a);
        type_list.appendChild(li)
    }

    draw(best_type)();

    for (var i = 0; i < toclinks.length; i++) {
        if (used_branch[branches[i]]) {
            toclinks[i].style.color = key(branches[i]);
            toclinks[i].style.borderColor = key(branches[i]);
        }
    }
}
