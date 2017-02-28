CONSTANTS = ["PI", "E"]
FUNCTIONS = {
    "+": [2], "-": [1, 2], "*": [2], "/": [2], "fabs": [1],
    "sqrt": [1], "sqr": [1], "exp": [1], "log": [1], "pow": [2],
    "sin": [1], "cos": [1], "tan": [1], "cot": [1],
    "asin": [1], "acos": [1], "atan": [1],
    "sinh": [1], "cosh": [1], "tanh": [1]
}

SECRETFUNCTIONS = {"^": "pow", "**": "pow", "abs": "fabs"}

function tree_errors(tree) /* tree -> list */ {
    var messages = [];
    var names = [];

    bottom_up(tree, function(node, path, parent) {
        switch(node.type) {
        case "ConstantNode":
            if (node.valueType !== "number")
                messages.push("Constants that are " + node.valueType + "s not supported.");
            break;
        case "FunctionNode":
            node.name = SECRETFUNCTIONS[node.name] || node.name;
            if (!FUNCTIONS[node.name]) {
                messages.push("Function <code>" + node.name + "</code> unsupported.");
            } else if (FUNCTIONS[node.name].indexOf(node.args.length) === -1) {
                messages.push("Function <code>" + node.name + "</code> expects " +
                              FUNCTIONS[node.name].join(" or ") + " arguments");
            }
            break;
        case "OperatorNode":
            node.op = SECRETFUNCTIONS[node.op] || node.op;
            if (!FUNCTIONS[node.op]) {
                messages.push("Operator <code>" + node.op + "</code> unsupported.");
            } else if (FUNCTIONS[node.op].indexOf(node.args.length) === -1) {
                messages.push("Operator <code>" + node.op + "</code> expects " +
                              FUNCTIONS[node.op].join(" or ") + " arguments");
            }
            break;
        case "SymbolNode":
            if (CONSTANTS.indexOf(node.name) === -1)
                names.push(node.name);
            break;
        default:
            messages.push("Unsupported syntax; found unexpected <code>" + node.type + "</code>.")
            break;
        }
    });

    if (names.length == 0) {
        messages.push("No variables mentioned.");
    }

    return messages;
}

function bottom_up(tree, cb) {
    if (tree.args) {
        tree.args = tree.args.map(function(node) {return bottom_up(node, cb)});
        tree.res = cb(tree);
    } else {
        tree.res = cb(tree);
    }
    return tree;
}

function dump_tree(tree, txt) /* tree string -> string */ {
    function extract(args) {return args.map(function(n) {return n.res});}
    var names = [];
    var body = bottom_up(tree, function(node) {
        switch(node.type) {
        case "ConstantNode":
            return "" + node.value;
        case "FunctionNode":
            node.name = SECRETFUNCTIONS[node.name] || node.name;
            return "(" + node.name + " " + extract(node.args).join(" ") + ")";
        case "OperatorNode":
            node.op = SECRETFUNCTIONS[node.op] || node.op;
            return "(" + node.op + " " + extract(node.args).join(" ") + ")";
        case "SymbolNode":
            if (CONSTANTS.indexOf(node.name) === -1)
                names.push(node.name);
            return node.name;
        default:
            throw SyntaxError("Invalid tree!");
        }
    });

    var dnames = [];
    for (var i = 0; i < names.length; i++) {
        if (dnames.indexOf(names[i]) === -1) dnames.push(names[i]);
    }

    var name = txt.replace("\\", "\\\\").replace("\"", "\\\"");
    return "(FPCore (" + dnames.join(" ") + ") :name \"" + name + "\" "  + body.res + ")";
}

function onload() /* null -> null */ {
    var form = document.getElementById("formula");
    var input = document.querySelector("#formula input");
    input.setAttribute("name", "formula-math");
    input.setAttribute("placeholder", "sqrt(x + 1) - sqrt(x)");
    input.removeAttribute("disabled");
    var hidden = document.createElement("input");
    hidden.type = "hidden";
    hidden.setAttribute("name", "formula");
    form.appendChild(hidden);

    document.getElementById("mathjs-instructions").style.display = "block";
    document.getElementById("lisp-instructions").style.display = "none";

    input.addEventListener("keyup", function(evt) {
        var txt = input.value;
        var tree, errors = [];
        try {
            tree = math.parse(txt);
            errors = tree_errors(tree);
        } catch (e) {
            errors = ["" + e];
        }

        if (txt && errors.length > 0) {
            document.getElementById("errors").innerHTML = "<li>" + errors.join("</li><li>") + "</li>";
        } else {
            document.getElementById("errors").innerHTML = "";
        }
    });

    form.addEventListener("submit", function(evt) {
        var txt = input.value;
        var tree, errors;
        try {
            tree = math.parse(txt);
            errors = tree_errors(tree);
        } catch (e) {
            errors = ["" + e];
        }

        if (errors.length > 0) {
            document.getElementById("errors").innerHTML = "<li>" + errors.join("</li><li>") + "</li>";
            evt.preventDefault();
            return false;
        } else {
            document.getElementById("errors").innerHTML = "";
        }

        var lisp = dump_tree(tree, txt);
        hidden.setAttribute("value", lisp);

        var url = document.getElementById("formula").getAttribute("data-progress");
        if (url) {
            input.disabled = "true";
            ajax_submit(url, txt, lisp);
            evt.preventDefault();
            return false;
        } else {
            return true;
        }
    });
}

function clean_progress(str) {
    var lines = str.split("\n");
    var outlines = [];
    for (var i = 0; i < lines.length; i++) {
        var line = lines[i];
        var words = line.split("  ");
        var word0 = words.shift();
        outlines.push(htmlescape((word0.substring(0, 6) === "* * * " ? "* " : "") + words.join("  ")));
    }
    return outlines.join("\n");
}

function htmlescape(str) {
    return ("" + str).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;");
}

function get_progress(loc) {
    var req2 = new XMLHttpRequest();
    req2.open("GET", loc);
    req2.onreadystatechange = function() {
        if (req2.readyState == 4) {
            if (req2.status == 202) {
                document.getElementById("progress").innerHTML = clean_progress(req2.responseText);
                setTimeout(function() {get_progress(loc)}, 100);
            } else if (req2.status == 201) {
                var loc2 = req2.getResponseHeader("Location");
                window.location.href = loc2;
            } else {
                document.getElementById("errors").innerHTML = req2.responseText;
            }
        }
    }
    req2.send();
}

function ajax_submit(url, text, lisp) {
    document.getElementById("progress").style.display = "block";
    var req = new XMLHttpRequest();
    req.open("POST", url);
    req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
    req.onreadystatechange = function() {
        if (req.readyState == 4) {
            if (req.status == 201) {
                var jobcount = req.getResponseHeader("X-Job-Count");
                var jobelt = document.getElementById("num-jobs")
                if (jobelt) jobelt.innerHTML = jobcount - 1;
                var loc = req.getResponseHeader("Location");
                get_progress(loc);
            } else {
                document.getElementById("errors").innerHTML = req.responseText;
            }
        }
    }
    var content = "formula=" + encodeURIComponent(lisp) + "&formula-math=" + encodeURIComponent(text);
    req.send(content);
}

window.addEventListener("load", onload);
