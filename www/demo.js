CONSTANTS = ["pi", "e"]
FUNCTIONS = [
    "+", "-", "*", "/", "abs",
    "sqrt", "sqr", "exp", "log", "expt",
    "sin", "cos", "tan", "cot",
    "asin", "acos", "atan",
    "sinh", "cosh", "tanh"
]

SECRETFUNCTIONS = {"pow": "expt", "^": "expt", "**": "expt"}

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
            if (FUNCTIONS.indexOf(node.name) === -1)
                messages.push("Function <code>" + node.name + "</code> unsupported.");
            break;
        case "OperatorNode":
            node.op = SECRETFUNCTIONS[node.op] || node.op;
            if (FUNCTIONS.indexOf(node.op) === -1)
                messages.push("Operator <code>" + node.op + "</code> unsupported.");
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

function dump_tree(tree) /* tree -> string */ {
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

    return "(lambda (" + dnames.join(" ") + ") " + body.res + ")";
}

function onload() /* null -> null */ {
    var form = document.getElementById("formula");
    var input = document.querySelector("#formula input");
    input.setAttribute("name", "formula-math");
    input.setAttribute("placeholder", "sqrt(x + 1) - sqrt(x)");
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

        var lisp = dump_tree(tree);
        hidden.setAttribute("value", lisp);
    });
}

window.addEventListener("load", onload);
