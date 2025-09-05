CONSTANTS = {"PI": "real", "E": "real", "TRUE": "bool", "FALSE": "bool"}

FUNCTIONS = {}

"+ - * / pow copysign fdim fmin fmax fmod hypot remainder atan2".split(" ").forEach(function(op) {
    FUNCTIONS[op] = [["real", "real"], "real"];
});
("fabs sqrt exp log sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh " +
 "cbrt ceil erf erfc exp2 expm1 floor lgamma log10 log1p log2 logb rint " + 
 "round tgamma trunc").split(" ").forEach(function(op) {
     FUNCTIONS[op] = [["real"], "real"];
});
FUNCTIONS["fma"] = [["real", "real", "real"], "real"];
"< > == != <= >=".split(" ").forEach(function(op) {
    FUNCTIONS[op] = [["real", "real"], "bool"];
});
"and or".split(" ").forEach(function(op) {
    FUNCTIONS[op] = [["bool", "bool"], "bool"];
});

SECRETFUNCTIONS = {"^": "pow", "**": "pow", "abs": "fabs", "min": "fmin", "max": "fmax", "mod": "fmod"}

function tree_errors(tree, expected) /* tree -> list */ {
    var messages = [];
    var names = [];

    var rtype = bottom_up(tree, function(node, path, parent) {
        switch(node.type) {
        case "ConstantNode":
            if (["number", "boolean"].indexOf(node.valueType) === -1) {
                messages.push("Constants that are " + node.valueType + "s not supported.");
            }
            return ({"number": "real", "boolean": "bool"})[node.valueType] || "real";
        case "FunctionNode":
            node.name = SECRETFUNCTIONS[node.name] || node.name;
            if (!FUNCTIONS[node.name]) {
                messages.push("Function <code>" + node.name + "</code> unsupported.");
            } else if (FUNCTIONS[node.name][0].length !== node.args.length) {
                messages.push("Function <code>" + node.name + "</code> expects " +
                              FUNCTIONS[node.name][0].length + " arguments");
            } else if (""+extract(node.args) !== ""+FUNCTIONS[node.name][0]) {
                messages.push("Function <code>" + node.name + "</code>" +
                              " expects arguments of type " +
                              FUNCTIONS[node.name][0].join(", ") +
                              ", got " + extract(node.args).join(", "));
            }
            return (FUNCTIONS[node.name] || [[], "real"])[1];
        case "OperatorNode":
            node.op = SECRETFUNCTIONS[node.op] || node.op;
            if (!FUNCTIONS[node.op]) {
                messages.push("Operator <code>" + node.op + "</code> unsupported.");
            } else if (FUNCTIONS[node.op][0].length !== node.args.length &&
                       !(node.op === "-" && node.args.length === 1)) {
                messages.push("Operator <code>" + node.op + "</code> expects " +
                              FUNCTIONS[node.op][0].length + " arguments");
            } else if (""+extract(node.args) !== ""+FUNCTIONS[node.op][0] &&
                       !(node.op === "-" && ""+extract(node.args) === "real") &&
                       !(is_comparison(node.op) /* TODO improve */)) {
                messages.push("Operator <code>" + node.op + "</code>" +
                              " expects arguments of type " +
                              FUNCTIONS[node.op][0].join(", ") +
                              ", got " + extract(node.args).join(", "));
            }
            return (FUNCTIONS[node.op] || [[], "real"])[1];
        case "SymbolNode":
            if (!CONSTANTS[node.name]) {
                names.push(node.name);
                return "real";
            } else {
                return CONSTANTS[node.name];
            }
        case "ConditionalNode":
            if (node.condition.res !== "bool") {
                messages.push("Conditional has type " + node.condition.res + " instead of bool");
            }
            if (node.trueExpr.res !== node.falseExpr.res) {
                messages.push("Conditional branches have different types " + node.trueExpr.res + " and " + node.falseExpr.res);
            }
            return node.trueExpr.res;
        case "BlockNode":
            for (var i = 0; i < tree.blocks.length - 1; i++) {
                stmt = tree.blocks[i].node;
                if (stmt.type != "AssignmentNode")
                    messages.push("Expected an assignment statement before a semicolon: " + stmt);
            }
            return node.blocks[node.blocks.length - 1].node.res;
        default:
            messages.push("Unsupported syntax; found unexpected <code>" + node.type + "</code>.")
            return "real";
        }
    }).res;

    if (rtype !== expected) {
        messages.push("Expected an expression of type " + expected + ", got " + rtype);
    }

    return messages;
}

function bottom_up(tree, cb) {
    if (tree.args) {
        tree.args = tree.args.map(function(node) {return bottom_up(node, cb)});
    } else if (tree.condition) {
        tree.condition = bottom_up(tree.condition, cb);
        tree.trueExpr = bottom_up(tree.trueExpr, cb);
        tree.falseExpr = bottom_up(tree.falseExpr, cb);
    } else if (tree.blocks) {
        for (var i = 0; i < tree.blocks.length - 1; i++) {
            stmt = tree.blocks[i].node;
            if (stmt.type != "AssignmentNode")
                throw SyntaxError("Expected an assignment statement before a semicolon: " + stmt);
            stmt.expr = bottom_up(stmt.expr, cb);
        }

        tree.blocks[tree.blocks.length - 1].node = bottom_up(tree.blocks[tree.blocks.length - 1].node, cb);
    }

    tree.res = cb(tree);
    return tree;
}

function extract(args) {
    return args.map(function(n) { return n.res });
}

function dump_fpcore(formula) {
    var tree = math.parse(formula);

    var names = [];
    var body = dump_tree(tree, names);
    var precondition = get_precondition_from_input_ranges(formula);

    var dnames = [];
    for (var i = 0; i < names.length; i++) {
        if (dnames.indexOf(names[i]) === -1) dnames.push(names[i]);
    }

    var name = formula.replace("\\", "\\\\").replace("\"", "\\\"");
    var fpcore = "(FPCore (" + dnames.join(" ") + ")\n  :name \"" + name + "\"";
    if (precondition) fpcore += "\n  :pre " + precondition;

    return fpcore + "\n  "  + body + ")";
}

function is_comparison(name) {
    return ["==", "!=", "<", ">", "<=", ">="].indexOf(name) !== -1;
}

function flatten_comparisons(node) {
    var terms = [];
    (function collect_terms(node) {
        if (node.type == "OperatorNode" && is_comparison(node.op)) {
            collect_terms(node.args[0]);
            collect_terms(node.args[1]);
        } else {
            terms.push(node.res);
        }
    })(node);
    var conjuncts = [];
    var iters = 0;
    (function do_flatten(node) {
        if (node.type == "OperatorNode" && is_comparison(node.op)) {
            do_flatten(node.args[0]);
            var i = iters++; // save old value and increment it
            var prev = conjuncts[conjuncts.length - 1];
            if (prev && prev[0] == node.op && prev[2] == terms[i]) {
                prev.push(terms[i + 1]);
            } else {
                conjuncts.push([node.op, terms[i], terms[i+1]]);
            }
            do_flatten(node.args[1]);
        }
    })(node);
    var comparisons = [];
    for (var i = 0; i < conjuncts.length; i++) {
        comparisons.push("(" + conjuncts[i].join(" ") + ")");
    }
    if (comparisons.length == 0) {
        return "TRUE";
    } else if (comparisons.length == 1) {
        return comparisons[0];
    } else {
        return "(and " + comparisons.join(" ") + ")";
    }
}

function dump_tree(tree, names) {
    function rec(node, bound) {
        switch(node.type) {
        case "ConstantNode":
            node.res = "" + node.value;
            return node;
        case "FunctionNode":
            node.args = node.args.map(function(n) { return rec(n, bound) });
            node.name = SECRETFUNCTIONS[node.name] || node.name;
            node.res = "(" + node.name + " " + extract(node.args).join(" ") + ")";
            return node;
        case "OperatorNode":
            node.args = node.args.map(function(n) { return rec(n, bound) });
            node.op = SECRETFUNCTIONS[node.op] || node.op;
            if (is_comparison(node.op)) {
                node.res = flatten_comparisons(node);
            } else {
                node.res = "(" + node.op + " " + extract(node.args).join(" ") + ")";
            }
            return node;
        case "SymbolNode":
            if (!CONSTANTS[node.name] && bound.indexOf(node.name) == -1)
                names.push(node.name);
            node.res = node.name;
            return node;
        case "ConditionalNode":
            node.condition = rec(node.condition, bound);
            node.trueExpr  = rec(node.trueExpr, bound);
            node.falseExpr = rec(node.falseExpr, bound);
            node.res = "(if " + node.condition.res + 
                       " " + node.trueExpr.res + 
                       " " + node.falseExpr.res + ")";
            return node;
        case "BlockNode":
            str = "";
            for (var i = 0; i < node.blocks.length - 1; i++) {
                stmt = node.blocks[i].node;
                if (stmt.type != "AssignmentNode")
                    throw SyntaxError("Expected an assignment statement before a semicolon: " + stmt);

                rec(stmt.expr, bound);
                str += ("(let ((" + stmt.name + " " + stmt.expr.res + ")) ");

                if (bound.indexOf(stmt.name) == -1)
                    bound.push(stmt.name);
            }

            rec(node.blocks[node.blocks.length - 1].node, bound);
            node.res = str + node.blocks[node.blocks.length - 1].node.res + ")".repeat(node.blocks.length - 1);
            return node;
        default:
            throw SyntaxError("Invalid tree! " + node);
        }
    }

    rec(tree, []);
    return tree.res;
}

function get_unused_var_warnings(tree) {
    let unused = [];
    bottom_up(tree, function(node) {
        switch(node.type) {
        case "ConstantNode":
            return new Set();
        case "FunctionNode":
        case "OperatorNode":
            used = new Set();
            extract(node.args).forEach(function(s) {
                s.forEach(function(n) { used.add(n); });
            })
            return used;
        case "SymbolNode":
            if (!CONSTANTS[node.name])
                return new Set([node.name]);
            else
                return new Set();
        case "ConditionalNode":
            usedCond = node.condition.res;
            usedTrue  = node.trueExpr.res;
            usedFalse = node.falseExpr.res;
            return new Set([...usedCond, ...usedTrue, ...usedFalse])
        case "BlockNode":
            bound = [];
            usedInAssigns = [];
            for (var i = 0; i < node.blocks.length - 1; i++) {
                stmt = node.blocks[i].node;
                if (stmt.type != "AssignmentNode")
                    throw SyntaxError("Expected an assignment statement before a semicolon: " + stmt);

                bound.push(stmt.name);
                usedInAssigns.push(stmt.expr.res);
            }

            // Assume each assignment is of the form:
            //  <assign> ::= <name> = <val>; <body>.
            // Then
            //  (i)  <name> is unused if <name> is not in Used(<body>),
            //  (ii) Used(<expr>) = Used(<val>) U (Used(<body>) \ { <name> })
            // Clearly, the assumption is slightly wrong, but this
            // tells us we just walk backwards checking condition (i)
            // and updating the used set with (ii).
            used = node.blocks[node.blocks.length - 1].node.res;
            for (var i = node.blocks.length - 2; i >= 0; i--) {
                if (!used.has(bound[i]))
                    unused.push("UnboundVariable: " + bound[i]);
                used.delete(bound[i]);
                used = new Set([...used, ...usedInAssigns[i]]);
            }

            return used
        default:
            throw SyntaxError("Invalid tree!");
        }
    });

    return unused;
}

function get_errors() {
    var tree, errors = [];
    let parse_fail = false
    for (var i = 0; i < arguments.length; i++) {
        try {
            tree = math.parse(arguments[i][0]);
            errors = errors.concat(tree_errors(tree, arguments[i][1]));
        } catch (e) {
            parse_fail = true
            errors.push("" + e);
        }
    }
    if (!parse_fail) {
        const input = document.querySelector("[name=formula-math]")
        errors = [...errors, ...get_varnames_mathjs(input.value).map(varname => get_input_range_errors(KNOWN_INPUT_RANGES[varname], true).map(s => "RangeError: " + varname + ": " + s)).flat()]
    }
    return errors;
}

function get_warnings() {
    try {
        const input = document.querySelector("[name=formula-math]")
        rangeWarnings = get_varnames_mathjs(input.value).map(varname => get_input_range_warnings(KNOWN_INPUT_RANGES[varname]).map(s => "RangeWarning: " + varname + ": " + s)).flat();
        unusedWarnings = get_unused_var_warnings(math.parse(input.value))
        return rangeWarnings.concat(unusedWarnings);
    } catch (e) {
        return []
    }
}

function check_errors() {
    var input = document.querySelector("[name=formula-math]");
    
    var errors = get_errors([input.value, "real"]);
    var warnings = get_warnings();

    document.getElementById("errors").innerHTML = errors.length == 0 || !input.value ? "" : "<li>" + errors.join("</li><li>") + "</li>";
    document.getElementById("warnings").innerHTML = warnings.length == 0 || !input.value ? "" : "<li>" + warnings.join("</li><li>") + "</li>";
    
    if (!input.value) {
        return false;
    } else if (errors.length > 0) {
        return false;
    } else {
        return true;
    }
}

function hide_extra_fields() {
    var $extra = document.querySelector("#formula .extra-fields");
    var inputs = $extra.querySelectorAll("input, select");
    for (var i = 0; i < inputs.length; i++) {
        if (inputs[i].tagName == "INPUT" && inputs[i].value) return;
        if (inputs[i].tagName == "SELECT" && inputs[i].selectedIndex) return;
    }
    var $a = document.createElement("a");
    $a.textContent = "Advanced options »";
    $a.classList.add("show-extra");
    $extra.parentNode.insertBefore($a, $extra.nextSibling);
    $a.addEventListener("click", function() {
        $extra.style.display = "block";
        $a.style.display = "none";
    });
}

var STATE = null;

function Form(form) {
    this.form = form;

    this.fpcore = form.querySelector("[name=formula]");
    this.math = form.querySelector("[name=formula-math]");
    this.extra_fields = form.querySelector(".extra-fields");

    this.math_doc = document.getElementById("mathjs-instructions");
    this.lisp_doc = document.getElementById("lisp-instructions");

    this.input_ranges = document.getElementById("input-ranges");
    this.extra_links = form.querySelector(".extra-links");
    this.button = form.querySelector("[type=submit]")
}

function get_precondition_from_input_ranges(formula) {
    const checks = get_varnames_mathjs(formula)
    .map(name => ([name, KNOWN_INPUT_RANGES[name]]))
    .filter(([_, range]) => get_input_range_errors(range).length == 0)
    .map(([name, [start, end]]) => `(<= ${start} ${name} ${end})`)
    .join(' ')
    return checks.length == 0 ? "" : `(and ${checks})`
}

function setup_state(state, form) {
    window.STATE = state;
    form.fpcore.removeAttribute("disabled");
    form.math.removeAttribute("disabled");

    document.querySelector('#use-fpcore').onclick = function(evt) {
        if (form.math.value) {
            if (!check_errors()) {
                alert("Please fix all errors before attempting to use FPCore input.")
                return evt.preventDefault();
            }
            var fpcore = dump_fpcore(form.math.value)
            form.fpcore.value = fpcore;
        }
        setup_state("fpcore", form);
    }
    document.querySelector('#show-example').onclick = function (evt) {
        form.math.value = "sqrt(x + 1) - sqrt(x)"
        CHECK_ERRORS_AND_DRAW_RANGES()
        document.querySelector('#x_low').value = "0";
        document.querySelector('#x_high').value = "1.79e308";
        window.KNOWN_INPUT_RANGES['x'] = [0, 1.79e308]
        update_run_button_mathjs(form)
    }

    if (state == "math") {
        form.fpcore.style.display = "none";
        form.math.style.display = "block";
        form.input_ranges.style.display = "table";
        document.querySelector('#options').style.display = 'block';
        document.querySelector("#lisp-instructions").style.display = "none";
        document.querySelector("#mathjs-instructions").style.display = "block";
        update_run_button_mathjs(form)
    } else {
        document.querySelector('#options').style.display = 'none';
        form.fpcore.style.display = "block";
        form.math.style.display = "none";
        form.input_ranges.style.display = "none";
        document.querySelector("#lisp-instructions").style.display = "block";
        document.querySelector("#mathjs-instructions").style.display = "none";
        form.button.classList.remove("hidden");
        form.button.removeAttribute("disabled");
    }
}

function get_varnames_mathjs(mathjs_text) {
    const names = []
    dump_tree(math.parse(mathjs_text), names)
    var dnames = [];
    for (var i = 0; i < names.length; i++) {
        if (dnames.indexOf(names[i]) === -1) dnames.push(names[i]);
    }
    return dnames
}

function update_run_button_mathjs(form) {
    function no_range_errors([low, high] = [undefined, undefined]) {
        return low !== '' && high !== '' && !isNaN(Number(low)) && !isNaN(Number(high)) && Number(low) <= Number(high) 
    }
    const button = document.querySelector('#run_herbie')
    let varnames;
    try {
        varnames = get_varnames_mathjs(form.math.value)
    } catch (e) {
        console.log("Couldn't get varnames:", e)
        button.setAttribute('disabled', 'true')
        return;
    }
    if (form.math.value && varnames.every(varname => no_range_errors(KNOWN_INPUT_RANGES[varname]))) {
        button.removeAttribute('disabled')
    } else {
        console.log('There are still range errors.', )
        button.setAttribute('disabled', 'true')
    }
}

function get_input_range_errors([low, high] = [undefined, undefined], empty_if_missing=false) {
    if ((low === undefined || low === '') || (high === undefined || high === '')) return empty_if_missing ? [] : ['input missing']
    const A = []
    if (!(low === undefined || low === '') && isNaN(Number(low))) {
        A.push(`The start of the range (${low}) is not a number.`)
    } else if (!Number.isFinite(Number(low))) {
        A.push(`The start of the range (${low}) is outside the floating point range.`)
    }

    if (!(high === undefined || high === '') && isNaN(Number(high))) {
        A.push(`The end of the range (${high}) is not a number.`)
    } else if (!Number.isFinite(Number(high))) {
        A.push(`The end of the range (${high}) is outside the floating point range.`)
    }

    if (Number(low) > Number(high)) A.push(`The start of the range is higher than the end.`)
    
    return A
}

function get_input_range_warnings([low, high] = [undefined, undefined]) {
    if ((low === undefined || low === '') || (high === undefined || high === '')) return []
    const A = []
    if (Number(low) == Number(high)) A.push(`This is a single point.`)
    return A
}

function onload() {

    // Only records ranges the user intentionally set.
    window.KNOWN_INPUT_RANGES = { /* "x" : [-1, 1] */ }

    function hide(selector) { document.querySelector(selector).style.display = 'none' }
    hide('#formula textarea')

    var form = new Form(document.getElementById("formula"));

    /* STATE represents whether we are working with the mathjs + precondition inputs or the fpcore input. */
    const params = new URLSearchParams(window.location.search);
    if (params.get('fpcore')) {
        form.fpcore.value = params.get('fpcore');
        STATE = "fpcore";
    }
    else if (form.math.value) STATE = "math";
    else if (form.fpcore.value) STATE = "fpcore";
    else STATE = "math";

    setup_state(STATE, form);

    function html(string) {
        const t = document.createElement('template');
        t.innerHTML = string;
        return t.content;
    }

    function range_inputs(varname) {
        const [low, high] = KNOWN_INPUT_RANGES[varname] || [undefined, undefined]
        
        const low_id = `${varname}_low`
        const high_id = `${varname}_high`
        const default_options = ['-1.79e308', '-1e9', '-1e3', '-1', '0', '1', '1e3', '1e9', '1.79e308']
        function input_view(id, value, placeholder) {
            return `
                <div id="${id}_dropdown" class="dropdown">
                    <input id="${id}" class="input-range" autocomplete="off" placeholder="${placeholder}" ${value ? `value="${value}"` : ``}>
                    <div class="dropdown-content">
                        ${default_options.map(s => `<div>${s}</div>`).join('')}
                    </div>
                </div>
            `
        }
        const error_msgs = get_input_range_errors(KNOWN_INPUT_RANGES[varname], true)
        const warning_msgs = get_warnings(KNOWN_INPUT_RANGES[varname])
        const view = html(`
                <tr id="${varname}_input" class="input-range-view">
                <td class="varname">
                    ${varname}:
                </td>
                <td>${input_view(low_id, low, '-1e3')}</td> <td>to</td> <td>${input_view(high_id, high, '1e3')}</td>
                </tr>`)

        const low_el = view.querySelector(`#${low_id}`)
        


        function show_errors(root=document) {
            root.querySelectorAll(`#${varname}_input input`).forEach(input => {
                input.style['background-color'] = input.value === '' ? '#a6ffff3d' : 'initial'
            })
        }
        function set_input_range({ low, high }) {
            if (low !== undefined) low_el.value = low
            if (high !== undefined) high_el.value = high
            if (!KNOWN_INPUT_RANGES[varname]) { KNOWN_INPUT_RANGES[varname] = [undefined, undefined] }
            const [old_low, old_high] = KNOWN_INPUT_RANGES[varname]
            KNOWN_INPUT_RANGES[varname] = [low ?? old_low, high ?? old_high]
            check_errors()
            show_errors()
            update_run_button_mathjs(form)
        }
        low_el.addEventListener('input', () => set_input_range({ low: low_el.value }))
        view.querySelectorAll(`#${low_id}_dropdown .dropdown-content div`).forEach((e, i) => {
            const handler = () => set_input_range({ low: e.textContent })
            e.addEventListener('click', handler)
            e.addEventListener('pointerdown', handler)
        })
        
        const high_el = view.querySelector(`#${high_id}`)
        high_el.addEventListener('input', () => set_input_range({ high: high_el.value }))
        view.querySelectorAll(`#${high_id}_dropdown .dropdown-content div`).forEach((e, i) => {
            const handler = () => set_input_range({ high: e.textContent })
            e.addEventListener('click', handler)
            e.addEventListener('pointerdown', handler)
        })

        check_errors()
        show_errors(view)

        return view
    }

    const range_div = document.createElement('DIV')
    document.querySelector('#formula').appendChild(range_div)

    

    
    update_run_button_mathjs(form)

    let current_timeout = undefined  // can be used to debounce the input box
    function check_errors_and_draw_ranges() {
        if (form.math.value === "") document.querySelector('#input-ranges').innerHTML=''
        let varnames;
        try {
            varnames = get_varnames_mathjs(form.math.value)
        } catch (e) {
            check_errors()
            return
        }
        const range_div = document.querySelector('#input-ranges')
        range_div.replaceChildren(...varnames.map(range_inputs))
    }
    CHECK_ERRORS_AND_DRAW_RANGES = check_errors_and_draw_ranges // HACK
    check_errors_and_draw_ranges()
    
    form.math.addEventListener("input", function () {
        clearTimeout(current_timeout)
        current_timeout = setTimeout(check_errors_and_draw_ranges, 400)
        update_run_button_mathjs(form)
    })
    form.math.setAttribute('autocomplete', 'off')  // (because it hides the error output)

    form.form.addEventListener("submit", function(evt) {
        var fpcore;
        if (STATE != "fpcore") {
            if (!check_errors()) return evt.preventDefault();
            fpcore = dump_fpcore(form.math.value)
        } else {
            fpcore = form.fpcore.value;
        }
        form.fpcore.value = fpcore;
        console.log(STATE, fpcore);

        var url = document.getElementById("formula").getAttribute("data-progress");
        if (url) {
            form.math.disabled = "true";
            form.fpcore.disabled = "true";
            ajax_submit(url, fpcore);
            evt.preventDefault();
            return false;
        } else {
            return true;
        }
    });
}

function get_progress(loc) {
    var req2 = new XMLHttpRequest();
    req2.open("GET", loc);
    req2.onreadystatechange = function() {
        if (req2.readyState == 4) {
            if (req2.status == 202) {
                document.getElementById("progress").textContent = req2.responseText;
                const nums = document.getElementById("num-jobs");
                if (nums != null) {
                    nums.textContent = req2.getResponseHeader("X-Job-Count");
                }
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

function ajax_submit(url, lisp) {
    document.getElementById("progress").style.display = "block";
    var req = new XMLHttpRequest();
    req.open("POST", url);
    req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
    req.onreadystatechange = function() {
        if (req.readyState == 4) {
            if (req.status == 201) {
                var jobcount = req.getResponseHeader("X-Job-Count");
                var jobelt = document.getElementById("num-jobs")
                if (jobelt) jobelt.innerHTML = Math.max(jobcount - 1, 0);
                var loc = req.getResponseHeader("Location");
                get_progress(loc);
            } else {
                document.getElementById("errors").innerHTML = req.responseText;
            }
        }
    }
    var content = "formula=" + encodeURIComponent(lisp);
    req.send(content);
}

window.addEventListener("load", onload);
