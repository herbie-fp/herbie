CONSTANTS = {"PI": "real", "E": "real", "TRUE": "bool", "FALSE": "bool"}

FUNCTIONS = {}

"+ - * / pow copysign fdim fmin fmax fmod hypot remainder".split(" ").forEach(function(op) {
    FUNCTIONS[op] = [["real", "real"], "real"];
});
("fabs sqrt exp log sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh " +
 "cbrt ceil erf erfc exp2 expm1 floor j0 j1 lgamma log10 log1p log2 logb rint " + 
 "round tgamma trunc y0 y1").split(" ").forEach(function(op) {
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
    }
    tree.res = cb(tree);
    return tree;
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

function extract(args) {return args.map(function(n) {return n.res});}

function dump_tree(tree, names) {
    return bottom_up(tree, function(node) {
        switch(node.type) {
        case "ConstantNode":
            return "" + node.value;
        case "FunctionNode":
            node.name = SECRETFUNCTIONS[node.name] || node.name;
            return "(" + node.name + " " + extract(node.args).join(" ") + ")";
        case "OperatorNode":
            node.op = SECRETFUNCTIONS[node.op] || node.op;
            if (is_comparison(node.op)) {
                return flatten_comparisons(node);
            } else {
                return "(" + node.op + " " + extract(node.args).join(" ") + ")";
            }
        case "SymbolNode":
            if (!CONSTANTS[node.name])
                names.push(node.name);
            return node.name;
        case "ConditionalNode":
            return "(if " + node.condition.res + 
                " " + node.trueExpr.res + 
                " " + node.falseExpr.res + ")";
        default:
            throw SyntaxError("Invalid tree!");
        }
    }).res;
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
        return get_varnames_mathjs(input.value).map(varname => get_input_range_warnings(KNOWN_INPUT_RANGES[varname]).map(s => "RangeWarning: " + varname + ": " + s)).flat()
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
    return checks.length == 0 ? "" : `(and ${checks}`
}

function setup_state(state, form) {
    window.STATE = state;
    form.fpcore.removeAttribute("disabled");
    form.math.removeAttribute("disabled");

    document.querySelector('#use-fpcore-input a').onclick = function(evt) {
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

    if (state == "math") {
        form.fpcore.style.display = "none";
        form.math.style.display = "block";
        form.input_ranges.style.display = "table";
        document.querySelector('#use-fpcore-input').style.display = 'block';
        document.querySelector("#lisp-instructions").style.display = "none";
        document.querySelector("#mathjs-instructions").style.display = "block";
        update_run_button_mathjs(form)
    } else {
        document.querySelector('#use-fpcore-input').style.display = 'none';
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
        button.setAttribute('disabled', 'true')
        return;
    }
    if (form.math.value && varnames.every(varname => no_range_errors(KNOWN_INPUT_RANGES[varname]))) {
        button.removeAttribute('disabled')
    } else {
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
    check_errors_and_draw_ranges()
    
    form.math.addEventListener("input", function () {
        clearTimeout(current_timeout)
        current_timeout = setTimeout(check_errors_and_draw_ranges, 0)  // turn off debouncing
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

function clean_progress(str) {
    var lines = str.split("\n");
    var outlines = [];
    for (var i = 0; i < lines.length; i++) {
        var line = lines[i];
        var words = line.split(": ");
        var word0 = words.shift();
        outlines.push(words.join(": "));
    }
    return outlines.join("\n");
}

function get_progress(loc) {
    var req2 = new XMLHttpRequest();
    req2.open("GET", loc);
    req2.onreadystatechange = function() {
        if (req2.readyState == 4) {
            if (req2.status == 202) {
                document.getElementById("progress").textContent = clean_progress(req2.responseText);
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
