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

function dump_fpcore(formula, pre, precision) {
    var tree = math.parse(formula);
    var ptree = math.parse(pre);

    var names = [];
    var body = dump_tree(tree, names);
    var precondition = dump_tree(ptree, names);

    var dnames = [];
    for (var i = 0; i < names.length; i++) {
        if (dnames.indexOf(names[i]) === -1) dnames.push(names[i]);
    }

    var name = formula.replace("\\", "\\\\").replace("\"", "\\\"");
    var fpcore = "(FPCore (" + dnames.join(" ") + ")\n  :name \"" + name + "\"";
    if (pre) fpcore += "\n  :pre " + precondition;
    if (precision) fpcore += "\n  :precision " + precision;

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
    for (var i = 0; i < arguments.length; i++) {
        try {
            tree = math.parse(arguments[i][0]);
            errors = errors.concat(tree_errors(tree, arguments[i][1]));
        } catch (e) {
            errors.push("" + e);
        }
    }
    return errors;
}

function check_errors() {
    var input = document.querySelector("[name=formula-math]");
    var pre = document.querySelector("[name=pre]");
    var errors = get_errors([input.value, "real"], [pre.value || "TRUE", "bool"]);

    if (!input.value) {
        document.getElementById("errors").innerHTML = "";
        return false;
    } else if (errors.length > 0) {
        document.getElementById("errors").innerHTML = "<li>" + errors.join("</li><li>") + "</li>";
        return false;
    } else {
        document.getElementById("errors").innerHTML = "";
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
    //$extra.style.display = "none";
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

function get_precondition_from_input_ranges(form) {
    return get_varnames_mathjs(form.math.value)
        .map(name => ([name, KNOWN_INPUT_RANGES[name]]))
        .filter(([_, range]) => get_input_range_errors(range).length == 0)
        .map(([name, [start, end]]) => `((${start} <= ${name} <= ${end}))`)
        .join(' and ')
}

function setup_state(state, form) {
    window.STATE = state;
    form.fpcore.removeAttribute("disabled");
    form.math.removeAttribute("disabled");

    if (!form.a_fpcore) {
        form.a_fpcore = document.createElement("a");
        form.a_fpcore.textContent = "FPCore input";
        form.a_fpcore.addEventListener("click", function(evt) {
            if (form.math.value) {
                if (!check_errors()) return evt.preventDefault();
                var fpcore = dump_fpcore(form.math.value, get_precondition_from_input_ranges(form), ''/*form.prec.value*/);
                form.fpcore.value = fpcore;
            }
            setup_state("fpcore", form);
        });
        
    }
    form.extra_links.appendChild(form.a_fpcore);

    if (!form.a_mathjs) {
        form.a_mathjs = document.createElement("a");
        form.a_mathjs.textContent = "back to MathJS input";
        form.a_mathjs.addEventListener("click", function(evt) {
            setup_state("math", form);
        });
        
    }
    form.extra_links.appendChild(form.a_mathjs);
    
    if (state == "math") {
        form.fpcore.style.display = "none";
        form.math.style.display = "block";
        form.input_ranges.style.display = "block";
        document.querySelector("#lisp-instructions").style.display = "none";
        document.querySelector("#mathjs-instructions").style.display = "block";
        form.extra_links.removeChild(form.a_mathjs)
        update_run_button_mathjs(form)
    } else {
        form.fpcore.style.display = "block";
        form.math.style.display = "none";
        form.input_ranges.style.display = "none";
        document.querySelector("#lisp-instructions").style.display = "block";
        document.querySelector("#mathjs-instructions").style.display = "none";
        form.extra_links.removeChild(form.a_fpcore)
        form.button.classList.remove("hidden");
        form.button.removeAttribute("disabled");
    }
}

function get_varnames_mathjs(mathjs_text) {
    /* ASK confirm that this should get the variable names */
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

function onload() {

    function hide(selector) { document.querySelector(selector).style.display = 'none' }
    hide('#formula textarea')
    hide('#formula .extra-fields')

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

    // Only records ranges the user intentionally set.
    window.KNOWN_INPUT_RANGES = { /* "x" : [-1, 1] */ }

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
        function input_view(id, value) {
            return `
                <div id="${id}_dropdown" class="dropdown">
                    <input id="${id}" class="input-range" autocomplete="off" placeholder="-1e3" ${value ? `value="${value}"` : ``}>
                    <div class="dropdown-content">
                        ${default_options.map(s => `<div>${s}</div>`).join('')}
                    </div>
                </div>
            `
        }
        const error_msgs = get_input_range_errors(KNOWN_INPUT_RANGES[varname], true)
        const warning_msgs = get_warnings(KNOWN_INPUT_RANGES[varname])
        const view = html(`
            <div id="${varname}_input" class="input-range-view">
                <div>
                <span class="varname"> ${varname}: </span> 
                ${input_view(low_id, low)} to ${input_view(high_id, high)}</div>
                <ul id="errors" style="display: ${error_msgs.length > 0 ? 'block' : 'none'}">${error_msgs.map(msg => `<li>${msg}</li>`).join('')}</ul>
                <ul id="warnings" style="display: ${warning_msgs.length > 0 ? 'block' : 'none'}">${warning_msgs.map(msg => `<li>${msg}</li>`).join('')}</ul>

            </div>`)

        const low_el = view.querySelector(`#${low_id}`)
        
        function get_warnings([low, high] = [undefined, undefined]) {
            if ((low === undefined || low === '') || (high === undefined || high === '')) return []
            const A = []
            if (Number(low) == Number(high)) A.push(`(This is a single point.)`)
            return A
        }

        function show_errors(root=document) {
            const error_msgs = get_input_range_errors(KNOWN_INPUT_RANGES[varname], true)
            root.querySelectorAll(`#${varname}_input input`).forEach(input => {
                input.style['background-color'] = input.value === '' ? '#a6ffff3d' : 'initial'
            })
            const e = root.querySelector(`#${varname}_input #errors`)
            e.innerHTML = error_msgs.map(msg => `<li>${msg}</li>`).join('')
            e.style.display = error_msgs.length > 0 ? 'block' : 'none'
        }
        function show_warnings(root=document) {
            const warning_msgs = get_warnings(KNOWN_INPUT_RANGES[varname])
            const e = root.querySelector(`#${varname}_input #warnings`)
            e.innerHTML = warning_msgs.map(msg => `<li>${msg}</li>`).join('')
            e.style.display = warning_msgs.length > 0 ? 'block' : 'none'
        }
        function set_input_range({ low, high }) {
            if (low !== undefined) low_el.value = low
            if (high !== undefined) high_el.value = high
            if (!KNOWN_INPUT_RANGES[varname]) { KNOWN_INPUT_RANGES[varname] = [undefined, undefined] }
            const [old_low, old_high] = KNOWN_INPUT_RANGES[varname]
            KNOWN_INPUT_RANGES[varname] = [low ?? old_low, high ?? old_high]
            show_errors()
            show_warnings()
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

        show_errors(view)

        return view
    }

    const range_div = document.createElement('DIV')
    document.querySelector('#formula').appendChild(range_div)

    

    
    update_run_button_mathjs(form)

    let current_timeout = undefined  // used to debounce the input box
    function check_errors_and_draw_ranges() {
        if (form.math.value === "") document.querySelector('#input-ranges').innerHTML=''
        if (!check_errors()) { return }
        const varnames = get_varnames_mathjs(form.math.value)
        const range_div = document.querySelector('#input-ranges')
        const range_prompt = varnames.length > 0 ? html(`<div>Please specify the approximate range of each input so that Herbie can optimize for your use case.</div>`) : ``
        range_div.replaceChildren(range_prompt, ...varnames.map(range_inputs))
    }
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
            //fpcore = dump_fpcore(form.math.value, form.pre.value, form.prec.value);
            fpcore = dump_fpcore(form.math.value, get_precondition_from_input_ranges(form), ''/*form.prec.value*/);
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
