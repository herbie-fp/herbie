let style = `
.loaded .load-text { display: none; }
.load-text { font-size: 140%; color: #888; }
a { color: #2A6496; text-decoration: none; cursor: pointer }
a:hover {text-decoration: underline; color: #295785}
a.delete { color: currentColor; }
a.delete:hover { color: #ed2b00; text-decoration: line-through; }
#profile input {
    width: 100%; padding: .5ex 1ex; box-sizing: border-box;
    border: none; border-bottom: 1px solid #ddd; font-size: 150%;
}
#profile .profile-row { margin: 1em 0; }
.profile-row div { display: flex; }
#profile .edge { color: #888; margin-left: 2em; }
#profile .node { font-size: 120%; }
#profile .path, #profile .name { font-family: monospace; white-space: nowrap; }
#profile .path { flex-grow: 1; overflow: hidden; text-overflow: ellipsis; }
#profile .path:before { content: " ("; }
#profile .path:after { content: ") "; }
.profile-row > div > * { padding: 0 .5em; }
#profile .pct { width: 4em; text-align: right; }
`


function Element(tagname, props, children) {
    if (children === undefined) { children = props; props = {}; }

    var $elt = document.createElement(tagname);
    for (var i in props) if (props.hasOwnProperty(i)) $elt[i] = props[i];

    function addAll(c) {
        if (!c) return;
        else if (Array.isArray(c)) c.map(addAll);
        else if (typeof c == "string") $elt.appendChild(document.createTextNode(c))
        else if (c instanceof Node) $elt.appendChild(c);
        else {
            console.error("Not an element: ", c);
            throw "Invalid element!"
        }
    }
    addAll(children);
    return $elt;
}

class Profile {
    constructor(elt) {
        this.elt = elt;
        var text = elt.querySelector(".load-text");
        this.promise = fetch("profile.json")
            .then(response => response.json())
            .catch(function(error) {
                text.textContent = "Error loading profile data"
            })
            .then(data => this.render(data))
    }

    render(json) {
        // Load the CSS
        document.documentElement.appendChild(Element("style", style));
        this.json = json;
        this.search = Element("input", {
            placeholder: "Search for a function...",
            autocomplete: "off",
            name: "profilefn",
        }, []);
        this.search.setAttribute("list", "profilefns");
        var form = Element("form", { method: "GET", action: "" }, [
            this.search,
            Element("datalist", { id: "profilefns" }, [
                json.nodes.map(n => n.id && Element("option", n.id))
            ]),
        ]);
        form.addEventListener("submit", (e) => this.doSearch(e));
        this.elt.appendChild(form);
        this.elt.appendChild(this.mkNode(json.nodes[json.nodes[0].callees[0].callee]));
        this.elt.classList.add("loaded");
    }

    mkNode(node) {
        var nelt = Element("div", { className: "node" }, [
            Element("a", { className: "name delete" }, node.id || "???"),
            Element("span", { className: "path" }, path(node.src)),
            Element("span", {
                className: "pct",
                title: "Self-time: " + pct(node.self, this.json.cpu_time) }, [
                    time(node.total),
                ]),
        ]);
        var elt = Element("div", { className: "profile-row" }, [
            node.callers.sort((e1, e2) => e1.caller_time - e2.caller_time).map((edge) => {
                var other = this.json.nodes[edge.caller];
                elt = Element("div", { className: "edge" }, [
                    Element("a", { className: "name" }, other.id || "???"),
                    Element("span", { className: "path" }, path(other.src)),
                    Element("span", { className: "pct" }, pct(edge.caller_time, node.total)),
                ]);
                elt.children[0].addEventListener("click", this.addElt(other));
                return elt;
            }),
            nelt,
            node.callees.sort((e1, e2) => e2.callee_time - e1.callee_time).map((edge) => {
                var other = this.json.nodes[edge.callee];
                elt = Element("div", { className: "edge" }, [
                    Element("a", { className: "name" }, other.id || "???"),
                    Element("span", { className: "path" }, path(other.src)),
                    Element("span", { className: "pct" }, pct(edge.callee_time, node.total)),
                ]);
                elt.children[0].addEventListener("click", this.addElt(other));
                return elt;
            }),
        ]);
        nelt.children[0].addEventListener("click", () => elt.remove());
        return elt;
    }

    addElt(other) {
        return () => {
            var newelt = this.mkNode(other)
            this.elt.appendChild(newelt);
            newelt.scrollTo();
            return newelt;
        }
    }

    doSearch(e) {
        e.preventDefault();
        var term = this.search.value;
        var elt = this.addElt(this.json.nodes.find(n => n.id == term))();
        elt.scrollTo();
        this.search.value = "";
        return false;
    }
}

function pct(val, base) {
    return Math.floor(val/base * 10000) / 100 + "%";
}

function time(s) {
    return Math.floor(s / 1000 * 100) / 100 + "s";
}

function path(p) {
    if (!p) {
        return "???";
    } else if (p[0] == "/") {
        var r = p.substr(p.toLowerCase().indexOf("/racket") + 1);
        var ds = r.split("/");
        if (ds[1] == "share" && ds[2] == "pkgs") {
            return "/" + ds.slice(3).join("/");
        } else if (ds[1] == "collects") {
            return "/" + ds.slice(2).join("/");
        } else {
            return "/" + ds.join("/");
        }
    } else {
        return p;
    }
}

document.addEventListener("DOMContentLoaded", () => {
    let profile = document.querySelector("#profile");
    if (!profile) throw new Error("Could not find #profile element");
    window.PROFILE = new Profile(profile);
})
