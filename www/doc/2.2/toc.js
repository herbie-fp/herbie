function make_toc() {
    var headings = document.querySelectorAll("h2");
    var toc = document.createElement("nav");
    toc.classList.add("toc")
    var list = document.createElement("ul");
    for (var i = 0; i < headings.length; i++) {
        var li = document.createElement("li");
        var a = document.createElement("a");
        var h = headings[i];
        if (! h.id) {
            h.setAttribute("id", "heading-" + i);
        }
        a.setAttribute("href", "#" + h.id);
        a.innerHTML = h.innerHTML;
        li.appendChild(a);
        list.appendChild(li);
    }
    toc.appendChild(list);
    headings[0].parentNode.insertBefore(toc, headings[0]);
}

window.addEventListener("load", make_toc);
