function tr_click() {
    this.querySelector("td a").click();
}

function load() {
    var trs = document.querySelectorAll("tbody tr");
    for (var i = 0; i < trs.length; i++) {
        trs[i].addEventListener("click", tr_click);
    }
}

window.addEventListener("load", load);
