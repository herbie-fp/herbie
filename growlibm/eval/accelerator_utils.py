import re
from pathlib import Path


def default_platform_path(platform):
    return Path(__file__).resolve().parents[1] / "platforms" / f"{platform.lower()}.rkt"


def load_accelerator_names(platform_path):
    text = platform_path.read_text(encoding="utf-8")
    matches = re.findall(r"from-accelerators\s+'([A-Za-z0-9_]+)", text)
    seen = set()
    names = []
    for name in matches:
        if name not in seen:
            seen.add(name)
            names.append(name)
    return names


def alternative_exprs(test):
    seen = set()
    exprs = []

    def add_expr(expr):
        if expr in (None, "", "()", False):
            return
        expr_text = str(expr)
        if expr_text in seen:
            return
        seen.add(expr_text)
        exprs.append(expr_text)

    add_expr(test.get("output"))

    raw = test.get("cost-accuracy")
    if isinstance(raw, list) and len(raw) >= 2:
        best = raw[1]
        if isinstance(best, list) and len(best) >= 3:
            add_expr(best[2])

        rest = raw[2] if len(raw) > 2 else []
        if isinstance(rest, list):
            for point in rest:
                if isinstance(point, list) and len(point) >= 3:
                    add_expr(point[2])

    return exprs


def accelerator_hits(test, accelerator_names):
    exprs = alternative_exprs(test)
    found = []
    for name in accelerator_names:
        pattern = rf"(?<![A-Za-z0-9_]){re.escape(name)}(?:\.f(?:32|64)\b|\b)"
        if any(re.search(pattern, expr) for expr in exprs):
            found.append(name)
    return found
