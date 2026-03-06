#!/usr/bin/env python3

import argparse
import json
import sys
from pathlib import Path

FRONTIER_START = "<!-- frontier-plot-start -->"
FRONTIER_END = "<!-- frontier-plot-end -->"


def warn(message):
    print(f"warning: {message}", file=sys.stderr)


def load_json(path):
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def parse_point(raw):
    if not isinstance(raw, list) or len(raw) < 2:
        return None

    x_value, y_value = raw[0], raw[1]
    if x_value == "N/A":
        return None

    try:
        return (float(x_value), float(y_value))
    except (TypeError, ValueError):
        return None


def parse_frontier(results_path):
    if not results_path.exists():
        warn(f"missing {results_path}")
        return (None, [])

    data = load_json(results_path)
    merged = data.get("merged-cost-accuracy")
    if not isinstance(merged, list) or len(merged) < 2:
        warn(f"missing merged-cost-accuracy in {results_path}")
        return (None, [])

    initial = parse_point(merged[0])
    frontier = []
    if isinstance(merged[1], list):
        for raw_point in merged[1]:
            point = parse_point(raw_point)
            if point is not None:
                frontier.append(point)

    frontier.sort(key=lambda point: point[0])
    return (initial, frontier)


def plot_frontier(series, image_path, platform):
    try:
        import matplotlib
        from matplotlib.ticker import FuncFormatter
    except ImportError as err:
        raise SystemExit(
            "matplotlib is required for growlibm/evaluate-frontier.py; "
            "install it with `python3 -m pip install matplotlib`."
        ) from err

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(8, 6))
    have_points = False

    for one_series in series:
        initial, frontier = parse_frontier(one_series["results_path"])

        if frontier:
            have_points = True
            xs = [point[0] for point in frontier]
            ys = [point[1] for point in frontier]
            ax.scatter(xs, ys, s=26, color=one_series["color"], label=one_series["label"])
            ax.plot(xs, ys, color=one_series["color"], linewidth=1.4, alpha=0.35)

        if initial is not None:
            have_points = True
            ax.scatter(
                [initial[0]],
                [initial[1]],
                s=64,
                marker="s",
                facecolors="white",
                edgecolors=one_series["color"],
                linewidths=1.8,
            )

    if not have_points:
        ax.text(0.5, 0.5, "No frontier data found", ha="center", va="center", transform=ax.transAxes)

    ax.set_title(f"Speed/Accuracy Frontier ({platform})")
    ax.set_xlabel("Speedup (x)")
    ax.set_ylabel("Accuracy")
    ax.set_ylim(0.0, 1.0)
    ax.xaxis.set_major_formatter(FuncFormatter(lambda value, _: f"{value:.2g}x"))
    ax.yaxis.set_major_formatter(FuncFormatter(lambda value, _: f"{100 * value:.0f}%"))
    ax.grid(True, linewidth=0.5, alpha=0.2)
    if have_points:
        ax.legend(loc="lower right")

    fig.tight_layout()
    fig.savefig(image_path, dpi=180)
    plt.close(fig)


def make_frontier_block(image_name, platform):
    return (
        f"\n{FRONTIER_START}\n"
        '<section id="frontier-comparison">\n'
        "  <h2>Speed/Accuracy Frontier</h2>\n"
        "  <p>\n"
        f'    <img src="{image_name}" alt="Pareto frontier for c platform, no accelerators, and {platform} platform" '
        'style="max-width:100%; height:auto;">\n'
        "  </p>\n"
        "</section>\n"
        f"{FRONTIER_END}\n"
    )


def inject_frontier_html(index_path, image_name, platform):
    if not index_path.exists():
        warn(f"missing {index_path}, skipping html injection")
        return

    html = index_path.read_text(encoding="utf-8")
    block = make_frontier_block(image_name, platform)

    if FRONTIER_START in html and FRONTIER_END in html:
        before, rest = html.split(FRONTIER_START, 1)
        _, after = rest.split(FRONTIER_END, 1)
        updated = before + block + after
    elif "</body>" in html:
        updated = html.replace("</body>", f"{block}</body>", 1)
    else:
        updated = html + block

    if updated != html:
        index_path.write_text(updated, encoding="utf-8")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("reports_path", help="Path containing evaluate.sh report folders")
    parser.add_argument("platform", help="Name of growlibm platform under test")
    parser.add_argument("--output", default="frontier.png", help="Output image filename")
    args = parser.parse_args()

    reports_path = Path(args.reports_path)
    image_path = reports_path / args.output

    series = [
        {
            "label": "no accelerators",
            "results_path": reports_path / "vanilla_base" / "results.json",
            "color": "#ff7f0e",
        },
        {
            "label": f"{args.platform} platform",
            "results_path": reports_path / "growlibm_base" / "results.json",
            "color": "#2ca02c",
        },
    ]

    plot_frontier(series, image_path, args.platform)
    inject_frontier_html(reports_path / "index.html", image_path.name, args.platform)


if __name__ == "__main__":
    main()
