#!/usr/bin/env python3

import argparse
import html
import json
import re
import sys
import textwrap
from pathlib import Path

PAGE_START = "<!-- benchmark-frontiers-start -->"
PAGE_END = "<!-- benchmark-frontiers-end -->"


def warn(message):
    print(f"warning: {message}", file=sys.stderr)


def load_json(path):
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


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


def benchmark_key(test):
    identifier = test.get("identifier")
    if identifier not in (None, "", "#f"):
        return str(identifier)
    return str(test.get("name") or test.get("link"))


def load_tests(results_path):
    if not results_path.exists():
        warn(f"missing {results_path}")
        return {}

    data = load_json(results_path)
    tests = {}
    for test in data.get("tests", []):
        tests[benchmark_key(test)] = test
    return tests


def parse_cost_accuracy(test):
    raw = test.get("cost-accuracy")
    if not isinstance(raw, list) or len(raw) < 2:
        return (None, [])

    initial = parse_point(raw[0])
    points = []

    best = parse_point(raw[1])
    if best is not None:
        points.append(best)

    if len(raw) > 2 and isinstance(raw[2], list):
        for point in raw[2]:
            parsed = parse_point(point)
            if parsed is not None:
                points.append(parsed)

    return (initial, points)


def parse_point(raw):
    if not isinstance(raw, list) or len(raw) < 2:
        return None

    try:
        return (float(raw[0]), float(raw[1]))
    except (TypeError, ValueError):
        return None


def pareto_frontier(points):
    frontier = []
    best_error = float("inf")
    for cost, error in sorted(points, key=lambda point: (point[0], point[1])):
        if error < best_error:
            frontier.append((cost, error))
            best_error = error
    return list(reversed(frontier))


def accelerator_hits(test, accelerator_names):
    text = " ".join(str(test.get(field, "")) for field in ("output", "target-prog"))
    found = []
    for name in accelerator_names:
        pattern = rf"(?<![A-Za-z0-9_]){re.escape(name)}(?:\.f(?:32|64)\b|\b)"
        if re.search(pattern, text):
            found.append(name)
    return found


def sanitize_filename(name):
    return re.sub(r"[^A-Za-z0-9._-]+", "_", name).strip("._") or "benchmark"


def format_accelerator_summary(accelerators):
    if accelerators:
        return f"Accelerators: yes ({', '.join(accelerators)})"
    return "Accelerators: no"


def plot_benchmark(series, image_path, title, accelerator_summary):
    try:
        import matplotlib
        from matplotlib.ticker import FuncFormatter
    except ImportError as err:
        raise SystemExit(
            "matplotlib is required for growlibm/evaluate-benchmark-frontiers.py; "
            "install it with `python3 -m pip install matplotlib`."
        ) from err

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(8, 4.8))
    have_points = False

    for entry in series:
        initial, points = parse_cost_accuracy(entry["test"])
        bits = entry["bits"]
        if initial is None or initial[0] == 0 or bits <= 0:
            continue

        frontier = pareto_frontier(points)
        if frontier:
            have_points = True
            ax.plot(
                [initial[0] / point[0] for point in frontier if point[0]],
                [1 - (point[1] / bits) for point in frontier if point[0]],
                color=entry["color"],
                linewidth=1.5,
                alpha=0.4,
            )
            ax.scatter(
                [initial[0] / point[0] for point in frontier if point[0]],
                [1 - (point[1] / bits) for point in frontier if point[0]],
                s=30,
                color=entry["color"],
                alpha=0.85,
                label=entry["label"],
            )

        have_points = True
        ax.scatter(
            [1.0],
            [1 - (initial[1] / bits)],
            s=64,
            marker="s",
            facecolors="white",
            edgecolors=entry["color"],
            linewidths=1.8,
        )

    if not have_points:
        ax.text(0.5, 0.5, "No frontier data found", ha="center", va="center", transform=ax.transAxes)

    ax.set_title(
        "Per-Benchmark Speed/Accuracy Frontier: "
        f"{title}\n{textwrap.fill(accelerator_summary, width=70)}"
    )
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


def benchmark_section(benchmark):
    title = html.escape(benchmark["title"])
    grow_link = html.escape(benchmark["growlibm_link"])
    vanilla_link = html.escape(benchmark["vanilla_link"])
    image = html.escape(benchmark["image"])
    accelerators = benchmark["accelerators"]
    summary = format_accelerator_summary(accelerators).replace("Accelerators:", "Uses accelerators:")

    return (
        '<section class="benchmark-frontier">\n'
        f"  <h2>{title}</h2>\n"
        f"  <p>{html.escape(summary)}</p>\n"
        '  <p><a href="'
        f"{grow_link}"
        '">growlibm_base</a> | <a href="'
        f"{vanilla_link}"
        '">vanilla_base</a></p>\n'
        f'  <p><img src="{image}" alt="Pareto frontier for {title}" loading="lazy"></p>\n'
        "</section>\n"
    )


def write_page(page_path, platform, benchmarks):
    sections = "".join(benchmark_section(benchmark) for benchmark in benchmarks)
    html_text = f"""<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>{html.escape(platform)} benchmark frontiers</title>
  <style>
    body {{
      font-family: sans-serif;
      margin: 2rem auto;
      max-width: 1100px;
      padding: 0 1rem 2rem;
      line-height: 1.4;
    }}
    .benchmark-frontier {{
      border-top: 1px solid #ccc;
      padding-top: 1.5rem;
      margin-top: 1.5rem;
    }}
    img {{
      max-width: 100%;
      height: auto;
    }}
  </style>
</head>
<body>
  <h1>{html.escape(platform)} per-benchmark frontiers</h1>
  <p>Comparing <code>growlibm_base</code> against <code>vanilla_base</code>.</p>
  {sections}
</body>
</html>
"""
    page_path.write_text(html_text, encoding="utf-8")


def inject_page_link(index_path, page_name):
    if not index_path.exists():
        warn(f"missing {index_path}, skipping html injection")
        return

    block = (
        f"\n{PAGE_START}\n"
        '<section id="benchmark-frontiers">\n'
        "  <h2>Per-Benchmark Frontiers</h2>\n"
        f'  <p><a href="{page_name}">Open per-benchmark Pareto curve comparisons</a></p>\n'
        "</section>\n"
        f"{PAGE_END}\n"
    )

    html_text = index_path.read_text(encoding="utf-8")
    if PAGE_START in html_text and PAGE_END in html_text:
        before, rest = html_text.split(PAGE_START, 1)
        _, after = rest.split(PAGE_END, 1)
        updated = before + block + after
    elif "</body>" in html_text:
        updated = html_text.replace("</body>", f"{block}</body>", 1)
    else:
        updated = html_text + block

    if updated != html_text:
        index_path.write_text(updated, encoding="utf-8")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("reports_path", help="Path containing growlibm_base and vanilla_base")
    parser.add_argument("platform", help="Platform label for generated html")
    parser.add_argument(
        "--platform-file",
        default="growlibm/platforms/growlibm.rkt",
        help="Platform file used to discover accelerator names",
    )
    parser.add_argument(
        "--page",
        default="benchmark-frontiers.html",
        help="Output page filename written in reports_path",
    )
    parser.add_argument(
        "--image-dir",
        default="benchmark-frontiers",
        help="Directory for generated benchmark frontier images",
    )
    args = parser.parse_args()

    reports_path = Path(args.reports_path)
    image_dir = reports_path / args.image_dir
    image_dir.mkdir(parents=True, exist_ok=True)

    accelerator_names = load_accelerator_names(Path(args.platform_file))
    grow_tests = load_tests(reports_path / "growlibm_base" / "results.json")
    vanilla_tests = load_tests(reports_path / "vanilla_base" / "results.json")

    shared_keys = [key for key in grow_tests if key in vanilla_tests]
    missing_keys = sorted(set(grow_tests) ^ set(vanilla_tests))
    for key in missing_keys:
        warn(f"benchmark present in only one report: {key}")

    benchmarks = []
    for key in shared_keys:
        grow_test = grow_tests[key]
        vanilla_test = vanilla_tests[key]
        title = str(grow_test.get("name") or key)
        image_name = f"{sanitize_filename(str(grow_test.get('link') or key))}.png"
        benchmark_accelerators = accelerator_hits(grow_test, accelerator_names)

        plot_benchmark(
            [
                {
                    "label": "vanilla_base",
                    "test": vanilla_test,
                    "bits": float(vanilla_test.get("bits", 0)),
                    "color": "#ff7f0e",
                },
                {
                    "label": "growlibm_base",
                    "test": grow_test,
                    "bits": float(grow_test.get("bits", 0)),
                    "color": "#2ca02c",
                },
            ],
            image_dir / image_name,
            title,
            format_accelerator_summary(benchmark_accelerators),
        )

        benchmarks.append(
            {
                "title": title,
                "image": f"{args.image_dir}/{image_name}",
                "growlibm_link": f"growlibm_base/{grow_test.get('link')}/graph.html",
                "vanilla_link": f"vanilla_base/{vanilla_test.get('link')}/graph.html",
                "accelerators": benchmark_accelerators,
            }
        )

    write_page(reports_path / args.page, args.platform, benchmarks)
    inject_page_link(reports_path / "index.html", args.page)


if __name__ == "__main__":
    main()
