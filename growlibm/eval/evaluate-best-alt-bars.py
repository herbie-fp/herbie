#!/usr/bin/env python3

import argparse
import json
import re
import sys
from pathlib import Path

from accelerator_utils import accelerator_hits, load_accelerator_names

def warn(message):
    print(f"warning: {message}", file=sys.stderr)


def load_json(path):
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def benchmark_key(test):
    identifier = test.get("identifier")
    if identifier not in (None, "", "#f"):
        return str(identifier)
    return str(test.get("name") or test.get("link"))


def parse_point(raw):
    if not isinstance(raw, list) or len(raw) < 2:
        return None
    try:
        return (float(raw[0]), float(raw[1]))
    except (TypeError, ValueError):
        return None


def most_accurate_alt(test):
    raw = test.get("cost-accuracy")
    if not isinstance(raw, list) or len(raw) < 2:
        return None

    initial = parse_point(raw[0])
    best = parse_point(raw[1])
    bits = float(test.get("bits", 0))
    if initial is None or best is None or initial[0] == 0 or bits <= 0:
        return None

    return {
        "error": 100.0 * (best[1] / bits),
        "speedup": initial[0] / best[0] if best[0] else 0.0,
    }


def collect_benchmarks(reports_path, accelerator_names):
    grow_data = load_json(reports_path / "growlibm_base" / "results.json")
    vanilla_data = load_json(reports_path / "vanilla_base" / "results.json")
    vanilla_tests = {benchmark_key(test): test for test in vanilla_data.get("tests", [])}

    benchmarks = []
    for grow_test in grow_data.get("tests", []):
        accelerators = accelerator_hits(grow_test, accelerator_names)
        if not accelerators:
            continue

        key = benchmark_key(grow_test)
        vanilla_test = vanilla_tests.get(key)
        if vanilla_test is None:
            warn(f"missing vanilla benchmark for {key}")
            continue

        grow_alt = most_accurate_alt(grow_test)
        vanilla_alt = most_accurate_alt(vanilla_test)
        if grow_alt is None or vanilla_alt is None:
            continue

        benchmarks.append(
            {
                "name": str(grow_test.get("name") or key),
                "accelerators": accelerators,
                "grow_error": grow_alt["error"],
                "vanilla_error": vanilla_alt["error"],
                "grow_speedup": grow_alt["speedup"],
                "vanilla_speedup": vanilla_alt["speedup"],
            }
        )

    return benchmarks


def plot_grouped_bars(
    benchmarks, image_path, title, grow_key, vanilla_key, ylabel, square=False
):
    import matplotlib


    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    if not benchmarks:
        raise SystemExit("no shared benchmarks with most-accurate alternatives found")

    labels = [format_label(benchmark["name"]) for benchmark in benchmarks]
    grow_values = [benchmark[grow_key] for benchmark in benchmarks]
    vanilla_values = [benchmark[vanilla_key] for benchmark in benchmarks]
    max_value = max([*grow_values, *vanilla_values], default=0.0)

    if square:
        side = max(8.0, 0.7 * len(benchmarks) + 3.0)
        fig, ax = plt.subplots(figsize=(side, side))
    else:
        fig_width = max(13.0, 0.6 * len(benchmarks) + 3.0)
        fig, ax = plt.subplots(figsize=(fig_width, 6.5))
    xs = list(range(len(benchmarks)))
    bar_width = 0.38

    ax.bar(
        [x - bar_width / 2 for x in xs],
        vanilla_values,
        width=bar_width,
        color="#ff7f0e",
        label="vanilla",
    )
    ax.bar(
        [x + bar_width / 2 for x in xs],
        grow_values,
        width=bar_width,
        color="#2ca02c",
        label="growlibm",
    )

    label_offset = max(0.02 * max_value, 0.15)
    top_limit = max(1.0, max_value + label_offset + 0.4)
    ax.set_ylim(0, top_limit)

    def add_bar_labels(values, x_offset):
        for x, value in zip(xs, values):
            ax.text(
                x + x_offset,
                value + label_offset,
                f"{value:.1f}%",
                ha="center",
                va="bottom",
                fontsize=11,
            )

    add_bar_labels(vanilla_values, -bar_width / 2)
    add_bar_labels(grow_values, bar_width / 2)

    ax.set_xticks(xs)
    ax.set_xticklabels(
        labels,
        rotation=90,
        fontsize=16,
    )
    ax.set_ylabel(ylabel, fontsize=20)
    ax.set_title(title, fontsize=20)
    ax.grid(True, axis="y", linewidth=0.5, alpha=0.2)
    ax.legend(loc="best")

    fig.tight_layout()
    fig.savefig(image_path, dpi=180)
    plt.close(fig)


def format_label(name):
    if len(name) <= 10:
        return name

    split_points = [match.end() for match in re.finditer(r"[-_/]", name[:-1])]
    if not split_points:
        return name

    midpoint = len(name) / 2
    split_point = min(split_points, key=lambda point: abs(point - midpoint))
    return f"{name[:split_point]}\n{name[split_point:]}"


def split_benchmarks(benchmarks, split_at):
    if not split_at:
        return [(benchmarks, None, None)]

    split_index = next(
        (index for index, benchmark in enumerate(benchmarks) if benchmark["name"] == split_at),
        None,
    )
    if split_index is None:
        warn(f"split benchmark {split_at!r} not found; plotting a single chart")
        return [(benchmarks, None, None)]

    if split_index == 0:
        warn(f"split benchmark {split_at!r} is first; plotting a single chart")
        return [(benchmarks, None, None)]

    return [
        (benchmarks[:split_index], f"before {split_at}", None),
        (benchmarks[split_index:], f"from {split_at} onward", f"from-{split_at}"),
    ]


def figure_path(base_path, label_suffix):
    if label_suffix is None:
        return base_path

    extension = base_path.suffix or ".png"
    label = re.sub(r"[^A-Za-z0-9._-]+", "_", label_suffix).strip("._") or "split"
    return base_path.with_name(f"{base_path.stem}-{label}{extension}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("reports_path", help="Path containing growlibm_base and vanilla_base")
    parser.add_argument("platform", help="Platform label for generated plots")
    parser.add_argument(
        "--platform-file",
        default="growlibm/platforms/growlibm.rkt",
        help="Platform file used to discover accelerator names",
    )
    args = parser.parse_args()

    reports_path = Path(args.reports_path)
    accelerator_names = load_accelerator_names(Path(args.platform_file))
    benchmarks = collect_benchmarks(reports_path, accelerator_names)
    benchmarks.sort(key=lambda benchmark: benchmark["vanilla_error"], reverse=True)

    platform_key = args.platform.lower()
    split_at = "forward-y" if platform_key == "proj" else None
    square = platform_key in {"basilisk", "coolprop"}
    base_image_path = reports_path / f"{platform_key}_best-alt-bars.png"

    for section_benchmarks, split_label, suffix in split_benchmarks(benchmarks, split_at):
        image_path = figure_path(base_image_path, suffix)
        title = args.platform
        plot_grouped_bars(
            section_benchmarks,
            image_path,
            title,
            "grow_error",
            "vanilla_error",
            "Error (%)",
            square=square,
        )

if __name__ == "__main__":
    main()
