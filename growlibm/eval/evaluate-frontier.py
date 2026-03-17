#!/usr/bin/env python3

import argparse
import json
import sys
from pathlib import Path

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

def benchmark_key(test):
    identifier = test.get("identifier")
    if identifier not in (None, "", "#f"):
        return str(identifier)
    return str(test.get("name") or test.get("link"))

def has_accelerator_alt(test):
    return bool(test.get("has-accelerator-alt", False))

def parse_cost_accuracy(test):
    raw = test.get("cost-accuracy")
    if not isinstance(raw, list) or len(raw) < 2:
        return None

    initial = parse_point(raw[0])
    if initial is None or initial[0] == 0:
        return None

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


def pareto_compare(point1, point2):
    cost1, error1 = point1
    cost2, error2 = point2
    if cost1 == cost2 and error1 == error2:
        return "="
    if cost1 <= cost2 and error1 <= error2:
        return "<"
    if cost1 >= cost2 and error1 >= error2:
        return ">"
    return "<>"

def pareto_union(curve1, curve2):
    result = []
    left = list(curve1)
    right = list(curve2)
    while left and right:
        point1 = left[0]
        point2 = right[0]
        relation = pareto_compare(point1, point2)
        if relation == "<":
            right.pop(0)
        elif relation == ">":
            left.pop(0)
        elif relation == "=":
            result.append(point1)
            left.pop(0)
            right.pop(0)
        elif point1[1] < point2[1]:
            result.append(point1)
            left.pop(0)
        else:
            result.append(point2)
            right.pop(0)
    result.extend(left)
    result.extend(right)
    return result


def pareto_minimize(points):
    minimized = []
    for point in sorted(points, key=lambda current: current[0]):
        minimized = pareto_union([point], minimized)
    return minimized


def pareto_shift(point0, frontier):
    return [(point0[0] + point[0], point0[1] + point[1]) for point in frontier]


def pareto_convex(points):
    prefix = []
    working = list(points)
    while len(working) >= 3:
        point0, point1, point2 = working[:3]
        slope01 = (point1[1] - point0[1]) / (point1[0] - point0[0])
        slope12 = (point2[1] - point1[1]) / (point2[0] - point1[0])
        if slope12 > slope01:
            if prefix:
                working = [prefix.pop(), point0, point2, *working[3:]]
            else:
                working = [point0, point2, *working[3:]]
        else:
            prefix.append(point0)
            working = [point1, point2, *working[3:]]
    return prefix + working


def pareto_combine(frontiers, convex=False):
    def finalize(frontier):
        return pareto_convex(frontier) if convex else frontier

    combined = []
    for frontier in [pareto_minimize(frontier) for frontier in frontiers]:
        if not combined:
            combined = finalize(frontier)
            continue

        combined_next = []
        for point in combined:
            shifted = pareto_minimize(pareto_shift(point, frontier))
            combined_next = pareto_union(shifted, combined_next)
        combined = finalize(combined_next)
    return combined


def aggregate_frontier(data, benchmark_keys=None):
    tests = data.get("tests", [])
    if benchmark_keys is not None:
        tests = [test for test in tests if benchmark_key(test) in benchmark_keys]

    if not tests:
        return []

    maximum_accuracy = sum(float(test.get("bits", 0)) for test in tests)
    if maximum_accuracy <= 0:
        return []

    parsed = []
    for test in tests:
        cost_accuracy = parse_cost_accuracy(test)
        if cost_accuracy is None:
            continue

        initial, points = cost_accuracy
        rescaled = [(point[0] / initial[0], point[1]) for point in [initial, *points]]
        parsed.append(rescaled)

    if not parsed:
        return []

    return sorted(
        [
            (len(tests) / cost, 1.0 - (error / maximum_accuracy))
            for cost, error in pareto_combine(parsed, convex=True)
            if cost != 0
        ],
        key=lambda point: point[0],
    )


def parse_frontier(results_path, benchmark_keys=None):
    if not results_path.exists():
        warn(f"missing {results_path}")
        return []

    data = load_json(results_path)
    if benchmark_keys is not None:
        return aggregate_frontier(data, benchmark_keys)

    merged = data.get("merged-cost-accuracy")
    if not isinstance(merged, list) or len(merged) < 2:
        warn(f"missing merged-cost-accuracy in {results_path}")
        return []

    frontier = []
    if isinstance(merged[1], list):
        for raw_point in merged[1]:
            point = parse_point(raw_point)
            if point is not None:
                frontier.append(point)

    return sorted(frontier, key=lambda point: point[0])


def build_accuracy_reference(frontier):
    reference = []
    best_speedup = 0.0
    for speedup, accuracy in sorted((point for point in frontier if point[0] > 0), key=lambda point: (-point[1], -point[0])):
        best_speedup = max(best_speedup, speedup)
        if reference and accuracy == reference[-1][0]:
            reference[-1] = (accuracy, best_speedup)
        else:
            reference.append((accuracy, best_speedup))
    return reference


def vanilla_speedup_at_accuracy(reference, accuracy):
    best_speedup = None
    for candidate_accuracy, speedup in reference:
        if candidate_accuracy < accuracy:
            break
        best_speedup = speedup
    return best_speedup


def transform_point(point, vanilla_reference):
    if point is None or point[0] <= 0:
        return None

    vanilla_speedup = vanilla_speedup_at_accuracy(vanilla_reference, point[1])
    if vanilla_speedup is None or vanilla_speedup <= 0:
        return None

    return (point[1], point[0] / vanilla_speedup)


def transform_frontier(frontier, vanilla_reference, max_relative_speedup=None):
    transformed = []
    for point in frontier:
        transformed_point = transform_point(point, vanilla_reference)
        if transformed_point is None:
            continue

        accuracy, relative_speedup = transformed_point
        if max_relative_speedup is not None:
            relative_speedup = min(relative_speedup, max_relative_speedup)
        transformed.append((accuracy, relative_speedup))

    return sorted(transformed, key=lambda point: point[0])


def plot_frontier(series, image_path, platform, max_relative_speedup=None):
    import matplotlib
    from matplotlib.ticker import FuncFormatter

    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    fig, ax = plt.subplots(figsize=(8, 6))
    have_points = False
    minimum_accuracy_point = None
    parsed_series = []
    vanilla_reference = None

    for one_series in series:
        frontier = parse_frontier(
            one_series["results_path"],
            one_series.get("benchmark_keys"),
        )
        parsed_series.append({**one_series, "frontier": frontier})
        if one_series.get("baseline"):
            vanilla_reference = build_accuracy_reference(frontier)

    if vanilla_reference is None:
        raise SystemExit("a baseline series is required to plot speedup over vanilla")

    for one_series in parsed_series:
        transformed_frontier = transform_frontier(
            one_series["frontier"], vanilla_reference, max_relative_speedup
        )

        if transformed_frontier:
            have_points = True
            xs = [point[0] for point in transformed_frontier]
            ys = [point[1] for point in transformed_frontier]
            candidate = min(transformed_frontier, key=lambda point: (point[0], point[1]))
            if minimum_accuracy_point is None or (candidate[0], candidate[1]) < (
                minimum_accuracy_point[0],
                minimum_accuracy_point[1],
            ):
                minimum_accuracy_point = candidate
            ax.scatter(xs, ys, s=26, color=one_series["color"], label=one_series["label"])
            ax.plot(xs, ys, color=one_series["color"], linewidth=1.4, alpha=0.35)

    if not have_points:
        ax.text(0.5, 0.5, "No frontier data found", ha="center", va="center", transform=ax.transAxes)

    title = f"{platform}"
    ax.set_title(title, fontsize=20)
    ax.set_xlabel("Accuracy", fontsize=18)
    ax.set_ylabel("Speedup over vanilla", fontsize=18)
    if minimum_accuracy_point is not None:
        ax.set_xlim(minimum_accuracy_point[0], 1.0)
    else:
        ax.set_xlim(0.4, 1.0)
    if max_relative_speedup is not None:
        ax.set_ylim(0.9, max_relative_speedup)
    else:
        ax.set_ylim(bottom=0.9)
    ax.axhline(1.0, color="black", linewidth=1.0, zorder=0)
    ax.xaxis.set_major_formatter(FuncFormatter(lambda value, _: f"{100 * value:.0f}%"))
    ax.yaxis.set_major_formatter(FuncFormatter(lambda value, _: f"{value:.2g}x"))
    ax.grid(True, linewidth=0.5, alpha=0.2)
    if have_points:
        ax.legend(loc="upper left", fontsize=16)
    ax.tick_params(axis="both", labelsize=14)

    fig.tight_layout()
    fig.savefig(image_path, dpi=180)
    plt.close(fig)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("reports_path", help="Path containing evaluate.sh report folders")
    parser.add_argument("platform", help="Name of growlibm platform under test")
    parser.add_argument(
        "--accelerators-only",
        action="store_true",
        help="Aggregate only benchmarks where growlibm uses an accelerator",
    )
    parser.add_argument(
        "--max-relative-speedup",
        type=float,
        help="Cap plotted speedup-over-vanilla values on the vertical axis",
    )
    args = parser.parse_args()

    reports_path = Path(args.reports_path)
    image_path = reports_path / f"{args.platform.lower()}_frontier.png"
    benchmark_keys = None

    if args.accelerators_only:
        growlibm_results = load_json(reports_path / "growlibm_base" / "results.json")
        benchmark_keys = {
            benchmark_key(test)
            for test in growlibm_results.get("tests", [])
            if has_accelerator_alt(test)
        }
        if not benchmark_keys:
            warn("no benchmarks used accelerators; filtered frontier will be empty")

    series = [
        {
            "label": "vanilla",
            "results_path": reports_path / "vanilla_base" / "results.json",
            "color": "#ff7f0e",
            "baseline": True,
            "benchmark_keys": benchmark_keys,
        },
        {
            "label": "growlibm",
            "results_path": reports_path / "growlibm_base" / "results.json",
            "color": "#2ca02c",
            "benchmark_keys": benchmark_keys,
        },
    ]

    plot_frontier(series, image_path, args.platform, args.max_relative_speedup)

if __name__ == "__main__":
    main()
