#!/usr/bin/env python3

import argparse
import json
import math
import os
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_RESULTS = REPO_ROOT / "reports" / "histo" / "results.json"
DEFAULT_COSTS = REPO_ROOT / "reports" / "histo" / "costs.json"
DEFAULT_COUNTS = REPO_ROOT / "reports" / "histo" / "counts.json"
PLOT_CACHE_DIR = Path("/tmp/herbie-plot-cache")
PLOT_CACHE_DIR.mkdir(parents=True, exist_ok=True)
os.environ.setdefault("MPLCONFIGDIR", str(PLOT_CACHE_DIR / "matplotlib"))
os.environ.setdefault("XDG_CACHE_HOME", str(PLOT_CACHE_DIR / "xdg-cache"))

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt


def warn(message):
    print(f"warning: {message}", file=sys.stderr)


def load_json(path):
    with path.open("r", encoding="utf-8") as handle:
        return json.load(handle)


def load_results(path):
    data = load_json(path)
    tests = data.get("tests")
    if not isinstance(tests, list):
        raise SystemExit(f"{path} is missing a tests list")
    return data


def benchmark_names(results):
    names = []
    for test in results.get("tests", []):
        name = test.get("name") or test.get("input") or test.get("link")
        if name is not None:
            names.append(str(name))
    return names


def numeric_mapping_values(mapping, keys, label):
    if not isinstance(mapping, dict):
        raise SystemExit(f"{label} file must contain a JSON object")

    values = []
    missing = []
    for key in keys:
        raw_value = mapping.get(key)
        if raw_value is None:
            missing.append(key)
            continue

        try:
            values.append(float(raw_value))
        except (TypeError, ValueError):
            warn(f"skipping non-numeric {label} value for {key}")

    if missing:
        warn(f"{label} file is missing {len(missing)} candidates")

    extras = set(mapping) - set(keys)
    if extras:
        warn(f"{label} file has {len(extras)} extra candidates")

    if not values:
        raise SystemExit(f"no numeric {label} values found")

    return values


def end_urgencies(results):
    values = []
    for test in results.get("tests", []):
        raw_error = test.get("end")
        raw_bits = test.get("bits")
        if raw_error is None or raw_bits is None:
            continue

        try:
            error_bits = float(raw_error)
            bits = float(raw_bits)
        except (TypeError, ValueError):
            warn(f"skipping malformed end/bits values for {test.get('name', '<unknown>')}")
            continue

        if bits <= 0:
            warn(f"skipping non-positive bit width for {test.get('name', '<unknown>')}")
            continue

        values.append(100.0 * (error_bits / bits))

    if not values:
        raise SystemExit("no end values found in results.json")

    return values


def maybe_drop_low_urgency(values, enabled):
    if not enabled:
        return values

    filtered = [value for value in values if value > 1]
    if not filtered:
        raise SystemExit("all urgency values were 0.1 or lower after filtering")
    return filtered


def linear_bins(values, bin_count):
    low = min(values)
    high = max(values)
    if low == high:
        padding = 1.0 if low == 0 else abs(low) * 0.05
        return [low - padding, high + padding]
    step = (high - low) / bin_count
    return [low + index * step for index in range(bin_count + 1)]


def log_bins(values, bin_count):
    positive_values = [value for value in values if value > 0]
    if not positive_values:
        raise SystemExit("log-scaled histogram requires positive values")

    low = min(positive_values)
    high = max(positive_values)
    if low == high:
        low *= 0.95
        high *= 1.05
        return [low, high]

    log_low = math.log10(low)
    log_high = math.log10(high)
    step = (log_high - log_low) / bin_count
    return [10 ** (log_low + index * step) for index in range(bin_count + 1)]


def plot_histogram(values, output_path, title, xlabel, color, bins, log_x=False):
    fig, ax = plt.subplots(figsize=(8, 6))
    ax.hist(values, bins=bins, color=color, edgecolor="white", linewidth=0.8)
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel("Candidates")
    ax.grid(axis="y", alpha=0.25, linewidth=0.6)
    ax.set_axisbelow(True)
    if log_x:
        ax.set_xscale("log")

    fig.tight_layout()
    fig.savefig(output_path, dpi=180)
    plt.close(fig)


def main():
    parser = argparse.ArgumentParser(
        description="Plot separate histograms for candidate costs, counts, and end urgency."
    )
    parser.add_argument("--results", type=Path, default=DEFAULT_RESULTS)
    parser.add_argument("--costs", type=Path, default=DEFAULT_COSTS)
    parser.add_argument("--counts", type=Path, default=DEFAULT_COUNTS)
    parser.add_argument("--output-dir", type=Path)
    parser.add_argument("--bins", type=int, default=30)
    parser.add_argument(
        "--drop-low-urgency",
        action="store_true",
        help="Discard candidates whose urgency is 0.1 or lower before plotting the urgency histogram",
    )
    parser.add_argument(
        "--drop-zero-urgency",
        action="store_true",
        help=argparse.SUPPRESS,
    )
    args = parser.parse_args()

    if args.bins < 1:
        raise SystemExit("--bins must be at least 1")

    results = load_results(args.results)
    names = benchmark_names(results)
    costs = numeric_mapping_values(load_json(args.costs), names, "cost")
    counts = numeric_mapping_values(load_json(args.counts), names, "count")
    urgencies = maybe_drop_low_urgency(
        end_urgencies(results),
        args.drop_low_urgency or args.drop_zero_urgency,
    )

    output_dir = args.output_dir or args.results.parent
    output_dir.mkdir(parents=True, exist_ok=True)

    plot_histogram(
        costs,
        output_dir / "costs-histogram.png",
        "Size Distribution",
        "Size",
        "#4c78a8",
        log_bins(costs, args.bins),
        log_x=True,
    )
    plot_histogram(
        counts,
        output_dir / "counts-histogram.png",
        "Frequency Distribution",
        "Frequency",
        "#f58518",
        log_bins(counts, args.bins),
        log_x=True,
    )
    plot_histogram(
        urgencies,
        output_dir / "urgency-histogram.png",
        "Urgency Distribution",
        "Urgency (%)",
        "#54a24b",
        linear_bins(urgencies, args.bins),
    )


if __name__ == "__main__":
    main()
