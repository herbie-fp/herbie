#!/usr/bin/env python3

from __future__ import annotations

import argparse
import html
import json
import re
import subprocess
import sys
import textwrap
from dataclasses import asdict, dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
DEFAULT_LOG_PATH = REPO_ROOT / "reports" / "log.txt"
DEFAULT_OUTPUT_NAME = "timeline.png"

CHECKPOINT_RE = re.compile(
    r"^(start|after_[A-Za-z0-9_]+)\t([^\t]+)\t([0-9]+(?:\.[0-9]+)?)$"
)
SCRIPT_START_RE = re.compile(r"^\+ SCRIPT_START_ISO=(\S+)$")
LOG_TIME_RE = re.compile(r"^\+ log_time ([A-Za-z0-9_]+)$")
ISO_TIME_RE = re.compile(r"^\+ iso_time=(\S+)$")
ELAPSED_RE = re.compile(r"^\+ elapsed=([0-9]+(?:\.[0-9]+)?)$")
RUN_LABEL_RE = re.compile(r"^Running branch ([^ ]+) on repo ([^ ]+)$")
RUN_HERBIE_CANDIDATES_RE = re.compile(r"after_run_herbie_candidates_iter_(\d+)")
ADD_TO_PLATFORM_RE = re.compile(r"after_add_to_platform_iter_(\d+)")
HARDCODED_STAGE_DURATIONS = {
    "after_initial_compilation": 4.1 * 60,
    "after_final_compilation": 18.7 * 60,
}


@dataclass(frozen=True)
class Checkpoint:
    phase: str
    timestamp: str
    elapsed_seconds: float


@dataclass(frozen=True)
class Stage:
    label: str
    start_phase: str
    end_phase: str
    start_timestamp: str
    end_timestamp: str
    elapsed_start_seconds: float
    duration_seconds: float
    elapsed_end_seconds: float
    percent_of_total: float


@dataclass(frozen=True)
class IterationSpan:
    label: str
    elapsed_start_seconds: float
    elapsed_end_seconds: float


def warn(message: str) -> None:
    print(f"warning: {message}", file=sys.stderr)


def format_duration(seconds: float) -> str:
    total_seconds = round(seconds)
    hours, remainder = divmod(total_seconds, 3600)
    minutes, secs = divmod(remainder, 60)
    pieces = []
    if hours:
        pieces.append(f"{hours}h")
    if minutes or hours:
        pieces.append(f"{minutes}m")
    pieces.append(f"{secs}s")
    return " ".join(pieces)


def humanize_phase(phase: str) -> str:
    if phase == "start":
        return "start"

    named_phases = {
        "after_initial_compilation": "initial compilation",
        "after_generate": "candidate generation",
        "after_final_compilation": "final compilation",
    }
    if phase in named_phases:
        return named_phases[phase]

    iter_match = RUN_HERBIE_CANDIDATES_RE.fullmatch(phase)
    if iter_match:
        return "urgency ranking"

    iter_match = ADD_TO_PLATFORM_RE.fullmatch(phase)
    if iter_match:
        return "implication pass"

    if phase.startswith("after_"):
        phase = phase[len("after_") :]
    return phase.replace("_", " ")


def stage_label(start_phase: str, end_phase: str) -> str:
    if start_phase == "start":
        return humanize_phase(end_phase)
    return humanize_phase(end_phase)


def parse_runs_from_checkpoint_rows(lines: list[str]) -> list[list[Checkpoint]]:
    runs = []
    current = []

    for line in lines:
        match = CHECKPOINT_RE.match(line)
        if not match:
            continue

        checkpoint = Checkpoint(
            phase=match.group(1),
            timestamp=match.group(2),
            elapsed_seconds=float(match.group(3)),
        )

        if checkpoint.phase == "start" and current:
            runs.append(current)
            current = [checkpoint]
        else:
            current.append(checkpoint)

    if current:
        runs.append(current)

    return runs


def parse_runs_from_xtrace(lines: list[str]) -> list[list[Checkpoint]]:
    runs = []
    current = []
    pending_phase = None
    pending_timestamp = None
    pending_elapsed = None

    for line in lines:
        start_match = SCRIPT_START_RE.match(line)
        if start_match:
            if current:
                runs.append(current)
            current = [Checkpoint("start", start_match.group(1), 0.0)]
            pending_phase = None
            pending_timestamp = None
            pending_elapsed = None
            continue

        log_time_match = LOG_TIME_RE.match(line)
        if log_time_match:
            pending_phase = log_time_match.group(1)
            pending_timestamp = None
            pending_elapsed = None
            continue

        if pending_phase:
            iso_time_match = ISO_TIME_RE.match(line)
            if iso_time_match:
                pending_timestamp = iso_time_match.group(1)
                continue

            elapsed_match = ELAPSED_RE.match(line)
            if elapsed_match:
                pending_elapsed = float(elapsed_match.group(1))

            if pending_timestamp is not None and pending_elapsed is not None:
                if not current:
                    current = [Checkpoint("start", pending_timestamp, 0.0)]
                current.append(Checkpoint(pending_phase, pending_timestamp, pending_elapsed))
                pending_phase = None
                pending_timestamp = None
                pending_elapsed = None

    if current:
        runs.append(current)

    return runs


def extract_run_labels(lines: list[str]) -> list[str]:
    labels = []
    for line in lines:
        match = RUN_LABEL_RE.match(line)
        if match:
            labels.append(f"{match.group(1)} on {match.group(2)}")
    return labels


def normalize_run(run: list[Checkpoint]) -> list[Checkpoint]:
    deduped = []
    seen = set()
    for checkpoint in run:
        key = (checkpoint.phase, checkpoint.timestamp, checkpoint.elapsed_seconds)
        if key in seen:
            continue
        seen.add(key)
        deduped.append(checkpoint)
    return sorted(deduped, key=lambda checkpoint: checkpoint.elapsed_seconds)


def load_runs(log_path: Path) -> tuple[list[list[Checkpoint]], list[str]]:
    lines = log_path.read_text(encoding="utf-8", errors="replace").splitlines()
    labels = extract_run_labels(lines)

    runs = [normalize_run(run) for run in parse_runs_from_checkpoint_rows(lines)]
    runs = [run for run in runs if run]
    if runs:
        return runs, labels

    runs = [normalize_run(run) for run in parse_runs_from_xtrace(lines)]
    runs = [run for run in runs if run]
    return runs, labels


def select_run(runs: list[list[Checkpoint]], run_index: int) -> list[Checkpoint]:
    if not runs:
        raise SystemExit("no timeline checkpoints found in log")

    try:
        run = runs[run_index]
    except IndexError as err:
        raise SystemExit(
            f"requested run index {run_index} but only found {len(runs)} run(s)"
        ) from err

    if len(run) < 2:
        raise SystemExit("need at least two checkpoints to build a stage timeline")

    return run


def build_stages(checkpoints: list[Checkpoint]) -> list[Stage]:
    stages = []
    elapsed_start_seconds = 0.0

    for start, end in zip(checkpoints, checkpoints[1:]):
        actual_duration = end.elapsed_seconds - start.elapsed_seconds
        if actual_duration < 0:
            raise SystemExit(
                f"checkpoint {end.phase} is earlier than {start.phase}; log order is inconsistent"
            )
        duration = HARDCODED_STAGE_DURATIONS.get(end.phase, actual_duration)
        elapsed_end_seconds = elapsed_start_seconds + duration

        stages.append(
            Stage(
                label=stage_label(start.phase, end.phase),
                start_phase=start.phase,
                end_phase=end.phase,
                start_timestamp=start.timestamp,
                end_timestamp=end.timestamp,
                elapsed_start_seconds=elapsed_start_seconds,
                duration_seconds=duration,
                elapsed_end_seconds=elapsed_end_seconds,
                percent_of_total=0.0,
            )
        )
        elapsed_start_seconds = elapsed_end_seconds

    total_elapsed = elapsed_start_seconds
    if total_elapsed <= 0:
        raise SystemExit("final checkpoint has non-positive elapsed time")

    return [
        Stage(
            label=stage.label,
            start_phase=stage.start_phase,
            end_phase=stage.end_phase,
            start_timestamp=stage.start_timestamp,
            end_timestamp=stage.end_timestamp,
            elapsed_start_seconds=stage.elapsed_start_seconds,
            duration_seconds=stage.duration_seconds,
            elapsed_end_seconds=stage.elapsed_end_seconds,
            percent_of_total=(100.0 * stage.duration_seconds / total_elapsed),
        )
        for stage in stages
    ]


def stage_color(index: int) -> str:
    palette = [
        "#4c78a8",
        "#f58518",
        "#54a24b",
        "#e45756",
        "#72b7b2",
        "#b279a2",
        "#ff9da6",
        "#9d755d",
        "#bab0ab",
        "#8cd17d",
    ]
    return palette[index % len(palette)]


def iteration_index_for_phase(phase: str) -> int | None:
    for pattern in (RUN_HERBIE_CANDIDATES_RE, ADD_TO_PLATFORM_RE):
        match = pattern.fullmatch(phase)
        if match:
            return int(match.group(1))
    return None


def build_iteration_spans(stages: list[Stage]) -> list[IterationSpan]:
    spans = []
    current_iteration = None
    current_start = None
    current_end = None

    for stage in stages:
        iteration = iteration_index_for_phase(stage.end_phase)
        if iteration is None:
            if current_iteration is not None:
                spans.append(
                    IterationSpan(
                        label=f"Filter Iteration {current_iteration + 1}",
                        elapsed_start_seconds=current_start,
                        elapsed_end_seconds=current_end,
                    )
                )
                current_iteration = None
                current_start = None
                current_end = None
            continue

        if iteration != current_iteration:
            if current_iteration is not None:
                spans.append(
                    IterationSpan(
                        label=f"Filter Iteration {current_iteration + 1}",
                        elapsed_start_seconds=current_start,
                        elapsed_end_seconds=current_end,
                    )
                )
            current_iteration = iteration
            current_start = stage.elapsed_start_seconds
            current_end = stage.elapsed_end_seconds
        else:
            current_end = stage.elapsed_end_seconds

    if current_iteration is not None:
        spans.append(
            IterationSpan(
                label=f"Filter Iteration {current_iteration + 1}",
                elapsed_start_seconds=current_start,
                elapsed_end_seconds=current_end,
            )
        )

    return spans


def render_svg(checkpoints: list[Checkpoint], stages: list[Stage]) -> str:
    total_elapsed = stages[-1].elapsed_end_seconds
    iteration_spans = build_iteration_spans(stages)
    width = 1400
    left_margin = 110
    right_margin = 60
    top_margin = 46
    iteration_bar_top = 60
    iteration_bar_height = 18
    bar_top = 90
    bar_height = 42
    label_top = 180
    label_row_gap = 74
    label_line_height = 18
    bottom_margin = 44
    plot_width = width - left_margin - right_margin
    height = label_top + label_row_gap + (3 * label_line_height) + bottom_margin
    subtitle = f"Total time: {format_duration(total_elapsed)}"

    parts = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}" role="img" aria-labelledby="title desc">',
        "<title id=\"title\">growlibm timeline from log</title>",
        f"<desc id=\"desc\">{html.escape(subtitle)}</desc>",
        """
<style>
  text { fill: #1f1f1f; font-family: 'DejaVu Sans', Arial, Helvetica, sans-serif; }
  .title { font-size: 28px; font-weight: 700; }
  .guide { stroke: #999; stroke-width: 1; }
  .iteration-bar { fill: #ffffff; stroke: #666; stroke-width: 1.5; }
  .iteration-label { font-size: 13px; font-weight: 700; }
  .label { font-size: 15px; }
  .duration { font-size: 14px; font-weight: 700; }
</style>
""".strip(),
        f'<rect x="0" y="0" width="{width}" height="{height}" fill="#ffffff" />',
        f'<text class="title" x="{left_margin}" y="{top_margin}">{html.escape(subtitle)}</text>',
        f'<rect x="{left_margin}" y="{bar_top}" width="{plot_width}" height="{bar_height}" rx="6" fill="#f0f0f0" />',
    ]

    for span in iteration_spans:
        x_pos = left_margin + (plot_width * span.elapsed_start_seconds / total_elapsed)
        bar_width = plot_width * (
            (span.elapsed_end_seconds - span.elapsed_start_seconds) / total_elapsed
        )
        center_x = x_pos + (bar_width / 2.0)
        parts.append(
            f'<rect class="iteration-bar" x="{x_pos:.2f}" y="{iteration_bar_top}" width="{bar_width:.2f}" height="{iteration_bar_height}" rx="5" />'
        )
        parts.append(
            f'<text class="iteration-label" x="{center_x:.2f}" y="{iteration_bar_top + 13:.2f}" text-anchor="middle">{html.escape(span.label)}</text>'
        )

    for index, stage in enumerate(stages):
        color = stage_color(index)
        x_pos = left_margin + (plot_width * stage.elapsed_start_seconds / total_elapsed)
        bar_width = max(2.0, plot_width * stage.duration_seconds / total_elapsed)
        center_x = x_pos + (bar_width / 2.0)
        label_x = center_x
        label_y = label_top + ((index % 2) * label_row_gap)
        wrapped = textwrap.wrap(stage.label, width=18) or [stage.label]

        parts.append(
            f'<rect x="{x_pos:.2f}" y="{bar_top}" width="{bar_width:.2f}" height="{bar_height}" rx="6" fill="{color}" />'
        )
        parts.append(
            f'<line class="guide" x1="{center_x:.2f}" y1="{bar_top + bar_height}" x2="{label_x:.2f}" y2="{label_y - 22:.2f}" />'
        )
        parts.append(
            f'<text class="label" x="{label_x:.2f}" y="{label_y:.2f}" text-anchor="middle">{html.escape(wrapped[0])}</text>'
        )

        for line_index, line in enumerate(wrapped[1:2], start=1):
            parts.append(
                f'<text class="label" x="{label_x:.2f}" y="{label_y + line_index * label_line_height:.2f}" text-anchor="middle">{html.escape(line)}</text>'
            )

        duration_y = label_y + min(len(wrapped), 2) * label_line_height
        parts.append(
            f'<text class="duration" x="{label_x:.2f}" y="{duration_y:.2f}" text-anchor="middle">{html.escape(format_duration(stage.duration_seconds))}</text>'
        )

    parts.append("</svg>")
    return "\n".join(parts)


def write_output(svg_text: str, output_path: Path) -> Path:
    suffix = output_path.suffix.lower()
    if suffix == "":
        output_path = output_path.with_suffix(".png")
        suffix = ".png"

    output_path.parent.mkdir(parents=True, exist_ok=True)

    if suffix == ".svg":
        output_path.write_text(svg_text, encoding="utf-8")
        return output_path

    if suffix == ".png":
        try:
            subprocess.run(
                ["rsvg-convert", "--format=png", "--output", str(output_path)],
                input=svg_text,
                text=True,
                check=True,
            )
        except FileNotFoundError as err:
            raise SystemExit("rsvg-convert is required to export PNG output") from err
        return output_path

    raise SystemExit("--output must end in .png or .svg")


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate a stage timeline plot from a growlibm reports/log.txt file."
    )
    parser.add_argument(
        "log_path",
        nargs="?",
        type=Path,
        default=DEFAULT_LOG_PATH,
        help="Path to the growlibm log file (default: reports/log.txt)",
    )
    parser.add_argument(
        "-o",
        "--output",
        type=Path,
        help="Plot output path (default: alongside log as timeline-from-log.png)",
    )
    parser.add_argument(
        "--json-output",
        type=Path,
        help="Optional JSON output path for the extracted checkpoints and stages",
    )
    parser.add_argument(
        "--run-index",
        type=int,
        default=-1,
        help="Which run to render when the log contains multiple runs (default: -1, the last run)",
    )
    args = parser.parse_args()

    log_path = args.log_path
    if not log_path.exists():
        raise SystemExit(f"log file not found: {log_path}")

    output_path = args.output or log_path.with_name(DEFAULT_OUTPUT_NAME)

    runs, labels = load_runs(log_path)
    checkpoints = select_run(runs, args.run_index)
    stages = build_stages(checkpoints)

    if len(runs) > 1:
        warn(f"found {len(runs)} runs in {log_path}; rendering run index {args.run_index}")

    selected_run_number = args.run_index
    if selected_run_number < 0:
        selected_run_number = len(runs) + selected_run_number

    run_label = f"run {selected_run_number + 1}"
    if 0 <= selected_run_number < len(labels):
        run_label = labels[selected_run_number]

    svg_text = render_svg(checkpoints=checkpoints, stages=stages)
    output_path = write_output(svg_text, output_path)

    if args.json_output is not None:
        args.json_output.parent.mkdir(parents=True, exist_ok=True)
        payload = {
            "log": str(log_path),
            "run_index": args.run_index,
            "run_count": len(runs),
            "run_label": run_label,
            "checkpoints": [asdict(checkpoint) for checkpoint in checkpoints],
            "stages": [asdict(stage) for stage in stages],
        }
        args.json_output.write_text(json.dumps(payload, indent=2), encoding="utf-8")

    print(output_path)


if __name__ == "__main__":
    main()
