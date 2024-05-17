# runner.plot_times(cores, times)
# runner.plot_pareto(frontier)

# By default, just open the plots, have an option to save them somewhere
# Remove time_unit from Runner? Always use what is probably there everytime anyways

from typing import List
from pathlib import Path

import argparse
import json
import matplotlib.pyplot as plt

def plot_times(cores: List[float], times: List[float]):
    """Plots Herbie cost estimate vs actual run time"""
    costs = plt.scatter(costs, times)
    plt.title("Estimated cost vs. actual run time")
    plt.xlabel("Estimated cost (Herbie)")
    plt.ylabel(f"Run time ({time_unit})")

    if save_path is not None:
        plt.savefig(save_path)
        plt.close()

def main():
    parser = argparse.ArgumentParser(description="Plotter tool for cost report data")
    parser.add_argument("report_file", help="path to report file to read", type=argparse.FileType())
    parser.add_argument("--baseline", help="plot baseline report", action="store_true")
    parser.add_argument("--output_dir", type=Path)
    arguments = parser.parse_args()
    report = json.load(arguments.report_file)

    if arguments.baseline:
        costs, errors = zip(*report["frontier"])
        baseline_costs, baseline_errors = zip(*report["baseline_frontier"])

        plt.plot(costs, errors, label="accelerators")
        plt.plot(baseline_costs, baseline_errors, label="baseline")
        plt.title('Estimated cost vs. cumulative average error (bits)')
        plt.xlabel('Estimated cost (Herbie)')
        plt.ylabel(f'Cumulative average error')
        plt.legend()

        if arguments.output_dir:
            plt.savefig(arguments.output_dir / "frontier_comparison.png")
        else:
            plt.show()
    else:
        costs, times = zip(*[(item["platform_core"]["cost"], item["time"]) for items in report["cores"] for item in items["platform_cores"]])
        plt.figure()
        plt.scatter(costs, times)
        plt.title('Estimated cost vs. actual run time')
        plt.xlabel('Estimated cost (Herbie)')
        plt.ylabel(f'Run time (ms)')
        if arguments.output_dir:
            plt.savefig(arguments.output_dir / "times.png")

        frontier_costs, frontier_errors = zip(*report["frontier"])
        plt.figure()
        plt.plot(frontier_costs, frontier_errors, label='Points')
        plt.title('Estimated cost vs. cumulative average error (bits)')
        plt.xlabel('Estimated cost (Herbie)')
        plt.ylabel(f'Cumulative average error')
        if arguments.output_dir:
            plt.savefig(arguments.output_dir / "frontier.png")

        if arguments.output_dir is None:
            plt.show()

if __name__ == "__main__":
    main()
