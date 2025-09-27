import argparse
import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib
import requests

def plot_cnt_per_iters(outcomes, args):
    # Create figure
    fig, ax = plt.subplots(figsize=(4, 3))
    fig.tight_layout(pad=2.0)
    
    # Drop precision column and sum up based on iteration
    # outcomes = outcomes.drop(['baseline_precision'], axis=1)
    # outcomes = outcomes.groupby(['rival_iter', 'tool_name'], as_index=False).sum()
    
    # Select tools
    baseline = outcomes.loc[(outcomes['tool_name'] == "valid-baseline") & (outcomes['baseline_precision'] > 63)]
    baseline = baseline.drop(['rival_iter'], axis=1)
    baseline = baseline.groupby(['baseline_precision'], as_index=False, sort=True).sum()
    
    rival = outcomes.loc[(outcomes['tool_name'] == "valid-rival") & (outcomes['rival_iter'] > 0)]
    rival = rival.drop(['baseline_precision'], axis=1)
    rival = rival.groupby(['rival_iter'], as_index=False, sort=True).sum()
    
    ax.bar(np.arange(len(baseline)) + 0.925, baseline['number_of_points'], color="green", alpha=1, width=0.5, label='baseline', hatch='/')
    ax.bar(np.arange(len(rival)) + 1.075, rival['number_of_points'], color="red", alpha=0.7, width=0.5, label='reval')

    rival_first_covergence = round(float(rival['number_of_points'][0]) / rival['number_of_points'].sum() * 100, 2)
    print("\\newcommand{\\RivalFirstIterConvergence}{" + str(rival_first_covergence) + "}")
    baseline_first_convergence = round(float(baseline['number_of_points'][0]) / baseline['number_of_points'].sum() * 100, 2)
    print("\\newcommand{\\BaselineFirstIterConvergence}{" + str(baseline_first_convergence) + "}")

    rival_second_covergence = round(float(rival['number_of_points'][1]) / rival['number_of_points'].sum() * 100  + rival_first_covergence, 2)
    print("\\newcommand{\\RivalSecondIterConvergence}{" + str(rival_second_covergence) + "}")
    baseline_second_convergence = round(float(baseline['number_of_points'][1]) / baseline['number_of_points'].sum() * 100 + baseline_first_convergence, 2)
    print("\\newcommand{\\BaselineSecondIterConvergence}{" + str(baseline_second_convergence) + "}")
    
    ax.legend()
    ax.set_xlabel("Iteration")
    ax.set_ylabel("# of converged points")
    plt.ticklabel_format(axis='y', style='sci', scilimits=(4,4))
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)
    plt.tight_layout()
    plt.savefig(args.path + "/cnt_per_iters_plot.pdf", format="pdf")
    
    ax.set_title("Convergence distribution")
    plt.tight_layout()
    plt.savefig(args.path + "/cnt_per_iters_plot.png", format="png")
   

def load_outcomes(path):
    outcomes = json.load(open(path, "r"))["outcomes"]
    outcomes = pd.DataFrame(outcomes, columns=['time', 'rival_iter', 'baseline_precision', 'tool_name', 'number_of_points'])
    return outcomes

parser = argparse.ArgumentParser(prog='ratio_plot.py', description='Script outputs ratio plots')
parser.add_argument('-t', '--timeline', dest='timeline', default="report/timeline.json")
parser.add_argument('-o', '--output-path', dest='path', default="report")
args = parser.parse_args()

outcomes = load_outcomes(args.timeline)
matplotlib.rcParams.update({'font.size': 12})
plot_cnt_per_iters(outcomes, args)
