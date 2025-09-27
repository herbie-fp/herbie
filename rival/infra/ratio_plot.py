import argparse
import json
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import requests

def plot_speed_graph_rival_iter(outcomes, args):
    # Create figure
    fig, ax = plt.subplots(figsize=(4, 2.5))
    
    # Drop precision column and sum up based on iteration
    outcomes = outcomes.drop(['baseline_precision'], axis=1)
    outcomes = outcomes.groupby(['rival_iter', 'tool_name'], as_index=False).sum()

    # Select appropriate tools
    baseline_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-baseline") & (outcomes['rival_iter'] > 0)]
    rival_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-rival") & (outcomes['rival_iter'] > 0)]
    sollya_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-sollya") & (outcomes['rival_iter'] > 0)]

    # Some weird functions that creates speed per millisecond for each tool
    def add_values(row):
        return int(row['rival_iter']), (row['number_of_points'] / row['time']) * 1000
    def tool_cmp2speed(x):
        return x.sort_values(by=['rival_iter']).apply(add_values, axis=1, result_type='expand')

    # Sollya timings considered are as base since we are doing speed ratio comparison
    base = np.array(tool_cmp2speed(sollya_cmp)[1])

    # Plot Rival
    ax.plot(tool_cmp2speed(rival_cmp)[0], np.array(tool_cmp2speed(rival_cmp)[1])/base, '.-', linewidth=2.0, color='r', label='reval')
    # Plot Baseline
    ax.plot(tool_cmp2speed(baseline_cmp)[0], np.array(tool_cmp2speed(baseline_cmp)[1])/base, '--', linewidth=2.0, color='g',
            label='baseline')
    # Plot Sollya
    ax.plot(tool_cmp2speed(sollya_cmp)[0], np.array(tool_cmp2speed(sollya_cmp)[1])/base, '-', linewidth=2.0, color='b',
            label='sollya')

    ax.legend()
    ax.set_xlabel("Difficulty")
    ax.set_ylabel("Ratio")
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)
    plt.savefig(args.path + "/ratio_plot_iter.pdf", format="pdf")
    
    ax.set_title("Ratio plot per iteration")
    plt.tight_layout()
    plt.savefig(args.path + "/ratio_plot_iter.png", format="png")
    
    
    # Latex stuff
    # print("\\newcommand{\RivalAvgSpeedupOverSollya}{" + str(round(sollya_cmp['time'].sum() / rival_cmp['time'].sum(), 2)) + "\\xspace}")
    # print("\\newcommand{\RivalAvgSpeedupOverBaseline}{" + str(round(baseline_cmp['time'].sum() / rival_cmp['time'].sum(), 2)) + "\\xspace}")
    # print("\\newcommand{\RivalMaxSpeedupOverSollya}{" + str(round(np.array(tool_cmp2speed(rival_cmp)[1])[-1]/np.array(tool_cmp2speed(sollya_cmp)[1])[-1], 2)) + "\\xspace}")
    # print("\\newcommand{\RivalMaxSpeedupOverBaseline}{" + str(round(np.array(tool_cmp2speed(rival_cmp)[1])[-1]/np.array(tool_cmp2speed(baseline_cmp)[1])[-1], 2)) + "\\xspace}")
    
def plot_speed_graph_baseline_precision(outcomes, args, sollya_norm=False):
    # Create figure
    fig, ax = plt.subplots(figsize=(4, 2.5))
    
    # Drop precision column and sum up based on iteration
    outcomes = outcomes.drop(['rival_iter'], axis=1)
    outcomes = outcomes.groupby(['baseline_precision', 'tool_name'], as_index=False).sum()
    
    # Select appropriate tools
    baseline_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-baseline") & (outcomes['baseline_precision'] > 73)]
    rival_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-rival") & (outcomes['baseline_precision'] > 73)]
    sollya_cmp = outcomes.loc[(outcomes['tool_name'] == "valid-sollya") & (outcomes['baseline_precision'] > 73)]

    rival_initial = float(outcomes.loc[(outcomes['tool_name'] == "valid-rival") & (outcomes['baseline_precision'] == 63)]['time'].iloc[0])
    baseline_initial = float(outcomes.loc[(outcomes['tool_name'] == "valid-baseline") & (outcomes['baseline_precision'] == 63)]['time'].iloc[0])
    sollya_initial = float(outcomes.loc[(outcomes['tool_name'] == "valid-sollya") & (outcomes['baseline_precision'] == 63)]['time'].iloc[0])
    
    if sollya_norm:
        print("\\newcommand{\\NumTunedPoints}{" + str(rival_cmp['number_of_points'].sum()) + "\\xspace}")
        print("\\newcommand{\\NumUntunedPoints}{" + str(4070208-rival_cmp['number_of_points'].sum()) + "\\xspace}")

    rival_initial = float(outcomes.loc[(outcomes['tool_name'] == "valid-rival") & (outcomes['baseline_precision'] == 63)]['time'].iloc[0])
    baseline_initial = float(outcomes.loc[(outcomes['tool_name'] == "valid-baseline") & (outcomes['baseline_precision'] == 63)]['time'].iloc[0])
    sollya_initial = float(outcomes.loc[(outcomes['tool_name'] == "valid-sollya") & (outcomes['baseline_precision'] == 63)]['time'].iloc[0])
    
    if sollya_norm:
        print("\\newcommand{\\RivalInitialSpeedupOverSollya}{" + str(round(sollya_initial/rival_initial, 2)) + "\\xspace}")
        print("\\newcommand{\\RivalInitialSpeedupOverBaseline}{" + str(round(baseline_initial/rival_initial, 2)) + "\\xspace}")
    
    # Some weird functions that creates speed per millisecond for each tool
    def add_values(row):
        return int(row['baseline_precision']), (row['number_of_points'] / row['time']) * 1000
    def tool_cmp2speed(x):
        return x.sort_values(by=['baseline_precision']).apply(add_values, axis=1, result_type='expand')

    # Sollya timings considered are as base since we are doing speed ratio comparison
    base = np.array(tool_cmp2speed(sollya_cmp if sollya_norm else baseline_cmp)[1])
    x = tool_cmp2speed(rival_cmp)[0]

    # Plot Rival
    ax.plot(np.arange(len(x)), np.array(tool_cmp2speed(rival_cmp)[1])/base, '.-', linewidth=2.0, color='r', label='reval')
    # Plot Baseline
    ax.plot(np.arange(len(x)), np.array(tool_cmp2speed(baseline_cmp)[1])/base, '--', linewidth=2.0, color='g',
            label='baseline')
    # Plot Sollya
    ax.plot(np.arange(len(x)), np.array(tool_cmp2speed(sollya_cmp)[1])/base, '-', linewidth=2.0, color='b',
            label='sollya')

    ax.legend()
    ax.set_xlabel("True uniform precision")
    ax.set_ylabel("Ratio")
    
    ax.set_xticks(np.arange(len(x)), x)
    ax.set_xticklabels(["$2^{" + str(i+7) + "}$" for i, x in enumerate(x)])
    
    ax.yaxis.grid(True, linestyle='-', which='major', color='grey', alpha=0.3)
    plt.tight_layout()
    if sollya_norm:
        plt.savefig(args.path + "/ratio_plot_precision.pdf", format="pdf")
        
        ax.set_title("Ratio plot per precision, Sollya normalized")
        plt.tight_layout()
        plt.savefig(args.path + "/ratio_plot_precision.png", format="png")
    
    else:
        plt.savefig(args.path + "/ratio_plot_precision_base_norm.pdf", format="pdf")
        
        ax.set_title("Ratio plot per precision, Baseline normalized")
        plt.tight_layout()
        plt.savefig(args.path + "/ratio_plot_precision_base_norm.png", format="png")
      
    # Latex stuff  
    if sollya_norm:
        average_over_sollya = round(sollya_cmp['time'].sum() / rival_cmp['time'].sum(), 2)
        average_over_baseline = round(baseline_cmp['time'].sum() / rival_cmp['time'].sum(), 2)
        print("\\newcommand{\\RivalAvgSpeedupOverSollya}{" + str(average_over_sollya) + "\\xspace}")
        print("\\newcommand{\\RivalAvgSpeedupOverBaseline}{" + str(average_over_baseline) + "\\xspace}")
        
        max_over_sollya = max([round(i/j, 2) for i, j in zip(np.array(tool_cmp2speed(rival_cmp)[1]), np.array(tool_cmp2speed(sollya_cmp)[1]))])
        max_over_baseline = max([round(i/j, 2) for i, j in zip(np.array(tool_cmp2speed(rival_cmp)[1]), np.array(tool_cmp2speed(baseline_cmp)[1]))])
        print("\\newcommand{\\RivalMaxSpeedupOverSollya}{" + str(max_over_sollya) + "\\xspace}")
        print("\\newcommand{\\RivalMaxSpeedupOverBaseline}{" + str(max_over_baseline) + "\\xspace}")
    
def load_outcomes(path):
    outcomes = json.load(open(path, "r"))["outcomes"]
    outcomes = pd.DataFrame(outcomes, columns=['time', 'rival_iter', 'baseline_precision', 'tool_name', 'number_of_points'])
    return outcomes

parser = argparse.ArgumentParser(prog='ratio_plot.py', description='Script outputs ratio plots')
parser.add_argument('-t', '--timeline', dest='timeline', default="report/timeline.json")
parser.add_argument('-o', '--output-path', dest='path', default="report")
args = parser.parse_args()

outcomes = load_outcomes(args.timeline)
plot_speed_graph_rival_iter(outcomes, args)
plot_speed_graph_baseline_precision(outcomes, args, sollya_norm=True)
plot_speed_graph_baseline_precision(outcomes, args, sollya_norm=False)

