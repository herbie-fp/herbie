import json
import matplotlib.pyplot as plt
from matplotlib.ticker import PercentFormatter, MultipleLocator

baseline = json.load(open("baseline/results.json"))
egglog_preprocess = json.load(open("egglog-preprocess/results.json"))
egglog_multi = json.load(open("egglog-multi/results.json"))
egglog_all = json.load(open("egglog-all/results.json"))

plt.xlim(0, 10) 
plt.ylim(0, 1) 
original_cost = (baseline['merged-cost-accuracy'][0][0] + egglog_preprocess['merged-cost-accuracy'][0][0] + egglog_multi['merged-cost-accuracy'][0][0]) / 3
original_accuracy = (baseline['merged-cost-accuracy'][0][1] + egglog_preprocess['merged-cost-accuracy'][0][1] + egglog_multi['merged-cost-accuracy'][0][1]) / 3
plt.plot(original_cost, original_accuracy, color='red', marker='s')

baseline_cost, baseline_accuracy = zip(*baseline['merged-cost-accuracy'][1])
plt.plot(baseline_cost, baseline_accuracy, label="Baseline (5.1 min, 0 Timeouts)", color="blue")

egglog_preprocess_cost, egglog_preprocess_accuracy = zip(*egglog_preprocess['merged-cost-accuracy'][1])
plt.plot(egglog_preprocess_cost, egglog_preprocess_accuracy, label="Egglog Preprocessing (4.6 min, 0 Timeouts)", color="Green")

egglog_multi_cost, egglog_multi_accuracy = zip(*egglog_multi['merged-cost-accuracy'][1])
plt.plot(egglog_multi_cost, egglog_multi_accuracy, label="Egglog Multi Extractor (7.1 min, 0 Timeouts)", color="Purple")

egglog_all_cost, egglog_all_accuracy = zip(*egglog_all['merged-cost-accuracy'][1])
plt.plot(egglog_all_cost, egglog_all_accuracy, label="Egglog All Extractors (32.4 min, 7 Timeouts)", color="olive")


plt.gca().yaxis.set_major_locator(MultipleLocator(0.1)) 
plt.gca().yaxis.set_major_formatter(PercentFormatter(xmax=1))
xticks = plt.gca().get_xticks()

# Add an "x" to each X-axis label
plt.xticks(xticks, [f'{int(i)}x' for i in xticks])
plt.xlabel("Speedup")
plt.ylabel("Accuracy")
plt.title("Accuracy vs. Speed")
plt.legend()
plt.show()