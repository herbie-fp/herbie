import json
import matplotlib
import matplotlib.pyplot as plt

matplotlib.rcParams['pdf.fonttype'] = 42
matplotlib.rcParams['ps.fonttype'] = 42
matplotlib.rcParams['savefig.bbox'] = 'tight'

assert (len(sys.argv) == 3)
results_file_vanilla = sys.argv[1]
results_file_egglog = sys.argv[2]

vanilla_data = json.load(results_file_vanilla)["tests"]
egglog_data = json.load(results_file_egglog)["tests"]


tests = set(vanilla_data.map(lambda row: row["input"]))
assert (len(tests) == len(vanilla_data))
vanilla_tests = dict(vanilla_data.map(lambda row: (row["input"], row)))
egglog_tests = dict(egglog_data.map(lambda row: (row["input"], row)))

def plot_error():
  tests_sorted = list(tests).sort(key = lambda test: vanilla_tests[test]["start"])

#plot_error()
#plt.savefig('egglogreport/error.pdf')


