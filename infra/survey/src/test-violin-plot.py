import sys
import json
import matplotlib.pyplot as plt
import numpy as np

jsonp = sys.argv[1]
field = sys.argv[2]

with open(jsonp, 'r') as f:
    ts = json.load(f)

labs = [t['test'] for t in ts]
data = [t['data'] for t in ts]

fig, ax = plt.subplots(figsize=(30,10))
ax.violinplot(data)

ax.set_xlabel('test')
ax.set_xticks(np.arange(1, len(labs) + 1))
ax.set_xticklabels(labs, rotation=45, ha='right')

ax.set_ylabel(field)
ax.set_yticks(np.arange(0, 64, step=4))

plt.tight_layout()
plt.savefig('by-test-{}-violin.pdf'.format(field))
