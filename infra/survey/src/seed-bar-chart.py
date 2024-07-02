import sys
import json
import matplotlib.pyplot as plt
import numpy as np

jsonp = sys.argv[1]
field = sys.argv[2]

with open(jsonp, 'r') as f:
    ss = json.load(f)

labs = [s['seed'] for s in ss]
data = [s['data'] for s in ss]

fig, ax = plt.subplots(figsize=(30, 10))
x = np.arange(len(data))
ax.bar(x, data)

ax.set_xlabel('seed')
ax.set_xticks(x)
ax.set_xticklabels(range(1, len(labs) + 1))
ax.set_xticklabels(labs, rotation=45, ha='right')

ax.set_ylabel('sum({})'.format(field))

plt.tight_layout()
plt.savefig('by-seed-{}-bar.pdf'.format(field))
