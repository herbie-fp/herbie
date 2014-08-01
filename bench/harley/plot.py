#!/usr/bin/env python

# Plot a cooccurence matrix
import sys
import numpy as np
import matplotlib.pyplot as plt

usage = "./plot directory"

if __name__ == '__main__':

    # Parse the argument
    if len(sys.argv) != 2:
        sys.exit(usage)

    dirname = sys.argv[1]
    if dirname[-1] != '/':  # Kind of a hack
        dirname += '/'

    # Load the files
    cooc_file = dirname + 'cooc.csv'
    cooc = np.loadtxt(cooc_file,delimiter=',')

    strengths_file = dirname + 'strengths.csv'
    strengths = np.loadtxt(strengths_file,delimiter=',')
    probs = 1/(1+np.exp(-strengths))

    # Plot
    plt.subplot(1,2,1)
    plt.imshow(probs,cmap='jet',interpolation='nearest',vmin=0,vmax=1)
    plt.colorbar()

    plt.subplot(1,2,2)
    plt.imshow(cooc,cmap='jet',interpolation='nearest',vmin=0,vmax=1)
    plt.show()
