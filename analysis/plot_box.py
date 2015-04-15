#!/usr/bin/env python

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib as mpl
from math import sqrt

from config import *

alg_names = {
    "e-Fuzzy PSO":r"$\varepsilon$-Fuzzy PSO",
    "MODE":"MODE",
    "HBCS":"HBCS",
    "MOHEFT":"MOHEFT",
    "NSPSO":"NSPSO",
    "NSGA2*":"NSGAII*",
    "SPEA2*":"SPEA2*",
    "MOEA/D":"MOEA/D",
    "NSGA-II":"NSGA-II"
}

def read_csv(filename):
        with open(filename) as f:
                names = f.readline().strip().split(",")[1:]
                d = zip(*[map(float, line.strip().split(",")[1:]) for line in f])
        return (names, d)

def setup_mpl():
    fig_width_pt = 252  # Get this from LaTeX using \showthe\columnwidth
    inches_per_pt = 1.0/72.27               # Convert pt to inch
    golden_mean = (sqrt(5)-1.0)/2.5         # Aesthetic ratio
    fig_width = fig_width_pt*inches_per_pt  # width in inches
    fig_height = fig_width*golden_mean      # height in inches
    fig_size =  [fig_width,fig_height]
    params = {'axes.labelsize': 8,
              'legend.fontsize': 6,
              'xtick.labelsize': 8,
              'ytick.labelsize': 8,
              'text.usetex': True,
              'figure.figsize': fig_size}
    mpl.rcParams.update(params)

def plot(names, hvs, save=None):
        setup_mpl()
        flg, ax = plt.subplots()
        # plt.subplots_adjust(bottom=0.2)
        ax.grid(True)
        plt.ylabel('Runtime ratio')
        ax.set_xscale('log')
        names = [alg_names[n] for n in names]
        ax.boxplot(hvs, labels=names, vert=False)
        plt.axvline(x=1, ls="-", lw=0.5)
        plt.tight_layout(pad=0.3, w_pad=0.3, h_pad=0.3)
        if save:
                plt.savefig(save+".eps", format="eps")
        else:
                plt.show()
if __name__ == '__main__':
        from sys import argv
        ns, hvs = read_csv(argv[1])
        plot(ns, hvs, argv[1][:-4])
