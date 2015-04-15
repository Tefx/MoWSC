#!/usr/bin/env python

from query import query
from utils import Normaliser
from hv import HyperVolume
from config import *

import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
from itertools import groupby
from math import sqrt

RES_PATH = "./results/pegasus/"
RES_PATH_HISTORY = RES_PATH + "hvs/"
RES_PATH_FRONT = RES_PATH + "fronts/"

alg_names = {
    # "nsga2_ind-0":"EMS-C",
    "nsga2_ind-1":"EMS-C+ (NSGA-II)",
    # "nsga2_ind-2":"NSGA2*",
    # "spea2_ind-0":"EMS-C (SPEA2)",
    "spea2_ind-1":"EMS-C+ (SPEA2)",
    # "spea2_ind-2":"SPEA2*",
    # "moead_ind-0":"MOEA/D (this paper)",
    # "moead_ind-2":"MOEAD*",
    # "moead_ind-1":"MOEA/D++",
    "FuzzyPSO":r"$\varepsilon$-Fuzzy PSO",
    "MODE":"MODE",
    "HBCS":"HBCS",
    "MOHEFT":"MOHEFT",
    "NSPSO":"NSPSO",
    "MOABC1":"MOABC1",
    "MOABC2":"MOABC2"
}

alg_markers = {
    "nsga2_ind-0":"m^-",
    "spea2_ind-0":"r^-",
    "nsga2_ind-1":"k+-",
    "spea2_ind-1":"r+-",
    "spea2_ind-2":"go",
    "FuzzyPSO":"bo",
    "MODE":"yo",
    "MOHEFT":"co",
    "NSPSO":"ro",
}

def update_hv_results(rs, hv, nz):
    for _, r in rs:
        r["hv_results"] = hv.compute(map(nz, r["results"]))
    return rs

def update_hv_history(rs, hv, nz):
    for _, r in rs:
        if r["ga"]:
            r["hv_history"] = [hv.compute(map(nz, g)) for g in r["pops"]]
    return rs

def min_time(rs):
    ts = [float(x[1]["time"]) for x in rs]
    return sum(ts)/len(ts)

def cbest(rs):
    rs = list(rs)
    a = max(rs, key=lambda x:x[1]["hv_results"])
    a[1]["time"] = min_time(rs)
    return a

def beDom(x, y):
    if x[0] < y[0]:
        return x[1] <= y[1]
    elif x[0] == y[0]:
        return x[1] < y[1]
    else:
        return False

def nonDom(x, xs):
    return not any([beDom(x, y) for y in xs])

def pareto_filter(y):
    xs = y["results"]
    y["results"] = [x for x in xs if nonDom(x, xs)]
    return y

def filter_best(rs, history=False):
    rs = [(x, pareto_filter(y)) for (x,y) in rs if x in alg_names ]
    nz = Normaliser(rs)
    hv = HyperVolume(ref_point)
    rs = update_hv_results(rs, hv, nz)
    k = lambda x: x[0]
    gs = groupby(sorted(rs, key=k), key=k)
    rs = [cbest(v) for _, v in gs]
    if history:
        rs = update_hv_history(rs, hv, nz)
    return rs

global f
f = False
def plot_hvs(d,rs):
    global f
    a,vs = zip(*rs)
    if not f:
        print ",".join(["Workflow"]+[alg_names[k] for k in a])
        f = True
    print ",".join([d]+[str(v["hv_results"]) for v in vs])

def plot_times(rs):
    global f
    a,vs = zip(*rs)
    if not f:
        print ",".join(["Workflow"]+[alg_names[k] for k in a])
        f = True
    print ",".join([d]+[str(v["time"]) for v in vs])

def plot_history(rs, save=None):
    fig, ax = plt.subplots()
    ax.grid(True)
    plt.xlabel('Generation')
    plt.ylabel('HyperVolume')

    lines = []
    names = []
    for k, v in rs:
        names.append(k)
        l = qCond["gennum"][0]
        if v["ga"]:
            lines.append(ax.plot(v["hv_history"])[0])
        else:
            lines.append(ax.plot([v["hv_results"]]*l)[0])


    plt.legend(lines, names, loc='lower right', numpoints=1)

    if save:
        plt.savefig(RES_PATH_HISTORY+save+".jpg", format="jpeg")
    else:
        plt.show()

def setup_mpl():
    fig_width_pt = 240  # Get this from LaTeX using \showthe\columnwidth
    inches_per_pt = 1.0/72.27               # Convert pt to inch
    golden_mean = (sqrt(5)-1.0)/2.0         # Aesthetic ratio
    fig_width = fig_width_pt*inches_per_pt  # width in inches
    fig_height = fig_width*golden_mean      # height in inches
    fig_size =  [fig_width,fig_height]
    params = {'axes.labelsize': 10,
              'legend.fontsize': 8,
              'xtick.labelsize': 10,
              'ytick.labelsize': 10,
              'text.usetex': True,
              'figure.figsize': fig_size}
    mpl.rcParams.update(params)

def plot_front(rs, save=None, eps=True):
    if eps:
        setup_mpl()
    # points = ["1", "*", ".", "+", "^", "x", "s", "o", "d", ]
    fig, ax = plt.subplots()
    # ax.grid(True)
    plt.xlabel('Time(s)')
    plt.ylabel('Cost(\$)')
    ax.set_xscale('log')
    # ax.set_yscale('log')

    lines = []
    names = []

    i = 0

    for k, v in rs:
        if "ind-0" in k: continue
        ps = sorted(v["results"], key=lambda x: x[0])
        x, y = zip(*ps)
        if eps:
            lines.append(ax.plot(x, y, alg_markers[k], ms=4, mew=0.4, lw=0.3)[0])
            # lines.append(ax.plot(x, y, "o", ms=2.5, mew=0.2, lw=0.3)[0])
        else:
            lines.append(ax.plot(x, y, ".-")[0])
        names.append(alg_names[k])
        i += 1

    for k, v in rs:
        if "ind-0" not in k: continue
        ps = sorted(v["results"], key=lambda x: x[0])
        x, y = zip(*ps)
        if eps:
            lines.append(ax.plot(x, y, alg_markers[k], ms=5, mew=0.4, lw=0.3)[0])
            # lines.append(ax.plot(x, y, "m^-", ms=2.5, mew=0.2, lw=0.3)[0])
        else:
            lines.append(ax.plot(x, y, ".-")[0])
        names.append(alg_names[k])
        i += 1

    plt.legend(lines, names, loc='upper right', numpoints=1)
    if eps:
        plt.tight_layout(pad=0.3, w_pad=0.3, h_pad=0.3)

    if save:
        if eps:
            plt.savefig(RES_PATH_FRONT+save+".eps", format="eps")
        else:
            plt.savefig(RES_PATH_FRONT+save+".jpg", format="jpeg")
    else:
        plt.show()

if __name__ == '__main__':
    for d in PEGASUS_DAGS:
    # for d in ["96", "44", "57"]:
    # for d in ["2"]:
    # for d in ["47", "24", "13"]:
    # for d in RANDAG:
        rs = filter_best(query(d, qCond), False)
        print "Plotting Pareto Front of %s..." % d
        plot_front(rs, d, False)
        plot_hvs(d, rs)
        plot_times(rs)
        # print "Plotting HV history of %s..." % d
        # plot_history(rs, d)
