#!/usr/bin/env python

from utils import Normaliser
from hv import HyperVolume
from config import *
from query import query
import pegasus

import matplotlib as mpl

mpl.use('Agg')
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from itertools import chain
import numpy as np

colors = ['b', 'r', 'g', 'c', 'm', 'y', 'k']
hatches = [1 * '/', 2 * '\\', 3 * '/', 4 * 'x', 5 * '.']
rts = [0.25, 0.5, 1, 2, 4, 8, 16]
legend = True

orders = ["ESC/F", "ESC/NH", "ESC/P", "MOHEFT", "NSPSO", "SPEA2*", "MOABC"]

colormaps = {
    "ESC/P":"g",
    "ESC/NH":"r",
    "ESC/F":"b",
    "SPEA2*":"c",
    "MOHEFT":"k",
    "MOABC":"y",
    "NSPSO":"m"
}

def fetch_rts(dag):
    d = {s: {} for s in rts}
    nzs = {s:Normaliser() for s in rts}
    for k, v in query(dag, ["algorithm", "runtime_scale"], "results"):
        alg, s = k.split("-")
        s = float(s)
        if alg not in d[s]:
            d[s][alg] = [v]
        else:
            d[s][alg].append(v)
        nzs[s].update(v)

    hv = HyperVolume(hv_ref)
    for s, dd in d.iteritems():
        for alg, vs in  dd.iteritems():
            v = max(hv.compute(map(nzs[s], v)) for v in vs)
            d[s][alg] = v
    res = {}
    for alg in d[rts[0]].iterkeys():
        res[alg] = [d[s][alg] for s in rts]
    return res

def fetch(dag, keys, field, initNZ=None):
    d = {}
    nz = Normaliser()

    for k, v in query(dag, keys, field):
        if k not in d:
            d[k] = [v]
        else:
            d[k].append(v)
        if initNZ:
            nz.update(initNZ(v))
    return d, nz


def front_objs(dag, keys):
    hv = HyperVolume(hv_ref)
    d, nz = fetch(dag, keys, "results", lambda x: x)
    for k, v in d.iteritems():
        d[k] = max(v, key=lambda x: hv.compute(map(nz, x)))
    return d


def budget_results(dag, keys):
    d = fetch(dag, keys, "results")[0]
    bs = fetch(dag, keys, "budgets")[0].values()[0][0]
    res = {}
    for alg, v in d.iteritems():
        res[alg] = [[0, 0] if r[1] > b else r for r, b in zip(v[0], bs)]
    return res


def best_hvs(dag, keys):
    hv = HyperVolume(hv_ref)
    d, nz = fetch(dag, keys, "results", lambda x: x)
    for k, v in d.iteritems():
        d[k] = max(hv.compute(map(nz, x)) for x in v)
    return dag, d


def best_time(dag, keys):
    d, _ = fetch(dag, keys, "time")
    for k, v in d.iteritems():
        d[k] = min(v)
    return dag, d


def evolve_history(dag, key, hv_only=True):
    hv = HyperVolume(hv_ref)
    d, nz = fetch(dag, keys, "trace", lambda x: x[-1])
    t = {}
    for k, v in d.iteritems():
        best = max(v, key=lambda x: hv.compute(map(nz, x[-1])))
        if hv_only:
            t[k] = [hv.compute(map(nz, x)) for x in best]
        else:
            t[k] = best
    return t


def dominate(x, y):
    if x[0] < y[0]:
        return x[1] <= y[1]
    elif x[0] == y[0]:
        return x[1] < y[1]
    else:
        return False


def nonDom(x, xs):
    return not any([dominate(y, x) for y in xs])


def pareto_filter(xs):
    return [x for x in xs if nonDom(x, xs)]


def plot_bar(d, save=None, legendsave=None):
    setup_mpl()
    max_1 = 0
    max_2 = 0
    for alg, res in d.iteritems():
        l1 = [x[0] for x in res]
        if "BHEFT" not in alg:
            if max(l1) > max_1:
                max_1 = max(l1)
        else:
            if max(l1) > max_2:
                max_2 = max(l1)
    max_1 = max_1 * 1.1
    h0 = max_1 / 4
    if max_2 - h0 > max_1:
        return split_plot_bar(max_1, max_2, d, save)

    fig, ax = plt.subplots()
    plt.xlabel('$k_{budget}$')
    plt.ylabel('time(s)')
    ax.locator_params(axis='y', nbins=8)

    i = 0
    j = 0
    bar_width = 0.8 / len(d)
    bars = []
    algs = []
    # for alg, res in d.iteritems():
    for alg in ["BHEFT", "LOSS2", "HBCS", "BHI/L", "BHI/E"]:
        res = d[alg]
        index = np.arange(len(res)) + 0.1
        l1 = [x[0] for x in res]
        # bar = ax.bar(index + j, l1, bar_width, color=colors[i], label=alg)[0]
        # bar = ax.bar(index + j, l1, bar_width, cmap=mpl.cm.gray, label=alg)[0]
        bar = ax.bar(index + j, l1, bar_width, color="white", ecolor="black", hatch=hatches[i], label=alg)[0]
        bars.append(bar)
        algs.append(alg)
        # plt.plot(l1, marker="+")
        j += bar_width
        i += 1

    mkfunc = lambda x, pos: '%1.1fM' % (x * 1e-6) if x >= 1e6 else '%1.1fK' % (x * 1e-3) if x >= 1e3 else '%1.1f' % x
    mkformatter = mpl.ticker.FuncFormatter(mkfunc)
    ax.yaxis.set_major_formatter(mkformatter)

    ax.set_xlim(0, len(l1))
    plt.xticks(index + 0.4, [float(x + 1) / len(l1) for x in range(len(l1))])
    plt.tight_layout()

    if save:
        print "Writing %s ..." % (figure_path_pegasus_bar + save + ".jpg")
        plt.savefig(figure_path_pegasus_bar + save + ".jpg", format="jpeg")
        if legendsave:
            legendfig, ax = plt.subplots(figsize=(16, 0.5))
            legendfig.legend(bars, algs, ncol=len(bars), loc="center")
            ax.set_visible(False)
            legendfig.savefig(figure_path_pegasus_bar + legendsave, format="jpeg")
    else:
        plt.legend()
        plt.show()


def split_plot_bar(max0, max1, d, save, legendsave=None):
    setup_mpl()
    # fig, (ax0, ax1) = plt.subplots(2, 1, sharex=True)
    fig = plt.figure()
    ax0 = plt.subplot2grid((3, 1), (0, 0), rowspan=1)
    ax1 = plt.subplot2grid((3, 1), (1, 0), rowspan=2, sharex=ax0)

    h0 = max0 / 4
    ax0.set_ylim(max1 - h0, max1 + h0)
    ax1.set_ylim(0, max0)
    ax0.locator_params(axis='y', nbins=3)
    ax1.locator_params(axis='y', nbins=5)

    i = 0
    j = 0
    bar_width = 0.8 / len(d)
    # for alg, res in d.iteritems():
    for alg in ["BHEFT", "LOSS2", "HBCS", "BHI/L", "BHI/E"]:
        res = d[alg]
        index = np.arange(len(res))
        l1 = [x[0] for x in res]
        # ax0.bar(index + j+0.1, l1, bar_width, color=colors[i], label=alg)
        # ax1.bar(index + j+0.1, l1, bar_width, color=colors[i], label=alg)
        ax0.bar(index + j + 0.1, l1, bar_width, color="white", ecolor="black", hatch=hatches[i], label=alg)
        ax1.bar(index + j + 0.1, l1, bar_width, color="white", ecolor="black", hatch=hatches[i], label=alg)
        j += bar_width
        i += 1

    ax0.spines['bottom'].set_visible(False)
    ax1.spines['top'].set_visible(False)
    ax0.xaxis.tick_top()
    ax0.tick_params(labeltop='off')  # don't put tick labels at the top
    ax1.xaxis.tick_bottom()
    ax1.set_xlim(0, len(l1))

    ax1.set_xlabel('$k_{budget}$')
    ax1.set_ylabel('time(s)')
    ax1.yaxis.set_label_coords(0.05, 0.5, transform=fig.transFigure)
    plt.xticks(index + 0.5, [float(x + 1) / len(l1) for x in range(len(l1))])

    mkfunc = lambda x, pos: '%1.1fM' % (x * 1e-6) if x >= 1e6 else '%1.1fK' % (x * 1e-3) if x >= 1e3 else '%1.1f' % x
    mkformatter = mpl.ticker.FuncFormatter(mkfunc)
    ax0.yaxis.set_major_formatter(mkformatter)
    ax1.yaxis.set_major_formatter(mkformatter)

    d = .015
    kwargs = dict(transform=ax0.transAxes, color='k', clip_on=False)
    ax0.plot((-d, +d), (0, 0), **kwargs)  # top-left diagonal
    ax0.plot((1 - d, 1 + d), (0, 0), **kwargs)  # top-right diagonal

    kwargs.update(transform=ax1.transAxes)  # switch to the bottom axes
    ax1.plot((-d, +d), (1, 1), **kwargs)  # bottom-left diagonal
    ax1.plot((1 - d, 1 + d), (1, 1), **kwargs)  # bottom-right diagonal

    plt.tight_layout()

    if save:
        print "Writing %s ..." % (figure_path_pegasus_bar + save + ".jpg")
        plt.savefig(figure_path_pegasus_bar + save + ".jpg", format="jpeg")
    else:
        plt.show()


def print_res(name, d):
    for alg, res in d.iteritems():
        res = [("%.1f" % x) if x != 0 else "-" for x, _ in res]
        print ",".join([name, alg, ",".join(res)])

def plot_rts(d, save=None):
    setup_mpl()
    fig, ax = plt.subplots()
    plt.xlabel('RT_Scale')
    plt.ylabel('Hypervolume')
    # ax.set_xscale('log')

    lines = []
    names = []

    for k in orders:
        if k not in d: continue
        v = d[k]
        lines.append(ax.plot(rts, v, "+-", color=colormaps[k])[0])
        names.append(k)

    fs = {0.25:r"$\frac{1}{4}$", 0.5:"", 1:r"$1$", 2:r"$2$", 4:r"$4$", 8:r"$8$", 16:r"$16$"}
    mkfunc = lambda x, pos: fs[x]
    mkformatter = mpl.ticker.FuncFormatter(mkfunc)
    ax.xaxis.set_major_formatter(mkformatter)
    ax.set_xlim(rts[0]-0.5, rts[-1]+0.5)
    plt.xticks(rts)

    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.yaxis.set_ticks_position('left')
    ax.xaxis.set_ticks_position('bottom')

    plt.tight_layout()
    if save:
        plt.savefig(figure_path_pegasus_rts + save + ".jpg", format="jpeg")
        plot_legend(figure_path_pegasus_rts, lines, names)
    else:
        plt.show()

def plot_front(d, save=None, pareto=True):
    setup_mpl()
    fig, ax = plt.subplots()
    plt.xlabel('Time(s)')
    plt.ylabel('Cost(\$)')
    ax.set_xscale('log')
    # ax.set_yscale('log')

    lines = []
    names = []

    for k in orders:
        if k not in d: continue
        v = d[k]
        if pareto:
            v = pareto_filter(v)
        ps = sorted(v, key=lambda x: (x[0], x[1]))
        x, y = zip(*ps)
        if pareto:
            mark = ".-"
        else:
            mark = "."
        lines.append(ax.plot(x, y, mark, color=colormaps[k], lw=0.5)[0])
        names.append(k)

    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.yaxis.set_ticks_position('left')
    ax.xaxis.set_ticks_position('bottom')
    ax.set_ylim(0, 100)

    plt.tight_layout()
    if save:
        plt.savefig(figure_path_pegasus_plot + save + ".jpg", format="jpeg")
        plot_legend(figure_path_pegasus_plot, lines, names)
    else:
        plt.show()

def plot_legend(path, lines, names, single=False):
    global legend
    if legend:
            if single:
                legendfig, ax = plt.subplots(figsize=(4, 3))
                legendfig.legend(lines, names, loc="upper center", numpoints=1)
            else:
                legendfig, ax = plt.subplots(figsize=(16, 0.5))
                legendfig.legend(lines, names, loc="center", ncol=len(lines), numpoints=1)
            ax.set_visible(False)
            legendfig.savefig(path + "legend.jpg", format="jpeg")
            legend = False

def plot_trace(d, dag, save=None):
    setup_mpl()
    fig, ax = plt.subplots()
    plt.xlabel('Generation')
    plt.ylabel('HyperVolume')
    n = int(dag.split("_")[1])*10

    lines = []
    names = []

    for k in orders:
        if k not in d: continue
        v = d[k]
        lines.append(ax.plot(v, color=colormaps[k])[0])
        names.append(k)

    mkformatter = mpl.ticker.FuncFormatter(lambda x, _:int(x*n/50))
    ax.xaxis.set_major_formatter(mkformatter)
    ax.set_xlim(0, 50)
    ax.set_ylim(0, 1.1)

    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.yaxis.set_ticks_position('left')
    ax.xaxis.set_ticks_position('bottom')

    plt.tight_layout()
    if save:
        plt.savefig(figure_path_pegasus_trace + save + ".jpg", format="jpeg")
        plot_legend(figure_path_pegasus_trace, lines, names)
    else:
        plt.show()


def show_trace(d, save=None):
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')

    ci = 0
    for k, ps in d.iteritems():
        f = lambda i, ps: [(i, x, y) for x, y in ps]
        ps = [f(i, x) for i, x in zip(range(len(ps)), ps)]
        ps = list(chain(*ps))
        zs, xs, ys = zip(*ps)
        print k
        ax.scatter(xs, ys, zs, marker=".", color=colors[ci])
        ci += 1

    ax.set_xlabel('Time')
    ax.set_ylabel('Cost')
    ax.set_zlabel('Gen')

    plt.show()


def setup_mpl():
    fig_width_pt = 240  # Get this from LaTeX using \showthe\columnwidth
    inches_per_pt = 1.0/72.27               # Convert pt to inch
    fig_width = fig_width_pt*inches_per_pt  # width in inches
    fig_height = fig_width_pt*inches_per_pt * 0.9 # height in inches
    fig_size =  [fig_width,fig_height]
    params = {'figure.figsize': fig_size}
    mpl.rcParams.update(params)

def collect_hvs(app):
    nums = []
    algs = None
    for w in pegasus.genWorkflow(app=[app]):
        dag, hvs = best_hvs(w["dag"], ["algorithm"])
        num = int(dag.split("_")[1])
        nums.append(num)
        if not algs:
            algs = hvs.keys()
            d = {a:[] for a in algs}
        for a in algs:
            d[a].append(hvs.get(a, 0))
    return nums, d


def plot_hvs(ds, save=None):
    setup_mpl()
    ns, d = ds
    fig, ax = plt.subplots()
    plt.xlabel('Number of Tasks')
    plt.ylabel('Hypervolume')
    ax.set_xscale('log')

    lines = []
    names = []

    for k in orders:
        if k not in d: continue
        v = d[k]
        if v[-1] == 0:
            lines.append(ax.plot(ns[:-1], v[:-1], "+-")[0])
        else:
            lines.append(ax.plot(ns, v, "+-", color=colormaps[k])[0])
        names.append(k)

    mkformatter = mpl.ticker.FuncFormatter(lambda x, _:x)
    ax.xaxis.set_major_formatter(mkformatter)
    ax.set_xlim(ns[0]*0.9, ns[-1]*1.1)
    plt.xticks(ns)

    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)
    ax.yaxis.set_ticks_position('left')
    ax.xaxis.set_ticks_position('bottom')

    plt.tight_layout()
    if save:
        plt.savefig(figure_path_pegasus_hvs + save + ".jpg", format="jpeg")
        plot_legend(figure_path_pegasus_hvs, lines, names, True)
    else:
        plt.show()

if __name__ == '__main__':
    from sys import argv
    # keys = ["algorithm", "pop_size"]
    keys = ["algorithm"]
    legend = True;
    for w in pegasus.genWorkflow(task_number=["default"]):
        dag = w["dag"]
        if argv[1] == "time":
            print best_time(dag, keys)
        elif argv[1] == "hv":
            print best_hvs(dag, keys)
        elif argv[1] == "trace":
            plot_trace(evolve_history(dag, keys), dag, dag)
        elif argv[1] == "plot":
            plot_front(front_objs(dag, keys), dag)
        elif argv[1] == "plot_d":
            plot_front(front_objs(dag, keys), dag, False)
        elif argv[1] == "bar":
            plot_bar(budget_results(dag, keys), dag, "reallegend.jpg")
        elif argv[1] == "pb":
            print_res(dag, budget_results(dag, keys))
        elif argv[1] == "rts":
            plot_rts(fetch_rts(dag), dag)

    if argv[1] == "hv_lines":
        for app in pegasus.APPs:
            plot_hvs(collect_hvs(app), app)

