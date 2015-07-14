#!/usr/bin/env python

from utils import Normaliser
from hv import HyperVolume
from config import hv_ref, figure_path_pegasus_plot, figure_path_pegasus_trace, dag_pegasus, figure_path_pegasus_bar
from query import query
import pegasus

import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
from itertools import chain
import numpy as np

colors = "bgrcmykw"

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
	d, nz = fetch(dag, keys, "results", lambda x:x)
	for k, v in d.iteritems():
		d[k] = max(v, key=lambda x: hv.compute(map(nz, x)))
	return d

def budget_results(dag, keys):
	d = fetch(dag, keys, "results")[0]
	bs = fetch(dag, keys, "budgets")[0].values()[0][0]
	res = {}
	for alg, v in d.iteritems():
		res[alg] = [[0,0] if r[1]>b else r for r,b in zip(v[0], bs)]
	return res

def best_hvs(dag, keys):
	hv = HyperVolume(hv_ref)
	d, nz = fetch(dag, keys, "results", lambda x:x)
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
	d, nz = fetch(dag, keys, "trace", lambda x:x[-1])
	t = {}
	for k, v in d.iteritems():
		best = max(v, key=lambda x:hv.compute(map(nz, x[-1])))
		if hv_only:
			t[k]=[hv.compute(map(nz, x)) for x in best]
		else:
			t[k] = best
	return t

def beDom(x, y):
	if x[0] < y[0]:
		return x[1] <= y[1]
	elif x[0] == y[0]:
		return x[1] < y[1]
	else:
		return False

def nonDom(x, xs):
	return not any([beDom(x, y) for y in xs])

def pareto_filter(xs):
	return [x for x in xs if nonDom(x, xs)]

def plot_bar(d, save=None):
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
	plt.xlabel('Budget Ratio')
	plt.ylabel('Time(s)')
	ax.locator_params(axis = 'y', nbins = 9)

	i = 0
	j = 0
	colors = ['b', 'r', 'g', 'c', 'm', 'y', 'k']
	bar_width = 0.7 / len(d)
	for alg, res in d.iteritems():
		index = np.arange(len(res))
		l1 = [x[0] for x in res]
		plt.bar(index + j, l1, bar_width, color=colors[i], label=alg)
		# plt.plot(l1, marker="+")
		j += bar_width
		i += 1

	# plt.ylim(ymin=1, ymax=max_1)
	plt.xticks(index + 0.35, [float(x+1)/len(l1) for x in range(len(l1))])
	# plt.legend()
	plt.tight_layout()


	if save:
		print "Writing %s ..." % (figure_path_pegasus_bar+save+".jpg")
		plt.savefig(figure_path_pegasus_bar+save+".jpg", format="jpeg")
	else:
		plt.show()

def split_plot_bar(max0, max1, d, save):
	# fig, (ax0, ax1) = plt.subplots(2, 1, sharex=True)
	fig = plt.figure()
	ax0 = plt.subplot2grid((3, 1), (0, 0), rowspan=1)
	ax1 = plt.subplot2grid((3, 1), (1, 0), rowspan=2, sharex=ax0)

	h0 = max0 / 4
	ax0.set_ylim(max1-h0, max1+h0)
	ax1.set_ylim(0, max0)
	ax0.locator_params(axis = 'y', nbins = 3)
	ax1.locator_params(axis = 'y', nbins = 6)

	i = 0
	j = 0
	colors = ['b', 'r', 'g', 'c', 'm', 'y', 'k']
	bar_width = 0.7 / len(d)
	for alg, res in d.iteritems():
		index = np.arange(len(res))
		l1 = [x[0] for x in res]
		ax0.bar(index + j, l1, bar_width, color=colors[i], label=alg)
		ax1.bar(index + j, l1, bar_width, color=colors[i], label=alg)
		j += bar_width
		i += 1

	ax0.spines['bottom'].set_visible(False)
	ax1.spines['top'].set_visible(False)
	ax0.xaxis.tick_top()
	ax0.tick_params(labeltop='off') # don't put tick labels at the top
	ax1.xaxis.tick_bottom()

	ax1.set_xlabel('k_Budget')
	plt.ylabel('Time(s)')
	ax1.yaxis.set_label_coords(0.05, 0.5, transform=fig.transFigure)
	plt.xticks(index + 0.35, [float(x+1)/len(l1) for x in range(len(l1))])
	plt.subplots_adjust(hspace=0.01)

	d = .015
	kwargs = dict(transform=ax0.transAxes, color='k', clip_on=False)
	ax0.plot((-d,+d),(-d*2,+d*2), **kwargs)      # top-left diagonal
	ax0.plot((1-d,1+d),(-d*2,+d*2), **kwargs)    # top-right diagonal

	kwargs.update(transform=ax1.transAxes)  # switch to the bottom axes
	ax1.plot((-d,+d),(1-d, 1+d), **kwargs)   # bottom-left diagonal
	ax1.plot((1-d,1+d),(1-d, 1+d), **kwargs) # bottom-right diagonal

	plt.tight_layout()

	if save:
		print "Writing %s ..." % (figure_path_pegasus_bar+save+".jpg")
		plt.savefig(figure_path_pegasus_bar+save+".jpg", format="jpeg")
	else:
		plt.show()

def print_res(name, d):
	for alg, res in d.iteritems():
		res = [("%.1f" % x) if x != 0 else "-" for x,_ in res]
		print ",".join([name, alg, ",".join(res)])

def plot_front(d, save=None, pareto=True):
	fig, ax = plt.subplots()
	plt.xlabel('Time(s)')
	plt.ylabel('Cost(\$)')
	ax.set_xscale('log')
	# ax.set_yscale('log')

	lines = []
	names = []

	for k, v in d.iteritems():
		if pareto:
			v = pareto_filter(v)
		ps = sorted(v, key=lambda x: (x[0], x[1]))
		x, y = zip(*ps)
		if pareto:
			mark = ".-"
		else:
			mark = "."
		lines.append(ax.plot(x, y, mark)[0])
		names.append(k)

	plt.legend(lines, names, loc='upper right', numpoints=1)
	if save:
		plt.savefig(figure_path_pegasus_plot+save+".jpg", format="jpeg")
	else:
		plt.show()

def plot_trace(d, save=None):
	fig, ax = plt.subplots()
	plt.xlabel('Generation')
	plt.ylabel('HyperVolume')

	lines = []
	names = []

	for k, v in d.iteritems():
		lines.append(ax.plot(v)[0])
		names.append(k)

	plt.legend(lines, names, loc='lower right')
	if save:
		plt.savefig(figure_path_pegasus_trace+save+"_trace.jpg", format="jpeg")
	else:
		plt.show()

def show_trace(d, save=None):
	fig = plt.figure()
	ax = fig.add_subplot(111, projection='3d')

	ci = 0
	for k, ps in d.iteritems():
		f = lambda i, ps:[(i, x, y) for x, y in ps]
		ps = [f(i, x) for i,x in zip(range(len(ps)), ps)]
		ps = list(chain(*ps))
		zs, xs, ys= zip(*ps)
		print k
		ax.scatter(xs, ys, zs, marker=".", color=colors[ci])
		ci += 1

	ax.set_xlabel('Time')
	ax.set_ylabel('Cost')
	ax.set_zlabel('Gen')

	plt.show()

def setup_mpl():
    fig_width_pt = 280  # Get this from LaTeX using \showthe\columnwidth
    inches_per_pt = 1.0/72.27               # Convert pt to inch
    fig_width = fig_width_pt*inches_per_pt  # width in inches
    fig_height = fig_width_pt*inches_per_pt * 0.8 # height in inches
    fig_size =  [fig_width,fig_height]
    params = {'figure.figsize': fig_size}
    mpl.rcParams.update(params)

if __name__ == '__main__':
	from sys import argv
	# keys = ["algorithm", "pop_size"]
	keys = ["algorithm"]
	for w in pegasus.genWorkflow(task_number=["default"]):
		dag = w["dag"]
		if argv[1] == "time":
			print best_time(dag, keys)
		elif argv[1] == "hv":
			print best_hvs(dag, keys)
		elif argv[1] == "trace":
			plot_trace(evolve_history(dag, keys), dag)
		elif argv[1] == "plot":
			plot_front(front_objs(dag, keys), dag)
		elif argv[1] == "plot_d":
			plot_front(front_objs(dag, keys), dag, False)
		elif argv[1] == "bar":
			setup_mpl()
			plot_bar(budget_results(dag, keys), dag)
		elif argv[1] == "pb":
			print_res(dag, budget_results(dag, keys))
