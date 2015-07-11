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
	fig, ax = plt.subplots()
	plt.xlabel('Budget Ratio')
	plt.ylabel('Time(s)')

	i = 0
	j = 0
	colors = ['b', 'r', 'g', 'c', 'm', 'y', 'k']
	bar_width = 0.7 / len(d)
	
	for alg, res in d.iteritems():
		index = np.arange(len(res))
		l1 = [x[0] for x in res]
		plt.bar(index + j, l1, bar_width, color=colors[i], label=alg)
		j += bar_width
		i += 1

	plt.xticks(index + 0.35, range(len(d)))
	plt.legend()

	if save:
		print "Writing %s ..." % (figure_path_pegasus_bar+save+".jpg")
		plt.savefig(figure_path_pegasus_bar+save+".jpg", format="jpeg")
	else:
		plt.show()

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

if __name__ == '__main__':
	from sys import argv
	keys = ["algorithm", "pop_size"]
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
				plot_bar(budget_results(dag, keys), dag)
