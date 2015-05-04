#!/usr/bin/env python

from utils import Normaliser
from hv import HyperVolume
from config import hv_ref, figure_path_pegasus_plot, figure_path_pegasus_trace, dag_pegasus
from query import query

import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt

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

def evolve_history(dag, key):
	hv = HyperVolume(hv_ref)
	d, nz = fetch(dag, keys, "trace", lambda x:x[-1])
	t = {}
	for k, v in d.iteritems():
		best = max(v, key=lambda x:hv.compute(map(nz, x[-1])))
		t[k]=[hv.compute(map(nz, x)) for x in best]
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

def plot_front(d, save=None):
	fig, ax = plt.subplots()
	plt.xlabel('Time(s)')
	plt.ylabel('Cost(\$)')
	ax.set_xscale('log')
	# ax.set_yscale('log')

	lines = []
	names = []

	for k, v in d.iteritems():
		ps = sorted(pareto_filter(v), key=lambda x: (x[0], x[1]))
		x, y = zip(*ps)
		lines.append(ax.plot(x, y, ".-")[0])
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

if __name__ == '__main__':
	from sys import argv
	keys = ["algorithm", "pop_size"]
	if argv[1] == "time":
		for dag in dag_pegasus:
			print best_time(dag, keys)
	elif argv[1] == "hv":
		for dag in dag_pegasus:
			print best_hvs(dag, keys)
	elif argv[1] == "trace":
		for dag in dag_pegasus:
			plot_trace(evolve_history(dag, keys), dag)
	elif argv[1] == "plot":
		for dag in dag_pegasus:
			plot_front(front_objs(dag, keys), dag)


