#!/usr/bin/env python

import config
import pegasus
import analysis

import matplotlib as mpl
# mpl.use('Agg')
import matplotlib.pyplot as plt
import numpy as np

def norm(x,x_max,x_min): 
	if not x:
		return None
	elif x_max == x_min:
		return 0
	else:
		return (x-x_min) / (x_max-x_min)

def fetch_info(app=pegasus.APPs[1], task_number=["default"]):
	keys = ["algorithm"]
	norm_res = {}

	for w in pegasus.genWorkflow(app, task_number):
		base_time, _ = analysis.fetch(w["dag"], keys, "heftRes")[0].values()[0][0]
		budgets = analysis.fetch(w["dag"], keys, "budgets")[0].values()[0][0]
		res = analysis.fetch(w["dag"], keys, "results")[0]
		k = w["task_number"]
		for alg, r in res.iteritems():
			if alg not in norm_res: 
				norm_res[alg] = {k:[] for k in pegasus.TASK_NUMBERS}
			for x, b in zip(r[0], budgets):
				if x[1] <= b:
					norm_res[alg][k].append(x[0])
				else:
					norm_res[alg][k].append(None)

		for i in range(len(budgets)):
			l = [norm_res[alg][k][i] for alg in norm_res.keys() if norm_res[alg][k][i] != None]
			# t_max = max(l) if l != [] else None
			t_min = min(l) if l != [] else None
			for alg in norm_res.keys():
				if norm_res[alg][k][i] != None:
					norm_res[alg][k][i] = 1 if norm_res[alg][k][i] == t_min else 0
				# norm(norm_res[alg][k][i], t_max, t_min)

	return norm_res

def compute_sr(res, by="k"):
	cmpt = lambda l: 1 - float(l.count(None)) / float(len(l))
	# print "  By %s:" % ("Deadline" if by == "k" else "Task Numbers")
	ret = {}
	for alg in res.keys():
		if by == "k":
			# print "   %s: %s" % (alg, map(cmpt, zip(*res[alg].values())))
			ret[alg] = map(cmpt, zip(*res[alg].values()))

		elif by == "n":
			# print "   %s: %s" % (alg, [cmpt(res[alg][nt]) for nt in pegasus.TASK_NUMBERS])
			ret[alg] = [cmpt(res[alg][nt]) for nt in pegasus.TASK_NUMBERS]
	return ret

def compute_nm(res, by="k"):
	ff = lambda l: [x for x in l if x != None]
	avg = lambda l: float(sum(l)) / float(len(l)) if l != [] else None
	top_rank = lambda l: 0 if len(l)==0 else (sum(l) / float(len(l)))

	ret = {}
	for alg in res.keys():
		if by == "k":
			ret[alg] = map(sum, map(ff, zip(*res[alg].values())))
		elif by == "n":
			ret[alg] = [sum(ff(res[alg][nt])) for nt in pegasus.TASK_NUMBERS]
	return ret

	# fmax = lambda l: max(l) if l != [] else None
	# fmin = lambda l: min(l) if l != [] else None
	# print "  By %s:" % ("Deadline" if by == "k" else "Task Numbers")
	# for fn, f in [("avg", avg), ("max", fmax), ("min", fmin)]:
	# 	print "   %s:" % fn
	# 	for alg in res.keys():
	# 		if by == "k":
	# 			print "    %s :%s" % (alg, map(f, map(ff, zip(*res[alg].values()))))
	# 		elif by == "n":
	# 			print "    %s :%s" % (alg, [f(ff(res[alg][nt])) for nt in pegasus.TASK_NUMBERS])

def plot_line(d, by="k", save=None):
	fig, ax = plt.subplots()
	plt.ylabel('Success Ratio')
	if by == "k":
		plt.xlabel('k_budget')
	else:
		plt.xlabel('Number of Tasks')

	lines = []
	names = []
	
	for alg, res in d.iteritems():
		if by == "k":
			x = list(genK(len(res)))
		else:
			x = pegasus.TASK_NUMBERS
		lines.append(ax.plot(x, res, "x-")[0])
		names.append(alg)

	# plt.xticks(index + 0.35, bs)
	plt.legend(lines, names, loc='lower right', numpoints=1)

	if save:
		path = "./results/budget/%s_%s.jpg" % (save, by)
		print "Writing %s..." % path
		plt.savefig(path, format="jpeg")
	else:
		plt.show()

def set_box_color(bp, color):
    plt.setp(bp['boxes'], color=color)
    plt.setp(bp['whiskers'], color=color)
    plt.setp(bp['caps'], color=color)
    plt.setp(bp['medians'], color=color)

def plot_bar(d, by="k", y="Success Ratio", save=None):
	fig, ax = plt.subplots()
	plt.ylabel(y)
	prefix = y.strip().split()[0]
	if by == "k":
		plt.xlabel('k_budget')
		prefix = prefix + "_budget"
	else:
		plt.xlabel('Number of Tasks')
		prefix = prefix + "_numtask"

	i = 0
	j = 0
	colors = ['b', 'r', 'g', 'c', 'm', 'y', 'k']
	bar_width = 0.7 / len(d)

	if by == "k":
		x = list(genK(len(d.values()[0])))
	else:
		x = pegasus.TASK_NUMBERS
	
	for alg, res in d.iteritems():
		index = np.arange(len(res))
		plt.bar(index + j, res, bar_width, color=colors[i], label=alg)
		j += bar_width
		i += 1

	plt.xticks(index + 0.35, x)
	plt.ylim(ymin=0, ymax=1.1)
	# plt.legend(loc='lower right')
	plt.tight_layout()

	if save:
		path = "./results/budget/%s_%s.jpg" % (prefix, save)
		print "Writing %s..." % path
		plt.savefig(path, format="jpeg")
	else:
		plt.show()

def genK(num):
	for x in range(num):
		yield float(x+1) / num

def plot_box(d, by="k", save=None):
	fig, ax = plt.subplots()
	plt.ylabel('Normalised Makespan')
	ax.grid(True)
	# ax.set_xscale('log')

	i = 0
	j = 0
	colors = ['b', 'r', 'g', 'c', 'm', 'y', 'k']
	width = 0.7 / len(d)

	if by == "k":
		x = list(genK(len(d.values()[0])))
	else:
		x = pegasus.TASK_NUMBERS
	
	for alg, res in d.iteritems():
		index = np.arange(len(res))
		b = ax.boxplot(res, positions=index + j, widths=width*0.6)
		set_box_color(b, colors[i])
		plt.plot([], colors[i], label=alg)
		j += width
		i += 1

	plt.xticks(index + 0.35, x)
	# plt.legend()

	if save:
		path = "./results/budget/%s_%s.jpg" % (save, by)
		print "Writing %s..." % path
		plt.savefig(path, format="jpeg")
	else:
		plt.show()

def print_sr(res, by, app):
	print app
	if by == "k":
		print "k", " ".join(map(str, genK(len(res.values()[0]))))
	else:
		print "n", " ".join(map(str, pegasus.TASK_NUMBERS))

	for k,v in res.iteritems():
		print k, " ".join(["{:.0%}".format(x) for x in v])

def print_nm(res, by, app):
	if by == "k":
		print "alg", "k", " ".join(map(str, genK(len(res.values()[0]))))
	else:
		print "alg", "n", " ".join(map(str, pegasus.TASK_NUMBERS))

	for k,v in res.iteritems():
		print app, k, " ".join(map(str, v))

def setup_mpl():
    fig_width_pt = 280  # Get this from LaTeX using \showthe\columnwidth
    inches_per_pt = 1.0/72.27               # Convert pt to inch
    fig_width = fig_width_pt*inches_per_pt  # width in inches
    fig_height = fig_width_pt*inches_per_pt * 0.8 # height in inches
    fig_size =  [fig_width,fig_height]
    params = {'figure.figsize': fig_size}
    mpl.rcParams.update(params)

if __name__ == '__main__':
	setup_mpl()
	for app in pegasus.APPs:
		res = fetch_info(app=app, task_number=pegasus.TASK_NUMBERS)
		# plot_bar(compute_sr(res, "k"), "k", "Success Rate", app)
		# plot_bar(compute_sr(res, "n"), "n", "Success Rate", app)
		# plot_bar(compute_nm(res, "k"), "k", "Ranking Count", app)
		# plot_bar(compute_nm(res, "n"), "n", "Ranking COunt", app)

		# print_sr(compute_sr(res, "k"), "k", app)
		print_nm(compute_nm(res, "k"), "k", app)
