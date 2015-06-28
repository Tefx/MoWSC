#!/usr/bin/env python

import config
import pegasus
import analysis

def fetch_info(app=pegasus.APPs[1], task_number=["default"]):
	keys = ["algorithm"]
	norm_res = {}

	for w in pegasus.genWorkflow(app, task_number):
		base_time, _ = analysis.fetch(w["dag"], keys, "heftRes")[0].values()[0][0]
		budgets = analysis.fetch(w["dag"], keys, "budgets")[0].values()[0][0]
		res = analysis.fetch(w["dag"], keys, "results")[0]
		k = w["task_number"]
		for alg, r in res.iteritems():
			if alg not in norm_res: norm_res[alg] = {}
			norm_res[alg][k] = []
			for x, b in zip(r[0], budgets):
				if x[1] <= b:
					norm_res[alg][k].append(x[0]/base_time)
				else:
					norm_res[alg][k].append(None)

	return norm_res

def compute_sr(res, by="k"):
	cmpt = lambda l: 1 - float(l.count(None)) / float(len(l))
	print "  By %s:" % ("Deadline" if by == "k" else "Task Numbers")
	for alg in res.keys():
		if by == "k":
			print "   %s: %s" % (alg, map(cmpt, zip(*res[alg].values())))
		elif by == "n":
			print "   %s: %s" % (alg, [cmpt(res[alg][nt]) for nt in pegasus.TASK_NUMBERS])

def compute_nm(res, by="k"):
	avg = lambda l: float(sum(l)) / float(len(l))
	ff = lambda l: [x for x in l if x]
	print "  By %s:" % ("Deadline" if by == "k" else "Task Numbers")
	for fn, f in [("avg", avg), ("max", max), ("min", min)]:
		print "   %s :" % fn
		for alg in res.keys():
			if by == "k":
				print "    %s :%s" % (alg, map(f, map(ff, zip(*res[alg].values()))))

if __name__ == '__main__':
	for app in pegasus.APPs:
		print "Application %s:" % app
		res = fetch_info(app=app, task_number=pegasus.TASK_NUMBERS)
		print " Success Rates:"
		compute_sr(res, "k")
		compute_sr(res, "n")
		print " Normalised Makespan:"
		compute_nm(res, "k")
		print