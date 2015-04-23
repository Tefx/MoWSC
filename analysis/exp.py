#!/usr/bin/env python

from config import exp_cmd, arg_tags, args_defaults, exp_defaults, experiments
from config import dag_pegasus, dag_pegasus_path
from config import db_addr, db_name

from copy import deepcopy
from time import time
from gevent import subprocess
from couchdb import Server as CouchDBServer, PreconditionFailed
from gevent.pool import Pool
from multiprocessing import cpu_count

def gen_cmd(dag, name, conf):
	ec = deepcopy(exp_defaults)
	ac = deepcopy(args_defaults)
	ec.update(conf)
	ac.update(ec["args"])
	ec["args"] = ac
	args = " ".join([arg_tags[k] % v for k, v in ec["args"].iteritems()])
	ec["args"]["algorithm"] = name
	ec["args"]["dag"] = dag
	cmd = "%s %s %s %s" % (exp_cmd, ec["cmd"], args, dag_pegasus_path % dag)
	for _ in range(ec["times"]):
		yield cmd, ec["args"]

def run_exp(cmd, conf):
	start = time()
	print cmd
	p = subprocess.Popen([cmd], stdout=subprocess.PIPE, shell=True)
	(out, err) = p.communicate()
	res = {
		"results" : [map(float, x.strip().split()) for x in out.splitlines()],
		"time"	  : time() - start,
		}
	res.update(conf)
	db_save(res)

def db_save(res):
    server = CouchDBServer(db_addr)
    db = server[db_name]
    db.save(res)

def init_db():
    server = CouchDBServer(db_addr)
    try:
        server.create(db_name)
    except PreconditionFailed:
        del server[db_name]
        server.create(db_name)

if __name__ == '__main__':
	from sys import argv

	if len(argv) > 1:
		if argv[1] == "initdb":
			print "Initialising database.."
			init_db()
	else:
		pool = Pool(cpu_count())
		for dag in dag_pegasus:
			for exp, conf in experiments.iteritems():
				for cmd, conf in gen_cmd(dag, exp, conf):
					pool.spawn(run_exp, cmd, conf)
		pool.join()

