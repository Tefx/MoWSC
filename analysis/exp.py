#!/usr/bin/env python2

from config import exp_cmd, arg_tags, args_defaults, exp_defaults, experiments
from config import dag_pegasus, dag_pegasus_path
from config import db_addr, db_name
import pegasus

from copy import deepcopy
from time import time
from gevent import subprocess
from couchdb import Server as CouchDBServer, PreconditionFailed
from gevent.pool import Pool
from multiprocessing import cpu_count
import json as json

def gen_cmd(workflow, name, conf):
    ec = deepcopy(exp_defaults)
    ac = deepcopy(args_defaults)
    ec.update(conf)
    ac.update(ec["args"])
    ec["args"] = ac
    args = " ".join([arg_tags[k] % v for k, v in ec["args"].iteritems()])
    ec["args"]["algorithm"] = name
    ec["args"].update(workflow)
    del ec["args"]["path"]
    path = pegasus.BASE_TINY_PATH % (workflow["path"], workflow["dag"])
    cmd = "%s %s %s %s" % (exp_cmd, ec["cmd"], args, path)
    for _ in range(ec["times"]):
        yield cmd, ec["args"]

def run_exp(cmd, conf):
    start = time()
    print cmd
    p = subprocess.Popen([cmd], stdout=subprocess.PIPE, shell=True)
    (out, err) = p.communicate()
    res = json.loads(out)
    if res["extra"] != []:
        res.update(res["extra"])
    del res["extra"]
    res["time"] = time() - start
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
        pool = Pool(2)
        for w in pegasus.genWorkflow(task_number=pegasus.TASK_NUMBERS):
            for exp, conf in experiments.iteritems():
                for cmd, conf in gen_cmd(w, exp, conf):
                    pool.spawn(run_exp, cmd, conf)
        pool.join()
