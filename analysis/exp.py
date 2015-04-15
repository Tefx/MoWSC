#!/usr/bin/env python

from itertools import product
from gevent import subprocess
from gevent.pool import Pool
from multiprocessing import cpu_count
import json as json
import couchdb
from time import time

from config import PEGASUS_DAGS, RANDAG, CouchDB_Addr, CONFIG, ALGS, DBNAME, EXP, DAG_PATH


ARGS = {
    "ind": "-i",
    "seed": "-s",
    "pcr": "-c",
    "pmu": "-m",
    "gennum": "-g",
    "popsize": "-p",
}


def build_cmd(f):
    return " ".join("%s %s" % (ARGS[k], v) for k, v in f.iteritems())


def produce_exp(wpath):
    path = DAG_PATH % wpath
    ns, c = zip(*CONFIG.iteritems())
    for i in list(product(*c)):
        for alg in ALGS:
            res = dict(zip(ns, i))
            cmd = build_cmd(res)
            res["alg"] = alg
            res["dag"] = wpath
            res["ga"] = True
            cmd = "%s %s %s %s" % (EXP, alg, path, cmd)
            yield res, cmd

def db_save(res):
    server = couchdb.Server(CouchDB_Addr)
    db = server[DBNAME]
    db.save(res)

def run_exp(exp):
    res, cmd = exp
    start = time()
    print cmd
    p = subprocess.Popen([cmd], stdout=subprocess.PIPE, shell=True)
    (out, err) = p.communicate()
    res.update(json.loads(out))
    res["time"] = time() - start
    del res["pops"]
    db_save(res)


if __name__ == '__main__':
    p = Pool(cpu_count())
    for _ in range(10):
        for w in PEGASUS_DAGS:
        # for w in RANDAG:
            for e in produce_exp(w):
                p.spawn(run_exp, e)
    p.join()
