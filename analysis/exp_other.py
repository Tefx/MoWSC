#!/usr/bin/env python

from gevent import subprocess
from gevent.pool import Pool
from multiprocessing import cpu_count
import json as json
import couchdb
from time import time

from config import CONFIG, EXP, DAG_PATH, PEGASUS_DAGS, RANDAG, CouchDB_Addr, OALGS, DBNAME


def produce_exp(wpath):
    path = DAG_PATH % wpath
    for a,f in OALGS.iteritems():
        cmd = "%s %s %s %s" % (EXP, f[0], f[1], path)
        res = {"alg":a, "dag":wpath, "ga":False}
        yield res, cmd


def parse_res(res):
    l = [map(float, x.strip().split()) for x in res.splitlines()]
    return {"results":l}

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
    res.update(parse_res(out))
    res["time"] = time() - start
    db_save(res)


if __name__ == '__main__':
    p = Pool(cpu_count())
    for w in PEGASUS_DAGS:
    # for w in RANDAG:
        for e in produce_exp(w):
            p.spawn(run_exp, e)
    p.join()
