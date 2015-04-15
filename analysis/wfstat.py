#!/usr/bin/env python

from config import PEGASUS_DAGS, DAG_PATH
from gevent import subprocess

def stat_rand(i):
    PATH = "resources/workflow/random_dot/%d.dot"
    with open(PATH%i) as f:
        f.readline()
        l = f.readline().strip().split()
        num_task = l[3]
        jump = l[5]
        regular = l[7]
        fat = l[9]
        density = l[11]
    return ",".join([str(i), num_task, jump, regular, fat, density])

def stat_rands():
    print "#,num_task,jump,regular,fat,density"
    for i in range(100):
        print stat_rand(i)

def stat_wf():
    cmd = "../dist/build/hookie-stat/hookie-stat %s -i %s"
    fields = {"time" : lambda x:"%.2fs" % float(x),
              "data" : lambda x:"%.2fMB" % (float(x)/1024/1024), 
              "countData": lambda x:x.strip()}
    print ",".join(["Workflow"]+fields.keys())
    for w in PEGASUS_DAGS:
        path = DAG_PATH % w
        fs = []
        for f,v in fields.iteritems():
            p = subprocess.Popen([cmd % (path, f)], stdout=subprocess.PIPE, shell=True)
            (out, err) = p.communicate()
            fs.append(v(out))
        print ",".join([w]+fs)

if __name__ == "__main__":
	stat_wf()
