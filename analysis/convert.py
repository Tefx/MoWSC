#!/usr/bin/env python2

from gevent import subprocess
from config import dag_pegasus, dax_pegasus_path, dag_pegasus_path

cmd = "../dist/build/MoWSC-convert/MoWSC-convert %s"

for g in dag_pegasus:
    orig = dax_pegasus_path % g
    goal = dag_pegasus_path % g
    print "%s -> %s" % (orig, goal)
    p = subprocess.Popen([cmd % orig], stdout=subprocess.PIPE, shell=True)
    (out, _) = p.communicate()
    with open(goal, "w") as f:
        f.write(out)
