#!/usr/bin/env python2
import matplotlib.pyplot as plt
import networkx as nx
from json import load

def read_and_plot(path, path2):
    with open(path) as f:
        data = load(f)
    dag = nx.DiGraph()
    for i in range(len(data)):
        for j in  [x[0] for x in data[i][1]]:
            dag.add_edge(j, i)

    nx.write_dot(dag, path2)

if __name__ == "__main__":
    from sys import argv
    read_and_plot(argv[1], argv[2])
