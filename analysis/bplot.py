#!/usr/bin/env python

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
import os.path


def read_budget(path):
    with open(path) as f:
        ls = f.readlines()
    return map(lambda x: float(x.strip()), ls)


def read_files(path):
    with open(path) as f:
        ls = f.readlines()
    return os.path.basename(path).split(".")[0], \
           [map(float, x.strip().split()) for x in ls]


def f(b, x):
    if b >= x[1]:
        return x[0]
    else:
        return 0


def plot(bs, ls):
    fig, ax = plt.subplots()
    ax.grid(True)

    index = np.arange(len(bs))
    bar_width = 0.7 / len(ls)

    plt.xlabel('Budget')
    plt.ylabel('Time')

    d = 0
    i = 0
    colors = ['b', 'r', 'g', 'c', 'm', 'y', 'k']
    for n, l in ls:
        l1 = [f(b, x) for b, x in zip(bs, l)]
        plt.bar(index + d, l1, bar_width, color=colors[i], label=n)
        d += bar_width
        i += 1

    plt.xticks(index + 0.35, bs)
    plt.legend()
    plt.show()


if __name__ == '__main__':
    from sys import argv

    bs = read_budget(argv[1])
    ls = map(read_files, argv[2:])
    plot(bs, ls)
