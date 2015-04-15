#!/usr/bin/env python

import pandas as pd
import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.cm as cm

csv_file = "./results/pegasus/hvs.csv"

wf_names = [
        "Montage 25",
        "Montage 50",
        "Montage 100",
        "Montage 1000",
        "Epigenomics 24",
        "Epigenomics 46",
        "Epigenomics 100",
        "Epigenomics 997",
        "CyberShake 30",
        "CyberShake 50",
        "CyberShake 100",
        "CyberShake 1000",
        "Sipht 30",
        "Sipht 60",
        "Sipht 100",
        "Sipht 1000",
        "Inspiral 30",
        "Inspiral 50",
        "Inspiral 100",
        "Inspiral 1000",
]

alg_names = {
        "nsga2_ind-0":"Hookie(NSGA-II)",
        # "nsga2_ind-1":"NSGA2++",
        "nsga2_ind-2":"NSGA2*",
        "moead_ind-0":"Hookie(MOEA/D)",
        # "moead_ind-1":"MOEA/D++",
        "FuzzyPSO":"e-Fuzzy PSO",
        "MODE":"MODE",
        "HBCS":"HBCS",
        "MOHEFT":"MOHEFT",
        "NSPSO":"NSPSO",
}

def read_data(a):
        b = a + 4
        wfs = []
        with open(csv_file) as f:
                names = f.readline().strip().split(",")[1:]
                d = zip(*[map(float, line.strip().split(",")[1:]) for line in f.readlines()[a:b]])
                vs = {n:list(k) for n,k in zip(names, d)}
        return pd.DataFrame(vs, index=[x.split()[1] for x in wf_names[a:b]])

def plot_bar(df, name):
        # mpl.rcParams.update({'font.size':14, 'pdf.fonttype':42, 'ps.fonttype':42})
        ax = df.plot(kind="bar", legend=False)
        box = ax.get_position()
        plt.xticks(rotation = 0)
        plt.xlabel("Number of Tasks")
        plt.ylabel("Hypervolume")
        patches, labels = ax.get_legend_handles_labels()
        ax.legend(patches, labels, loc="lower right", ncol=2)
        epsname = "./results/pegasus/figures/%s.jgp" % name
        plt.savefig(epsname, format="jpg")

if __name__ == '__main__':
        plot_bar(read_data(0), "Montage")
        plot_bar(read_data(4), "Epigenomics")
        plot_bar(read_data(8), "Cybershake")
        plot_bar(read_data(12), "Sipht")
        plot_bar(read_data(16), "Inspiral")
