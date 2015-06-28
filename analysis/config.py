exp_cmd = "../dist/build/MoWSC-exp/MoWSC-exp"

arg_tags = {
    "pop_size" : "-p %d",
    "gen_num" : "-g %d",
    "prob_xo" : "-c %f",
    "prob_mu" : "-m %f",
}

args_defaults = {
    "pop_size" : 20,
    "gen_num" : 1000,
    "prob_xo" : 1,
    "prob_mu" : 1,
}

exp_defaults = {
    "times" : 4,
    "args" : {}
}

experiments = {
    # "SPEA2-C0":{"cmd":"spea2_c0",},
    # "SPEA2-C3":{"cmd":"spea2_c3",},
    # "SPEA2-C5":{"cmd":"spea2_c5",},
    # "NSGA2-C3":{"cmd":"nsga2_c3"},
    # "MOHEFT":{"times":1,"cmd":"moheft"},
    "LOSS2":{"times":1,"cmd":"loss2"},
    # "LOSS3":{"times":1,"cmd":"loss3"},
    # "BHEFT":{"times":1,"cmd":"bheft"},
    # "HBCS":{"times":1,"cmd":"hbcs"},
    # "EBCS":{"times":1,"cmd":"ebcs"},
    # "LBCS":{"times":1,"cmd":"lbcs"},
}

## DAG

# dax_pegasus_path = "./resources/workflows/Pegasus/%s.xml"
# dag_pegasus_path = "./resources/workflows/tiny/%s.json"

dax_pegasus_path = "./resources/workflows/Pegasus/task_number/%s.xml"
dag_pegasus_path = "./resources/workflows/tiny/task_number/%s.json"

dag_pegasus_unused = [
    "Montage_25",
    "Montage_50",
    "Montage_100",
    # "Montage_1000",
    "Epigenomics_24",
    "Epigenomics_46",
    "Epigenomics_100",
    # "Epigenomics_997",
    "CyberShake_30",
    "CyberShake_50",
    "CyberShake_100",
    # "CyberShake_1000",
    "Sipht_30",
    "Sipht_60",
    "Sipht_100",
    # "Sipht_1000",
    "Inspiral_30",
    "Inspiral_50",
    "Inspiral_100",
    # "Inspiral_1000",
]

dag_pegasus = [
    "CyberShake_30",
    "CyberShake_50",
    "CyberShake_70",
    "CyberShake_100",
    "CyberShake_200",
    "CyberShake_300",
    "CyberShake_400",
    "CyberShake_500",
    "CyberShake_600",
    "CyberShake_700",
    "CyberShake_800",
    "CyberShake_900",
    "CyberShake_1000",

    "Montage_30",
    "Montage_50",
    "Montage_70",
    "Montage_100",
    "Montage_200",
    "Montage_300",
    "Montage_400",
    "Montage_500",
    "Montage_600",
    "Montage_700",
    "Montage_800",
    "Montage_900",
    "Montage_1000",

    "Epigenomics_30",
    "Epigenomics_50",
    "Epigenomics_70",
    "Epigenomics_100",
    "Epigenomics_200",
    "Epigenomics_300",
    "Epigenomics_400",
    "Epigenomics_500",
    "Epigenomics_600",
    "Epigenomics_700",
    "Epigenomics_800",
    "Epigenomics_900",
    "Epigenomics_1000",

    "LIGO_30",
    "LIGO_50",
    "LIGO_70",
    "LIGO_100",
    "LIGO_200",
    "LIGO_300",
    "LIGO_400",
    "LIGO_500",
    "LIGO_600",
    "LIGO_700",
    "LIGO_800",
    "LIGO_900",
    "LIGO_1000",

    "SIPHT_30",
    "SIPHT_50",
    "SIPHT_70",
    "SIPHT_100",
    "SIPHT_200",
    "SIPHT_300",
    "SIPHT_400",
    "SIPHT_500",
    "SIPHT_600",
    "SIPHT_700",
    "SIPHT_800",
    "SIPHT_900",
    "SIPHT_1000",
]

## Database
db_addr = 'http://127.0.0.1:5984/'
db_name = "hookie-exp-test"

## Query

query_cond = {
    "algorithm" : [
        "SPEA2-C0",
        "SPEA2-C3",
        "NSGA2-C3",
        "SPEA2-C5",
        "LOSS2",
        "LOSS3",
        "BHEFT",
        "HBCS",
        "EBCS",
        "LBCS",
        # "MOHEFT",

        "SPEA2-C3_c4",
    ],
    "pop_size" : [20],
}

## Hypervolume

hv_ref = [1.01, 1.01]

## Plotting

figure_path_pegasus_plot = "./results/"
figure_path_pegasus_trace = "./results/trace/"
figure_path_pegasus_bar = "./results/bar/"
