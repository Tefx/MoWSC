PEGASUS_DAGS = [
    # "Montage_25",
    # "Montage_50",
    # "Montage_100",
    # "Montage_1000",
    # "Epigenomics_24",
    # "Epigenomics_46",
    # "Epigenomics_100",
    # "Epigenomics_997_fixed",
    # "CyberShake_30",
    "CyberShake_50",
    # "CyberShake_100",
    "CyberShake_1000",
    # "Sipht_30",
    # "Sipht_60",
    # "Sipht_100",
    # "Sipht_1000",
    # "Inspiral_30",
    # "Inspiral_50",
    # "Inspiral_100",
    # "Inspiral_1000",
]

RANDAG = map(str, range(100))

CONFIG = {
    "ind": [1],
    "seed": ["ToI"],
    "pcr": [1],
    "pmu": [1],
    "gennum": [1000],
    "popsize": [50],
}

ALGS = ["spea2"]

OALGS = {
    # "MOHEFT"    : ("moheft", "-a %d" % CONFIG["popsize"][0]),
    # "HBCS"      : ("hbcs", "-k %f" % (1.0/CONFIG["popsize"][0])),
    # "FuzzyPSO"  : ("fpso", "-g %d -p %d" % (CONFIG["gennum"][0], CONFIG["popsize"][0])),
    # "NSPSO"     : ("nspso", "-g %d -p %d" % (CONFIG["gennum"][0], CONFIG["popsize"][0])),
    # "MODE"      : ("mode", "-g %d -p %d" % (CONFIG["gennum"][0], CONFIG["popsize"][0])),
    "MOABC1"    : ("moabc1", "-g %d -p %d" % (CONFIG["gennum"][0], CONFIG["popsize"][0])),
    # "MOABC2"    : ("moabc2", "-g %d -p %d" % (CONFIG["gennum"][0]/2, CONFIG["popsize"][0]))
}

DBNAME = "hookie-exp-test"

qCond = {"popsize": [50],
         "gennum": [1000],
         "seed" : ["ToI"],
         "pcr": [1],
         "pmu": [1],
         "ind": [1, 2],
         "alg": ["spea2"]}

qOA = ["MOHEFT", "MODE", "NSPSO", "FuzzyPSO", "HBCS", "MOABC1", "MOABC2"]

ref_point = [1.1, 1.1]

# EXP = "../src/Main"
EXP = "../dist/build/hookie-exp/hookie-exp"
DAG_PATH = "./resources/workflow/Pegasus/%s.xml"
# DAG_PATH = "./resources/workflow/random/%s.json"

# CouchDB_Addr = 'http://192.168.70.171:5984/'
# CouchDB_Addr = 'http://10.4.128.8:5984/'
CouchDB_Addr = 'http://127.0.0.1:5984/'
