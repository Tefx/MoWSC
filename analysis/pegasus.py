WORKFLOWS = [
	# Standard"dag with default parameters
	{"dag":"Montage_25"			,"task_number":"default", "path":"", "app":"Montage"},
    {"dag":"Montage_50"			,"task_number":"default", "path":"", "app":"Montage"},
    {"dag":"Montage_100"		,"task_number":"default", "path":"", "app":"Montage"},
    {"dag":"Montage_1000"		,"task_number":"default", "path":"", "app":"Montage"},

    {"dag":"Epigenomics_24"		,"task_number":"default", "path":"", "app":"Epigenomics"},
    {"dag":"Epigenomics_46"		,"task_number":"default", "path":"", "app":"Epigenomics"},
    {"dag":"Epigenomics_100"	,"task_number":"default", "path":"", "app":"Epigenomics"},
    {"dag":"Epigenomics_997"	,"task_number":"default", "path":"", "app":"Epigenomics"},

    {"dag":"CyberShake_30"		,"task_number":"default", "path":"", "app":"CyberShake"},
    {"dag":"CyberShake_50"		,"task_number":"default", "path":"", "app":"CyberShake"},
    {"dag":"CyberShake_100"		,"task_number":"default", "path":"", "app":"CyberShake"},
    {"dag":"CyberShake_1000"	,"task_number":"default", "path":"", "app":"CyberShake"},

    {"dag":"Sipht_30"			,"task_number":"default", "path":"", "app":"SIPHT"},
    {"dag":"Sipht_60"			,"task_number":"default", "path":"", "app":"SIPHT"},
    {"dag":"Sipht_100"			,"task_number":"default", "path":"", "app":"SIPHT"},
    {"dag":"Sipht_1000"			,"task_number":"default", "path":"", "app":"SIPHT"},

    {"dag":"Inspiral_30"		,"task_number":"default", "path":"", "app":"LIGO"},
    {"dag":"Inspiral_50"		,"task_number":"default", "path":"", "app":"LIGO"},
    {"dag":"Inspiral_100"		,"task_number":"default", "path":"", "app":"LIGO"},
	{"dag":"Inspiral_1000"		,"task_number":"default", "path":"", "app":"LIGO"},
]

BASE_DAX_PATH = "./resources/workflows/Pegasus/%s%s.xml"
BASE_TINY_PATH = "./resources/workflows/tiny/%s%s.json"
APPs = ["Montage", "Epigenomics", "SIPHT", "LIGO", "CyberShake"]
# TASK_NUMBERS = [30, 50, 70, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000]
TASK_NUMBERS = [30, 70, 200, 500, 1000]

def genWorkflow(app=APPs, task_number=["default"]):
	for w in WORKFLOWS:
		if w["app"] in app and w["task_number"] in task_number:
			yield w

if __name__ == '__main__':
	for w in genWorkflow(task_number=[100]):
		print w
