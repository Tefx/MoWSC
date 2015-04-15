#!/usr/bin/env python

from json import load, dump
from itertools import chain
from sys import argv

price_file = "pricing.json"
csv = "description.csv"

using_types = [
  "m1.small",
  "m1.medium",
  "m1.large",
  "m1.xlarge",

  # "t2.micro",
  # "t2.small",
  # "t2.medium",

  "m3.medium",
  "m3.large",
  "m3.xlarge",
  "m3.2xlarge",

  "c4.large",
  "c4.xlarge",
  "c4.2xlarge",
  "c4.4xlarge",
  "c4.8xlarge",

  # "c3.large",
  # "c3.xlarge",
  # "c3.2xlarge",
  # "c3.4xlarge",
  # "c3.8xlarge",
  ]

with open(price_file) as f:
    data = load(f)

ecus = {}
bws = {}
with open(csv) as f:
    for line in f:
        size,bw,_,ecu = line.split(",")
        bw = float(bw)
        ecu = float(ecu)
        ecus[size] = ecu
        bws[size] = bw

def getInfo(s):
    size = s["size"]
    return {"name":size,
            "ecu":ecus[size],
            "bandwidth":bws[size] * 131072,
            "price":float(s["valueColumns"][0]["prices"]["USD"])}

def getRegion(region):
    for t in data:
        if t["region"] == region:
            s = [x["sizes"] for x in t["instanceTypes"]]
            return map(getInfo, chain(*s))

def usingOnly(infos):
    return [x for x in infos if x["name"] in using_types]

with open(argv[2], "w") as f:
    dump(usingOnly(getRegion(argv[1])), f)
