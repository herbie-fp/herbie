#!/bin/python

import sys

if len(sys.argv) < 2:
    print("USAGE: makejson.py [prefix]")
    sys.exit()
else:
    PREFIX = sys.argv[1]

import csv

def rows(file):
    with open(file) as csvfile:
        return list(csv.reader(csvfile))

with open(PREFIX + ".names.csv") as namefile:
    NAMES = [line[3:-1] for line in namefile]

def read(base, type, col):
    IDROWS = rows("{}.i{}.csv".format(base, type))
    IDERRS = [float(row[col]) for row in IDROWS]
    
    ODROWS = rows("{}.o{}.csv".format(base, type))
    ODERRS = [float(row[col]) for row in ODROWS]
    return zip(IDERRS, ODERRS)

import json

with open(PREFIX + ".json", "w") as jsonfile:
    DOUBLEAVG = read(PREFIX, "d", 3)
    DOUBLEMAX = read(PREFIX, "d", 2)
    SINGLEAVG = read(PREFIX, "f", 3)
    SINGLEMAX = read(PREFIX, "f", 2)

    json.dump(
        [ { "name": name, "doubleAvg": da, "doubleMax": dm, "singleAvg": sa, "singleMax": sm }
          for name, da, dm, sa, sm
          in zip(NAMES, DOUBLEAVG, DOUBLEMAX, SINGLEAVG, SINGLEMAX)], jsonfile)
