#!/bin/python3

import urllib.request
import json

REPO="uwplse/herbie"

def travis_req(after_number=None):
    url = "http://api.travis-ci.org/repos/{}/builds".format(REPO)
    if after_number:
        url += "?after_number=" + after_number
    req = urllib.request.Request(url)
    req.add_header("Accept", "application/vnd.travis-ci.2.1+json")
    resp = urllib.request.urlopen(req)
    data = json.load(resp)
    for build in data["builds"]:
        yield build

def get_travis():
    last = None
    while True:
        for x in travis_req(after_number=last):
            last = x["number"]
            yield x

def times():
    for build in get_travis():
        if "started_at" in build and build["started_at"] < "2018-06-15":
            break
        if build["event_type"] == "push" and build["duration"] and build["state"] == "passed":
            yield build["started_at"], build["duration"]

if __name__ == "__main__":
    for d, t in times():
        print(d.replace("T", " ").replace("Z", ""), t, sep="\t")
