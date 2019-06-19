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
    return data["builds"], data["commits"]

def get_travis():
    commits = {}
    last = None
    while True:
        data, coms = travis_req(after_number=last)
        for x in coms:
            commits[x["id"]] = x
        for x in data:
            last = x["number"]
            if "commit_id" in x:
                x["commit"] = commits[x["commit_id"]]
            yield x

def times():
    for build in get_travis():
        if build["event_type"] != "push": continue
        if not build["commit"]["committed_at"]: continue
        if build["commit"]["committed_at"] < "2018-06-15": break
        if build["commit"]["branch"] == "master": continue
        if build["state"] != "passed": continue
        yield build["started_at"], build["duration"]

if __name__ == "__main__":
    for d, t in times():
        print(d.replace("T", " ").replace("Z", ""), t, sep="\t")
