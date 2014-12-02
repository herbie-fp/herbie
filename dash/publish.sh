#!/usr/bin/env bash

DASH="recycle.cs.washington.edu:/cse/web/research/plse/dashboard/herbie"
rsync --quiet --recursive . "$DASH"
