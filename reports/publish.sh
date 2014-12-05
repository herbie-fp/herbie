#!/usr/bin/env bash
set -e -x

RHOST="recycle.cs.washington.edu"
RHOSTDIR="/cse/web/research/plse/dashboard/herbie"

B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RFOLDER="reports"
RDIR="$(date +%s):$(hostname):$B:$C"

rsync --verbose --recursive graphs/ "$RHOST:$RHOSTDIR/$RFOLDER/$RDIR"
ssh "$RHOST" chmod a+rx "$RHOSTDIR/$RFOLDER/$RDIR" -R
set +x
REPORTS=$(ssh "$RHOST" "cd $RHOSTDIR/$RFOLDER/; echo *:*:*:*")
racket reports/make-index.rkt $REPORTS
set -x
rsync --verbose --recursive "index.html" "$RHOST:$RHOSTDIR/$RFOLDER"
ssh "$RHOST" chgrp -R plse_www "$RHOSTDIR/$RFOLDER/$RDIR/" "$RHOSTDIR/$RFOLDER/index.html"
rm index.html
