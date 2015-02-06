#!/usr/bin/env bash
set -e -x

RHOST="uwplse.org"
RHOSTDIR="/var/www/herbie"

B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RFOLDER="reports"
RDIR="$(date +%s):$(hostname):$B:$C"

rsync --verbose --recursive graphs/ --exclude reports/ "$RHOST:$RHOSTDIR/$RFOLDER/$RDIR"
ssh "$RHOST" chmod a+rx "$RHOSTDIR/$RFOLDER/$RDIR" -R
REPORTS=$(ssh "$RHOST" "cd $RHOSTDIR/$RFOLDER/; echo *:*:*:*")
rsync -v --include 'results.json' --include '/*/' --exclude '*' -r uwplse.org:/var/www/herbie/reports/ graphs/reports/
racket herbie/reports/make-index.rkt
set -x
rsync --verbose --recursive "index.html" "herbie/reports/index.css" "$RHOST:$RHOSTDIR/$RFOLDER"
ssh "$RHOST" chgrp -R uwplse "$RHOSTDIR/$RFOLDER"
rm index.html
