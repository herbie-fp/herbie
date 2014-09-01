#!/usr/bin/env bash
set -e -x

RHOST="totalcrazyhack.net"
RHOSTDIR="/var/www/casio"
TOPDIR=$(pwd)

T=$(date +%s)
B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RFOLDER="reports"
RDIR="$T-$(hostname)-$B-$C"

rsync --verbose --recursive graphs/ "$RHOST:$RHOSTDIR/$RFOLDER/$RDIR"
ssh "$RHOST" chmod a+rx "$RHOSTDIR/$RFOLDER/$RDIR" -R
set +x
REPORTS=$(ssh "$RHOST" "cd $RHOSTDIR/$RFOLDER/; echo *-*-*-*")
racket reports/make-index.rkt $REPORTS
set -x
rsync --verbose --recursive "index.html" "$RHOST:$RHOSTDIR/$RFOLDER"
ssh "$RHOST" chgrp -R casio "$RHOSTDIR/$RFOLDER/$RDIR/" "$RHOSTDIR/$RFOLDER/index.html"
rm index.html
