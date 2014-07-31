#!/usr/bin/env bash
set -e -x

RHOST="totalcrazyhack.net"
RHOSTDIR="/var/www/casio"
TOPDIR=$(pwd)

T=$(date +"%y%m%d%H%M%S")
B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RFOLDER="reports"
RDIR="$T-$(hostname)-$B-$C"

rsync --verbose --recursive graphs/ "$RHOST:$RHOSTDIR/$RFOLDER/$RDIR"
ssh "$RHOST" chmod a+rx "$RHOSTDIR/$RFOLDER/$RDIR" -R
REPORTS=$(ssh "$RHOST" "cd $RHOSTDIR/$RFOLDER; find * -maxdepth 0 -type d")
racket reports/make-index.rkt $REPORTS
rsync --verbose --recursive "index.html" "$RHOST:$RHOSTDIR/$RFOLDER"
ssh "$RHOST" chgrp -R casio "$RHOSTDIR/$RFOLDER/"
rm index.html
