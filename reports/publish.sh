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

read -p "Username: " user
rsync --verbose --recursive graphs/ "$user@$RHOST:$RHOSTDIR/$RFOLDER/$RDIR"
ssh "$user@$RHOST" chmod a+rx "$RHOSTDIR/$RFOLDER/$RDIR" -R
REPORTS=$(ssh "$user@$RHOST" "cd $RHOSTDIR/$RFOLDER; find * -maxdepth 0 -type d")
racket reports/make-index.rkt $REPORTS
rsync --verbose --recursive "index.html" "$user@$RHOST:$RHOSTDIR/$RFOLDER"
rm index.html
