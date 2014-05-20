#!/usr/bin/env bash
set -e

RHOST="totalcrazyhack.net"
RHOSTDIR="/var/www/casio"
TOPDIR=$(pwd)

T=$(date +"%y%m%d%H%M%S")
B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RFOLDER="reports"
RDIR="$T-$(hostname)-$B-$C"

mkdir "$RDIR"
mv graphs "$RDIR/"

read -p "Publish? (y/N) " yn
case $yn in
    y)
	read -p "Username: " user
	rsync --verbose --recursive "$RDIR" "$user@$RHOST:$RHOSTDIR/$RFOLDER"
        ssh "$user@$RHOST" chmod a+rx "$RHOSTDIR/$RFOLDER" -R
	REPORTS=$(ssh $user@$RHOST "cd $RHOSTDIR/$RFOLDER; find * -maxdepth 0 -type d")
	racket reports/make-index.rkt $REPORTS
	pandoc -f markdown -t html -o "index.html" "index.md"
	rsync --verbose --recursive "index.html" "$user@$RHOST:$RHOSTDIR/$RFOLDER"
        rm index.html
	;;
    *)
	echo "Report copied, but not published."
	;;
esac
