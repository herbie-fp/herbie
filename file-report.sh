#!/usr/bin/env bash

RHOST="totalcrazyhack.net"
RHOSTDIR="/var/www/casio"
TOPDIR=$(pwd)

T=$(date +"%y%m%d%H%M%S")
B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RFOLDER="reports"
RDIR="$RFOLDER/$T-$(hostname)-$B-$C"
mkdir "$RDIR"
cp report.md "$RDIR/report.md"
cp "$RFOLDER/reportStyle.css" "$RDIR/reportStyle.css"
pandoc --css "reportStyle.css" -f markdown -t html -o "$RDIR/report.html" "$RDIR/report.md"

read -p "Publish? (y/N) " yn
case $yn in
    y)
	read -p "Username: " user
	rsync --verbose --recursive "$RDIR" "$user@$RHOST:$RHOSTDIR/$RFOLDER"
	REPORTS=$(ssh $user@$RHOST "cd $RHOSTDIR/$RFOLDER; find * -maxdepth 0 -type d")
	racket casio/make-index.rkt $REPORTS
	cp index.md "$RFOLDER/index.md"
	pandoc -f markdown -t html -o "$RFOLDER/index.html" "$RFOLDER/index.md"
	rsync --verbose --recursive "$RFOLDER/index.html" "$user@$RHOST:$RHOSTDIR/$RFOLDER"
	;;
    *)
	echo "Report copied, but not published."
	;;
esac
