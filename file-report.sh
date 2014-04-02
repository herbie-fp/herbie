#!/usr/bin/env bash

RHOST="totalcrazyhack.net:/var/www/casio/"

T=$(date +"%y%m%d%H%M%S")
B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RDIR="reports/$T-$(hostname)-$B-$C"
IDIR="reports"
mkdir "$RDIR"
cp report.md "$RDIR/report.md"
cp index.md "$IDIR/index.md"
pandoc -f markdown -t html -o "$RDIR/report.html" "$RDIR/report.md"
pandoc -f markdown -t html -o "$IDIR/index.html" "$IDIR/index.md"

read -p "Publish? (y/N) " yn
case $yn in
    y)
	read -p "Username: " user
	rsync --verbose --recursive "$RDIR" "$user@$RHOST$RDIR"
	rsync --verbose --recursive "$IDIR" "$user@$RHOST$IDER"
	;;
    *)
	echo "Report copied, but not published."
	;;
esac
