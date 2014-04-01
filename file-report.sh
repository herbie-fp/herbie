#!/usr/bin/env bash

RHOST="totalcrazyhack.net:/var/www/casio/"

T=$(date +"%y%m%d%H%M%S")
B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RDIR="reports/$T-$(hostname)-$B-$C"
mkdir "$RDIR"
cp report.md "$RDIR/"
pandoc -f markdown -t html -o report.html report.md

read -p "Publish? (y/N) " yn
case $yn in
    y)
	rsync --verbose --recursive "$RDIR" "$RHOST/$RDIR"
	;;
    *)
	echo "Report copied, but not published."
	;;
esac
