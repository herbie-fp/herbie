#!/usr/bin/env bash

WIKI="../casio.wiki"
RDIR="reports"

mkdir -p $WIKI/$RDIR
T=$(date +"%y%m%d%H%M%S")
B=$(git rev-parse --abbrev-ref HEAD)
C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
RPATH="$RDIR/$T-$(hostname)-$B-$C.md"
cp report.md $WIKI/$RPATH

read -p "Publish? (y/N) " yn
case $yn in
    y)
	cd $WIKI && \
	git add $RPATH && \
	git commit -m "Added Report" && \
	git push
	;;
    *)
	echo "Report copied, but not published."
	;;
esac
