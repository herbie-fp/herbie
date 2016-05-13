#!/usr/bin/env bash
set -e -x

RHOST="uwplse.org"
RHOSTDIR="/var/www/herbie/reports"

upload () {
    DIR=$1
    B=$(git rev-parse --abbrev-ref HEAD)
    C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
    RDIR="$(date +%s):$(hostname):$B:$C"
    find "$DIR" -name "debug.txt" -exec gzip -f {} \;
    rsync --verbose --recursive "$1" --exclude reports/ "$RHOST:$RHOSTDIR/$RDIR"
    ssh "$RHOST" chmod a+rx "$RHOSTDIR/$RDIR" -R
}

index () {
    rsync --verbose --include 'results.json' --include '/*/' --exclude '*' \
          --recursive uwplse.org:/var/www/herbie/reports/ graphs/reports/
    racket infra/make-index.rkt
    rsync --verbose --recursive \
          "index.html" "herbie/reports/index.css" \
          "herbie/reports/report.js" "herbie/reports/regression-chart.js" \
          "$RHOST:$RHOSTDIR/"
    ssh "$RHOST" chgrp uwplse "$RHOSTDIR/{index.html,index.css,report.js,regression-chart.js}"
    rm index.html
}

backfill () {
    rsync --verbose --include 'results.json' --include '/*/' --exclude '*' \
          --recursive uwplse.org:/var/www/herbie/reports/ graphs/reports/
    racket infra/backfill-index.rkt
    rsync --verbose --recursive graphs/reports/ uwplse.org:/var/www/herbie/reports/
}

help () {
    printf "USAGE: publish.sh upload <dir>\t\t\tUpload the directory <dir>\n"
    printf "       publish.sh index\t\t\t\tRegenerate the report index\n"
}

CMD="$1"

if [[ $CMD = "upload" ]]; then
    DIR="$2"
    if [[ -z $DIR ]]; then
        echo "Please pass a directory to upload"
        echo
        help
        exit 1
    elif [[ ! -d $DIR ]]; then
        echo "Directory $DIR does not exist"
        exit 2
    else
        upload "$DIR"
    fi
elif [[ $CMD = "index" ]]; then
    index
elif [[ $CMD = "backfill" ]]; then
    backfill
else
    help
fi

