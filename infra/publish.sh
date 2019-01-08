#!/usr/bin/env bash
set -e -x

RHOST="uwplse.org"
RHOSTDIR="/var/www/herbie/reports"

upload () {
    DIR=$1
    B=$(git rev-parse --abbrev-ref HEAD)
    C=$(git rev-parse HEAD | sed 's/\(..........\).*/\1/')
    RDIR="$(date +%s):$(hostname):$B:$C"
    find "$DIR" -name "profile.txt" -or -name "debug.txt" -exec gzip -f {} \;
    rsync --recursive "$DIR" --exclude reports/ "$RHOST:$RHOSTDIR/$RDIR"
    ssh "$RHOST" chmod a+rx "$RHOSTDIR/$RDIR" -R
}

index () {
    racket infra/make-index.rkt
    rsync --recursive \
          "index.html" "infra/index.css" "infra/regression-chart.js" "src/web/report.js" \
          "$RHOST:$RHOSTDIR/"
    ssh "$RHOST" chgrp uwplse "$RHOSTDIR/{index.html,index.css,report.js,regression-chart.js}"
    rm index.html
}

backfill () {
    racket infra/backfill-index.rkt
}

download_reports () {
    rsync --recursive --checksum --inplace --ignore-existing \
          --include 'results.json' --include '*/' --exclude '*' \
          uwplse.org:/var/www/herbie/reports/ previous/
}

upload_reports () {
    rsync --recursive previous/ uwplse.org:/var/www/herbie/reports/
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
    download_reports
    index
elif [[ $CMD = "backfill" ]]; then
    download_reports
    backfill
elif [[ $CMD = "update-reports" ]]; then
    upload_reports
    index
else
    help
fi

