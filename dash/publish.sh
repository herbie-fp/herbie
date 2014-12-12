#!/usr/bin/env bash

DASH="uwplse.org:/var/www/herbie/dash"
rsync --quiet --recursive . "$DASH"
