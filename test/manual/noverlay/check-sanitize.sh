#!/bin/bash

prog=$1
shift

[ -z "$prog" ] && {
    echo "usage:$(basename $0) CHECK_PRGOGRAM";
    exit 1;
}

"$prog" "$@" | sed -e 's/^\([^:]\+\):\([0-9]\+\):[PFE]:[^:]*:\([^:]*\):[^:]*: *\(.*\)/\1:\2:\3:\4/'
