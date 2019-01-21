#!/bin/bash

function grab_external {
    rm -rf packages/$PACKAGE*
    mkdir --parents $PACKAGE_LOC
    cd elpa-git
    git archive $SHA \
        | tar xv -C ../$PACKAGE_LOC
    cd ..
    cp --no-clobber bin/package-makefile.mk $PACKAGE_LOC
}


function grab_subtree {
    rm -rf packages/*$PACKAGE
    mkdir --parents $PACKAGE_LOC
    cd elpa-git
    git archive $SHA packages/$PACKAGE \
        | tar xv  --strip-components=2 -C ../$PACKAGE_LOC
    cd ..
    cp --no-clobber bin/package-makefile.mk $PACKAGE_LOC
}

SHA=
PACKAGE=
EXTERNAL=0

while getopts "s:p:e" opt; do
    case $opt in
        s)
            SHA=$OPTARG
            ;;
        p)
            PACKAGE=$OPTARG
            ;;
        e)
            EXTERNAL=1
            ;;
    esac
done

PACKAGE_LOC=packages/$PACKAGE-$SHA/$PACKAGE

if (($EXTERNAL))
then
    grab_external
    exit 0
fi

grab_subtree
