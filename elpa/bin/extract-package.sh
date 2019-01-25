#!/bin/bash

function grab_external {
    rm -rf packages/$PACKAGE*
    mkdir --parents $PACKAGE_LOC
    cwd=`pwd`
    cd $GIT_LOC
    git archive $SHA \
        | tar xv -C $cwd/$PACKAGE_LOC
    cd $cwd
    cp --no-clobber bin/package-makefile.mk $PACKAGE_LOC
}


function grab_subtree {
    rm -rf packages/*$PACKAGE
    mkdir --parents $PACKAGE_LOC
    cwd=`pwd`
    cd $GIT_LOC
    git archive $SHA packages/$PACKAGE \
        | tar xv  --strip-components=2 -C $cwd/$PACKAGE_LOC
    cd $cwd
    cp --no-clobber bin/package-makefile.mk $PACKAGE_LOC
}

SHA=
PACKAGE=
EXTERNAL=0
GIT_LOC=
while getopts "g:s:p:e" opt; do
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
        g)
            GIT_LOC=$OPTARG
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
