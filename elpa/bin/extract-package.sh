#!/bin/bash

function grab_external {
    rm -rf packages/*$PACKAGE
    mkdir --parents packages/$SHA-$PACKAGE/$PACKAGE
    cd elpa-git
    git archive $SHA \
        | tar xv -C ../packages/$SHA-$PACKAGE/$PACKAGE
    cd ..
    cp --no-clobber bin/package-makefile.mk packages/$SHA-$PACKAGE/$PACKAGE/
}


function grab_subtree {
    rm -rf packages/*$PACKAGE
    mkdir --parents packages/$SHA-$PACKAGE/$PACKAGE
    cd elpa-git
    git archive $SHA \
        | tar xv -C ../packages/$SHA-$PACKAGE/$PACKAGE
    cd ..
    cp --no-clobber bin/package-makefile.mk packages/$SHA-$PACKAGE/$PACKAGE
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

if (($EXTERNAL))
then
    grab_external
    exit 0
fi

grab_subtree
