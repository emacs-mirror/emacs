#!/bin/bash

function grab_external {
    rm -rf packages/*$PACKAGE
    mkdir packages/$SHA-$PACKAGE
    cd elpa-git
    git archive $SHA \
        | tar xv -C ../packages/$SHA-$PACKAGE
    cd ..
    cp bin/package-makefile.mk packages/$SHA-$PACKAGE
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

exit 1
