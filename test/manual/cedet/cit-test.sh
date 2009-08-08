#!/bin/sh

EMACS=${EMACS:-emacs};

STYLE=$1;
if [ -z $1 ]; then
    STYLE=Make
fi

rm -r /tmp/CEDET_INTEG*

if $EMACS -q -l ../common/cedet.el -l cit-load.el -f toggle-debug-on-error  -f toggle-debug-on-quit -f cedet-integ-test-${STYLE}; then
    # Reverse the meaning of a 0 exit status, as the user had to quit Emacs
    # but on success, the program kills emacs with 1 (to be different.)
    exit 1;
else
    exit 0;
fi