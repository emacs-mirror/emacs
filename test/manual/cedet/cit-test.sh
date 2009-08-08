#!/bin/sh

EMACS=${EMACS:-emacs};

STYLE=$1;
if [ -z $1 ]; then
    STYLE=Make
fi

rm -r /tmp/CEDET_INTEG*
$EMACS -q -l ../common/cedet.el -l cit-load.el -f toggle-debug-on-error  -f toggle-debug-on-quit -f cedet-integ-test-${STYLE}
