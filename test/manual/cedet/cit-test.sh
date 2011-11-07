#!/bin/sh

EMACS=${EMACS:-emacs}

STYLE=$1
if [ -z $1 ]; then
    STYLE=Make
fi

# Further Emacs options (like --batch)
OPTIONS=$2

rm -r /tmp/CEDET_INTEG*
cd ..
$EMACS $OPTIONS -q -l cedet-load.el -l tests/cit-load.el -f toggle-debug-on-error  -f toggle-debug-on-quit -f cedet-integ-test-${STYLE}
EXITCODE=$?

if [ $EXITCODE -eq 0 ]; then
    # Reverse the meaning of a 0 exit status, as the user had to quit Emacs.
    exit 1
elif [ $EXITCODE -eq 1 ]; then
    # On success, the program kills emacs with 1 (to be different.)
    exit 0
else
    # Otherwise, Emacs might have an error running in batch mode, and we return it.
    exit $EXITCODE
fi
