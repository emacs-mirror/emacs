#!/bin/sh
# 
# $Id$
# Copyright (c) 2013-2014 Ravenbrook Limited. See end of file for license.
# 
# This program runs a series of test cases, capturing the output of
# each one to a temporary file. In addition, the output of any test
# case that fails gets printed to standard output so that it is not
# lost when the test is running on a temporary build server (see
# job003489). Finally, it prints a summary of passes and failures, and
# if there were any failures, it exits with a non-zero status code.
#
# Usage::
# 
#     testrun.sh DIR [CASE1 CASE2 ...]

ALL_TEST_CASES="
    abqtest
    amcss
    amcsshe
    amcssth
    amsss
    amssshe
    apss
    arenacv
    awlut
    awluthe
    awlutth
    btcv
    exposet0
    expt825
    fbmtest
    finalcv
    finaltest
    fotest
    locbwcss
    lockcov
    locusss
    locv
    messtest
    mpmss
    mpsicv
    mv2test
    poolncv
    qs
    sacss
    segsmss
    steptest
    walkt0
    zmess
"
# bttest -- interactive, so cannot be run unattended
# djbench -- benchmark, not test case
# gcbench -- benchmark, not test case
# teletest -- interactive, so cannot be run unattended
# zcoll -- takes too long to be useful as a regularly run smoke test

# Make a temporary output directory for the test logs.
LOGDIR=$(mktemp -d /tmp/mps.log.XXXXXX)
TEST_DIR=$1
echo "MPS test suite"
echo "Logging test output to $LOGDIR"
echo "Test directory: $TEST_DIR"
shift
TEST_CASES=${*:-${ALL_TEST_CASES}}

SEPARATOR="----------------------------------------"
TEST_COUNT=0
PASS_COUNT=0
FAIL_COUNT=0
for TESTCASE in $TEST_CASES; do
    TEST="$(basename -- "$TESTCASE")"
    LOGTEST="$LOGDIR/$TEST"
    echo "Running $TEST"
    TEST_COUNT=$(expr $TEST_COUNT + 1)
    if "$TEST_DIR/$TESTCASE" > "$LOGTEST" 2>&1; then
        PASS_COUNT=$(expr $PASS_COUNT + 1)
    else
        echo "$TEST failed: log follows"
        echo ${SEPARATOR}${SEPARATOR}
        cat -- "$LOGTEST"
        echo
        echo ${SEPARATOR}${SEPARATOR}
        FAIL_COUNT=$(expr $FAIL_COUNT + 1)
    fi
done
if [ $FAIL_COUNT = 0 ]; then
    echo "Tests: $TEST_COUNT. All tests pass."
else
    echo "Tests: $TEST_COUNT. Passes: $PASS_COUNT. Failures: $FAIL_COUNT."
    exit 1
fi


# C. COPYRIGHT AND LICENSE
#
# Copyright (C) 2013-2014 Ravenbrook Limited <http://www.ravenbrook.com/>.
# All rights reserved.  This is an open source license.  Contact
# Ravenbrook for commercial licensing options.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
# 
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
# 
# 3. Redistributions in any form must be accompanied by information on how
# to obtain complete source code for this software and any accompanying
# software that uses this software.  The source code must either be
# included in the distribution or be available for no more than the cost
# of distribution plus a nominal fee, and must be freely redistributable
# under reasonable conditions.  For an executable file, complete source
# code means the source code for all modules it contains. It does not
# include source code for modules or files that typically accompany the
# major components of the operating system on which the executable file
# runs.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
# PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
# USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
# ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
