#!/bin/sh
#
# $Id$
# Copyright (c) 2013-2020 Ravenbrook Limited. See end of file for license.
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
#     testrun.sh [-s SUITE] [-r RUNNER] DIR [CASE1 CASE2 ...]
#
# You can use this program to run the same test many times, to get
# lots of random coverage. For example::
#
#     yes amcss | head -100 | xargs tool/testrun.sh code/xc/Debug
#
# This runs the AMC stress test 100 times from the code/xc/Debug
# directory, reporting all failures.

echo "MPS test suite"

TEST_RUNNER=
TEST_CASES=

# Parse command-line arguments.
while [ $# -gt 0 ]; do
    case "$1" in
        -s)
            TEST_SUITE=$2
            case "$TEST_SUITE" in
                testrun)      EXCLUDE="LNW"   ;;
                testci)       EXCLUDE="BNW"   ;;
                testall)      EXCLUDE="NW"    ;;
                testansi)     EXCLUDE="LNTW"  ;;
                testpollnone) EXCLUDE="LNPTW" ;;
                *)
                    echo "Test suite $TEST_SUITE not recognized."
                    exit 1 ;;
            esac
            echo "Test suite: $TEST_SUITE"
            TEST_CASE_DB=$(dirname -- "$0")/testcases.txt
            TEST_CASES=$(<"$TEST_CASE_DB" grep -e '^[a-z]' |
                                grep -v -e "=[$EXCLUDE]" |
                                cut -d' ' -f1)
            shift 2
            ;;
        -r)
            TEST_RUNNER=$2
            shift 2
            ;;
        -*)
            echo "Unrecognized option $1"
            exit 1
            ;;
        *)
            break
            ;;
    esac
done

# Make a temporary output directory for the test logs.
LOGDIR=$(mktemp -d /tmp/mps.log.XXXXXX)
echo "Logging test output to $LOGDIR"

# Next argument is the directory containing the test cases.
TEST_DIR=$1
shift
echo "Test directory: $TEST_DIR"

# Determine which tests to run.
TEST_CASES="$TEST_CASES $*"

SEPARATOR=----------------------------------------
TEST_COUNT=0
PASS_COUNT=0
FAIL_COUNT=0
for TESTCASE in $TEST_CASES; do
    TEST=$(basename -- "$TESTCASE")
    LOGTEST=$LOGDIR/$TEST_COUNT-$TEST
    TELEMETRY=$LOGDIR/$TEST_COUNT-$TEST-io
    MPS_TELEMETRY_FILENAME=$TELEMETRY.log
    export MPS_TELEMETRY_FILENAME

    echo "Running $TEST"
    TEST_COUNT=$((TEST_COUNT + 1))
    if $TEST_RUNNER "$TEST_DIR/$TESTCASE" > "$LOGTEST" 2>&1; then
        PASS_COUNT=$((PASS_COUNT + 1))
    else
        echo "$TEST failed: log follows"
        echo ${SEPARATOR}${SEPARATOR}
        cat -- "$LOGTEST"
        echo
        echo ${SEPARATOR}${SEPARATOR}
        FAIL_COUNT=$((FAIL_COUNT + 1))
    fi

    if [ -f "$MPS_TELEMETRY_FILENAME" ]; then
        "$TEST_DIR/mpseventcnv" -f "$MPS_TELEMETRY_FILENAME" > "$TELEMETRY.cnv"
        gzip "$MPS_TELEMETRY_FILENAME"
        "$TEST_DIR/mpseventtxt" < "$TELEMETRY.cnv" > "$TELEMETRY.txt"
        if [ -x "$TEST_DIR/mpseventsql" ]; then
            MPS_TELEMETRY_DATABASE=$TELEMETRY.db
            export MPS_TELEMETRY_DATABASE
            "$TEST_DIR/mpseventsql" < "$TELEMETRY.cnv" >> "$LOGTEST" 2>&1
        fi
        rm -f "$TELEMETRY.cnv" "$TELEMETRY.txt" "$TELEMETRY.db"
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
# Copyright (C) 2013-2020 Ravenbrook Limited <https://www.ravenbrook.com/>.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the
#   distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
