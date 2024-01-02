#! /bin/bash
# Run the mh-utils-tests against all MH variants found on this system.

# Copyright (C) 2021-2024 Free Software Foundation, Inc.

# This file is part of GNU Emacs.

# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

# Commentary:

# By default runs all tests; test names or Emacs-style regexps may be
# given on the command line to run just those tests.
#
# Option -d turns on Emacs variable mh-test-utils-debug-mocks, which
# causes the tests to output all interactions with the file system.

# If you want to run the tests for only one MH variant, you don't need
# to use this script, because "make" can do it.  See the commentary at
# the top of ./mh-utils-tests.el for the recipe.

debug=
if [[ "$1" = -* ]]; then
    if [[ "$1" != -d ]]; then
        echo "Usage: $(basename "$0") [-d] [test ...]" >&2
        exit 2
    fi
    debug=t
    shift
fi

shopt -s extglob
ert_test_list=()
for tst; do
    # Guess the type the test spec
    case $tst in
        *[\[\].*+\\]*)  # Regexp: put in string quotes
            ert_test_list+=("\"$tst\"")
            ;;
        *)  # Lisp expression, keyword, or symbol: use as is
            ert_test_list+=("$tst")
            ;;
    esac
done
if [[ ${#ert_test_list[@]} -eq 0 ]]; then
    # t means true for all tests, runs everything
    ert_test_list=(t)
fi

# This script is 3 directories down in the Emacs source tree.
cd "$(dirname "$0")"
cd ../../..
emacs=(src/emacs --batch -Q)

# MH-E has a good list of directories where an MH variant might be installed,
# so we look in each of those.
read -r -a mh_sys_path \
    < <("${emacs[@]}" -l mh-e --eval "(princ mh-sys-path)" | sed 's/[()]//g')

have_done_mocked_variant=false
declare -i tests_total=0 tests_passed=0

for path in "${mh_sys_path[@]}"; do
    if [[ ! -x "$path/mhparam" ]]; then
        if [[ "$have_done_mocked_variant" = false ]]; then
            have_done_mocked_variant=true
        else
            continue
        fi
    fi
    echo "**  Testing with PATH $path"
    ((++tests_total))
    TEST_MH_PATH=$path TEST_MH_DEBUG=$debug \
    HOME=/nonexistent \
    "${emacs[@]}" -l ert \
        --eval "(setq load-prefer-newer t)" \
        --eval "(load \"$PWD/test/lisp/mh-e/mh-utils-tests\" nil t)" \
        --eval "(ert-run-tests-batch-and-exit '(or ${ert_test_list[*]}))" \
        && ((++tests_passed))
done

if (( tests_total == 0 )); then
    echo "NO tests run"
    exit 1
elif (( tests_total == tests_passed )); then
    echo "All tested variants pass: $tests_passed/$tests_total"
else
    echo "Tested variants passing: $tests_passed/$tests_total," \
         "FAILING: $((tests_total - tests_passed))/$tests_total"
    exit 1
fi
