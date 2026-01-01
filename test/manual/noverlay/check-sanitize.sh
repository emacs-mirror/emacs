#!/usr/bin/env bash
### check-sanitize.sh - strip confusing parts of Check error output

## Copyright (C) 2017-2026 Free Software Foundation, Inc.

## This file is part of GNU Emacs.

## GNU Emacs is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## GNU Emacs is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

set -o pipefail

prog=$1
shift

[ -z "$prog" ] && {
    echo "usage:$(basename $0) CHECK_PROGRAM";
    exit 1;
}

# FIXME: This would be unnecessary if
# compilation-error-regexp-alist-alist understood libcheck OOTB.
"$prog" "$@" | sed -e 's/^\([^:]\+\):\([0-9]\+\):\([PFE]\):\([^:]*\):\([^:]*\):[^:]*:\(.*\)/\1:\2:\3:\4:\5:\6/'
