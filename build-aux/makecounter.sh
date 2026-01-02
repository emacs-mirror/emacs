#!/bin/sh
# Generate or update a C file containing an increasing counter
# variable.
#
# Copyright (C) 2023-2026 Free Software Foundation, Inc.
#
# This file is part of GNU Emacs.  GNU Emacs is free software: you can
# redistribute it and/or modify it under the terms of the GNU General
# Public License as published by the Free Software Foundation, either
# version 3 of the License, or (at your option) any later version.
#
# GNU Emacs is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs.	 If not, see <https://www.gnu.org/licenses/>.

set -e

curcount=
if test -f "$1"; then
    curcount=`cat "$1" | grep = | cut -d= -f2 \
		  | sed -e 's/;//' -e 's/ //'`
fi

curcount=`expr 1 + $curcount 2>/dev/null || echo 0`

cat > $1 <<EOF
/* Generated automatically by makecounter.sh.  Do not edit! */

#include <config.h>

#ifdef HAVE_ANDROID
#define EXPORT __attribute__ ((visibility ("default")))
#endif /* HAVE_ANDROID */

extern int emacs_shortlisp_counter;
#ifdef EXPORT
EXPORT
#endif /* EXPORT */
int emacs_shortlisp_counter = $curcount;
EOF
