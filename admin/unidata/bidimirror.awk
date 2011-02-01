# Generate data for bidi_mirroring_table, see src/bidi.c:bidi_initialize.

# Copyright (C) 2010-2011  Free Software Foundation, Inc.

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
# along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

# Written by Eli Zaretskii <eliz@gnu.org>

BEGIN {
     printf "  struct {\n    int from, to;\n  } bidi_mirror[] = {\n";
     first = 1;
 }

$1 !~ /^#/ && NF >= 2 {
     if (!first)
	 printf ",\n";
     else
	 first = 0;
     printf "\t{ 0x%s, 0x%s }", $1, $2;
 }

END {
     printf " };\n";
 }
