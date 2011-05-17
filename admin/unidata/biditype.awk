# Generate data for filling bidi_type_table, see src/bidi.c:bidi_initialize.

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

function trtype(type)
{
    # Types are listed in the order of decresing use in UnicodeData.txt:
    if (type == "ON")
	return "NEUTRAL_ON";
    else if (type == "NSM")
	return "WEAK_NSM";
    else if (type == "AL")
	return "STRONG_AL";
    else if (type == "R")
	return "STRONG_R";
    else if (type == "BN")
	return "WEAK_BN";
    else if (type == "EN")
	return "WEAK_EN";
    else if (type == "ET")
	return "WEAK_ET";
    else if (type == "AN")
	return "WEAK_AN";
    else if (type == "WS")
	return "NEUTRAL_WS";
    else if (type == "CS")
	return "WEAK_CS";
    else if (type == "ES")
	return "WEAK_ES";
    else if (type == "B")
	return "NEUTRAL_B";
    else if (type == "S")
	return "NEUTRAL_S";
    else if (type == "LRE" || type == "RLE" || type == "LRO" || type == "RLO" || type == "PDF")
	return type;
    else if (type == "L")
	return "STRONG_L";
    else
    {
	printf "Unknown type: %s\n", type > "/dev/stderr";
	exit 1;
    }
}

BEGIN {
     otype = "";
     startcode = "";
     endcode = "";
     printf "  struct {\n    int from, to;\n    bidi_type_t type;\n  } bidi_type[] = {\n";
     first = 1;
 }

 {   code = $1;
     ntype = $5;
     if (ntype != otype)
     {
	 # Don't output data for L, as that's the default value, see bidi.c.
	 if (otype != "L" && startcode != "")
	 {
	     if (!first)
		 printf ",\n";
	     else
		 first = 0;
	     printf "\t{ 0x%s, 0x%s, %s }", startcode, endcode, trtype(otype);
	 }
	 otype = ntype;
	 startcode = code;
	 endcode = code;
     }
     else
	 endcode = code;
 }

END {
     printf " };\n";
 }
