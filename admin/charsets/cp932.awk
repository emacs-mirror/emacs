# cp932.awk -- Add sort key at the tail of each line of CP932-2BYTE.map.
# Copyright (C) 2004
#   National Institute of Advanced Industrial Science and Technology (AIST)
#   Registration Number H13PRO009
#
# This file is part of GNU Emacs.
#
# GNU Emacs is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Emacs; see the file COPYING.  If not, write to the
# Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA 02111-1307, USA.

# Comment:
# Add a sort key 0, 1, 2, or 3 at the tail of each line as a comment
# to realize the round trip mapping to Unicode works as described in
# this page:
#	http://support.microsoft.com/default.aspx?scid=kb;EN-US;170559
# Each sort key means as below:
#   0: JISX0208 characters.
#   1: NEC special characters.
#   2: IBM extension characters.
#   3: NEC selection of IBM extension characters.

BEGIN {
  tohex["A"] = 10;
  tohex["B"] = 11;
  tohex["C"] = 12;
  tohex["D"] = 13;
  tohex["E"] = 14;
  tohex["F"] = 15;
}

function decode_hex(str) {
  n = 0;
  len = length(str);
  for (i = 1; i <= len; i++)
    {
      c = substr(str, i, 1);
      if (c >= "0" && c <= "9")
	n = n * 16 + (c - "0");
      else
	n = n * 16 + tohex[c];
    }
  return n;
}

function sjis_to_jis_ku(code)
{
  s1 = int(code / 256);
  s2 = code % 256;
  if (s2 >= 159)		# s2 >= 0x9F
    {
      if (s1 >= 224)		# s1 >= 0xE0
	j1 = s1 * 2 - 352;	# j1 = s1 * 2 - 0x160
      else
	j1 = s1 * 2 - 224;	# j1 = s1 * 2 - 0xE0
    }
  else
    {
      if (s1 >= 224)
	j1 = s1 * 2 - 353;	# j1 = s1 * 2 - 0x161
      else
	j1 = s1 * 2 - 225;	# j1 = s1 * 2 - 0xE1
    }
  return j1 - 32;
}

/^0x[89E]/ {
  sjis=decode_hex(substr($1, 3, 4))
  ku=sjis_to_jis_ku(sjis);
  if (ku == 13)
    print $0" # 1";
  else if (ku >= 89 && ku <= 92)
    print $0" # 3";
  else
    print $0" # 0";
  next;
}

/^0xF/ {
  print $0" # 2";
  next;
}

{
  print;
}
