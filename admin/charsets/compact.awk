# compact.awk -- Make charset map compact.
# Copyright (C) 2003
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
# Make a charset map compact by changing this kind of line sequence:
#   0x00 0x0000
#   0x01 0x0001
#   ...
#   0x7F 0x007F
# to one line of this format:
#   0x00-0x7F 0x0000

BEGIN {
  tohex["0"] = 1;
  tohex["1"] = 2;
  tohex["2"] = 3;
  tohex["3"] = 4;
  tohex["4"] = 5;
  tohex["5"] = 6;
  tohex["6"] = 7;
  tohex["7"] = 8;
  tohex["8"] = 9;
  tohex["9"] = 10;
  tohex["A"] = 11;
  tohex["B"] = 12;
  tohex["C"] = 13;
  tohex["D"] = 14;
  tohex["E"] = 15;
  tohex["F"] = 16;
  tohex["a"] = 11;
  tohex["b"] = 12;
  tohex["c"] = 13;
  tohex["d"] = 14;
  tohex["e"] = 15;
  tohex["f"] = 16;
  from_code = 0;
  to_code = -1;
  to_unicode = 0;
  from_unicode = 0;
}

function decode_hex(str, idx) {
  n = 0;
  len = length(str);
  for (i = idx; i <= len; i++)
    {
      c = tohex[substr (str, i, 1)];
      if (c == 0)
	break;
      n = n * 16 + c - 1;
    }
  return n;
}

/^\#/ {
  print;
  next;
}

{
  code = decode_hex($1, 3);
  unicode = decode_hex($2, 3);
  if ((code == to_code + 1) && (unicode == to_unicode + 1))
    {
      to_code++;
      to_unicode++;
    }
  else
    {
      if (to_code < 256)
	{
	  if (from_code == to_code)
	    printf "0x%02X 0x%04X\n", from_code, from_unicode;
	  else if (from_code < to_code)
	    printf "0x%02X-0x%02X 0x%04X\n", from_code, to_code, from_unicode;
	}
      else
	{
	  if (from_code == to_code)
	    printf "0x%04X 0x%04X\n", from_code, from_unicode;
	  else if (from_code < to_code)
	    printf "0x%04X-0x%04X 0x%04X\n", from_code, to_code, from_unicode;
	}
      from_code = to_code = code;
      from_unicode = to_unicode = unicode;
    }
}

END {
  if (to_code < 256)
    {
      if (from_code == to_code)
	printf "0x%02X 0x%04X\n", from_code, from_unicode;
      else
	printf "0x%02X-0x%02X 0x%04X\n", from_code, to_code, from_unicode;
    }
  else
    {
      if (from_code == to_code)
	printf "0x%04X 0x%04X\n", from_code, from_unicode;
      else
	printf "0x%04X-0x%04X 0x%04X\n", from_code, to_code, from_unicode;
    }
}

# arch-tag: 7e6f57c3-8e62-4af3-8916-ca67bca3a0ce
