# eucjp-ms.awk -- Generate a translation table for eucJP-ms.
# Copyright (C) 2004, 2005, 2006, 2007, 2008
#   National Institute of Advanced Industrial Science and Technology (AIST)
#   Registration Number H13PRO009

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

# Commentary:

# eucJP-ms is one of eucJP-open encoding defined at this page:
#  http://www.opengroup.or.jp/jvc/cde/appendix.html

BEGIN {
  print ";;; eucjp-ms.el -- translation table for eucJP-ms. -*- no-byte-compile: t -*-";
  print ";;; Automatically genrated from eucJP-13th.txt, eucJP-udc.txt, eucJP-ibmext.txt";
  print "(let ((map";
  printf "       '(;JISEXT<->UNICODE";

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

/0x8F/ {
  code = decode_hex(substr($1, 5, 4));
  code -= 32896;		# code -= 0x8080
  printf "\n	 (#x%04x #x%s)", code, substr($2, 3, 4);
  next;
}

/0x[A-F]/ {
  code = decode_hex(substr($1, 3, 4));
  code -= 32896;		# code -= 0x8080
  printf "\n	 (#x%04x . #x%s)", code, substr($2, 3, 4);
}

END {
  print ")))";
  print "  (mapc #'(lambda (x)";
  print "	    (if (integerp (cdr x))";
  print "		(setcar x (decode-char 'japanese-jisx0208 (car x)))";
  print "	      (setcar x (decode-char 'japanese-jisx0212 (car x)))";
  print "	      (setcdr x (cadr x))))";
  print "	map)";
  print "  (define-translation-table 'eucjp-ms-decode map)";
  print "  (mapc #'(lambda (x)";
  print "	    (let ((tmp (car x)))";
  print "	      (setcar x (cdr x)) (setcdr x tmp)))";
  print "	map)";
  print "  (define-translation-table 'eucjp-ms-encode map))";
}

# arch-tag: d9cc7af7-2d6e-48cd-8eed-a6d25226de7c
