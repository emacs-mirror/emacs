#!/usr/bin/awk -f

## Copyright (C) 2020, 2022-2024 Free Software Foundation, Inc.

## Author: Robert Pluim <rpluim@gmail.com>

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

### Commentary:

## This script takes as input Unicode's emoji-zwj-sequences.txt
## and produces output for Emacs's lisp/international/emoji-zwj.el.
## It also outputs the composition sequences for flags, UK flags, and
## skin tones which have been derived from emoji-sequences.txt by hand.

## For additional details, see <https://debbugs.gnu.org/39799#8>.

## Things to do after installing a new version of
## emoji-zwj-sequences.txt and emoji-sequences.txt
## Check the output against the old output.  See if there are any new
## composition sequences in emoji-sequences.txt that that need to be
## added Rebuild emacs, visit emoji-zwj-sequences.txt and
## emoji-sequences.txt and check that the various sequences are being
## composed properly.  Don't forget to install an appropriate font,
## such as Noto Color Emoji.

### Code:

/^[0-9A-F].*; RGI_Emoji_(ZWJ|Modifier)_Sequence/ {
    sub(/ *;.*/, "", $0)
    num = split($0, elts)
    if (ch[elts[1]] == "")
    {
        vec[elts[1]] = ""
        ch[elts[1]] = elts[1]
    }
     else
     {
         vec[elts[1]] = vec[elts[1]] "\n"
     }
     vec[elts[1]] = vec[elts[1]] "\""
    for (j = 1; j <= num; j++)
    {
        c = sprintf("\\N{U+%s}", elts[j])
        vec[elts[1]] = vec[elts[1]] c
    }
    vec[elts[1]] = vec[elts[1]] "\""
}

END {
     print ";;; emoji-zwj.el --- emoji zwj character composition table  -*- lexical-binding:t -*-"
     print ";;; Automatically generated from admin/unidata/emoji-{zwj-,}sequences.txt"
     print "(eval-when-compile (require 'regexp-opt))"

     # The following codepoints are not emoji, but they are part of
     # emoji sequences.  We have code in font.c:font_range that will
     # try to display them with the emoji font anyway.

     trigger_codepoints[1] = "261D"
     trigger_codepoints[2] = "26F9"
     trigger_codepoints[3] = "270C"
     trigger_codepoints[4] = "270D"
     trigger_codepoints[5] = "2764"
     trigger_codepoints[6] = "1F3CB"
     trigger_codepoints[7] = "1F3CC"
     trigger_codepoints[8] = "1F3F3"
     trigger_codepoints[9] = "1F3F4"
     trigger_codepoints[10] = "1F441"
     trigger_codepoints[11] = "1F574"
     trigger_codepoints[12] = "1F575"
     trigger_codepoints[13] = "1F590"

     printf "(setq auto-composition-emoji-eligible-codepoints\n"
     printf "'("

     for (trig in trigger_codepoints)
     {
         printf("\n?\\N{U+%s}", trigger_codepoints[trig])
     }
     printf "\n))\n\n"

     #  We add entries for 'codepoint U+FE0F' here to ensure that the
     # code in font_range is triggered.

     for (trig in trigger_codepoints)
     {
         codepoint = trigger_codepoints[trig]
         c = sprintf("\\N{U+%s}", codepoint)
         vec[codepoint] = vec[codepoint] "\n\"" c "\\N{U+FE0F}\""
     }

     print "(dolist (elt `("

     for (elt in ch)
    {
        entries = vec[elt] sprintf("\n\"\\N{U+%s}\\N{U+FE0E}\"\n\"\\N{U+%s}\\N{U+FE0F}\"", elt, elt)
        printf("(#x%s .\n,(eval-when-compile (regexp-opt\n'(\n%s\n))))\n", elt, entries)
    }
     print "))"
     print "  (set-char-table-range composition-function-table"
     print "                        (car elt)"
     print "                        (nconc (char-table-range composition-function-table (car elt))"
     print "                               (list (vector (cdr elt)"
     print "                                             0"
     print "                                             #'compose-gstring-for-graphic)))))"

     print ";; The following two blocks are derived by hand from emoji-sequences.txt"
     print ";; FIXME: add support for Emoji_Keycap_Sequence once we learn how to respect FE0F/VS-16"
     print ";; for ASCII characters."

     print ";; Flags"
     print "(set-char-table-range composition-function-table"
     print "                      '(#x1F1E6 . #x1F1FF)"
     print "                      (nconc (char-table-range composition-function-table '(#x1F1E6 . #x1F1FF))"
     print "                             (list (vector \"[\\U0001F1E6-\\U0001F1FF][\\U0001F1E6-\\U0001F1FF]\""
     print "                                           0"
     print "                                           #'compose-gstring-for-graphic))))"

     print ";; UK Flags"
     print "(set-char-table-range composition-function-table"
     print "                      #x1F3F4"
     print "                      (nconc (char-table-range composition-function-table #x1F3F4)"
     print "                             (list (vector \"\\U0001F3F4\\U000E0067\\U000E0062\\\\(?:\\U000E0065\\U000E006E\\U000E0067\\\\|\\U000E0073\\U000E0063\\U000E0074\\\\|\\U000E0077\\U000E006C\\U000E0073\\\\)\\U000E007F\""
     print "                                           0"
     print "                                           #'compose-gstring-for-graphic))))"

     printf "\n(provide 'emoji-zwj)"
}
