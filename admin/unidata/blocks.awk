#!/usr/bin/awk -f

## Copyright (C) 2015-2020 Free Software Foundation, Inc.

## Author: Glenn Morris <rgm@gnu.org>

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

## This script takes as input Unicode's Blocks.txt
## (http://www.unicode.org/Public/UNIDATA/Blocks.txt)
## and produces output for Emacs's lisp/international/charscript.el.

## It lumps together all the blocks belonging to the same language.
## E.g., "Basic Latin", "Latin-1 Supplement", "Latin Extended-A",
## etc. are all lumped together under "latin".

## The Unicode blocks actually extend past some of these ranges with
## undefined codepoints.

## For additional details, see <https://debbugs.gnu.org/20789#11>.

## Things to do after installing a new version of Blocks.txt:
## Check the output against the old output.
## Adjust the alias array, and the name2alias function for any new
## entries, if necessary.
## Check fix_start (and fix_end) to see if entries need adding/removing.
## Review the hard-coded splits at the end of the main body.

### Code:

BEGIN {
    ## Hard-coded names.  See name2alias for the rest.
    alias["ipa extensions"] = "phonetic"
    alias["letterlike symbols"] = "symbol"
    alias["number forms"] = "symbol"
    alias["miscellaneous technical"] = "symbol"
    alias["control pictures"] = "symbol"
    alias["optical character recognition"] = "symbol"
    alias["enclosed alphanumerics"] = "symbol"
    alias["box drawing"] = "symbol"
    alias["block elements"] = "symbol"
    alias["miscellaneous symbols"] = "symbol"
    alias["symbols for legacy computing"] = "symbol"
    alias["cjk strokes"] = "cjk-misc"
    alias["cjk symbols and punctuation"] = "cjk-misc"
    alias["halfwidth and fullwidth forms"] = "cjk-misc"
    alias["common indic number forms"] = "north-indic-number"

    tohex["a"] = 10
    tohex["b"] = 11
    tohex["c"] = 12
    tohex["d"] = 13
    tohex["e"] = 14
    tohex["f"] = 15

    fix_start["0080"] = "00A0"
    ## Define fix_end here if you need it.
}

## From admin/charsets/.
## With gawk's --non-decimal-data switch we wouldn't need this.
function decode_hex(str   , n, len, i, c) {
  n = 0
  len = length(str)
  for (i = 1; i <= len; i++)
    {
      c = substr (str, i, 1)
      if (c >= "0" && c <= "9")
	n = n * 16 + (c - "0")
      else
	n = n * 16 + tohex[tolower(c)]
    }
  return n
}

function name2alias(name   , w, w2) {
    name = tolower(name)
    if (alias[name]) return alias[name]
    else if (name ~ /for symbols/) return "symbol"
    else if (name ~ /latin|combining .* marks|spacing modifier|tone letters|alphabetic presentation/) return "latin"
    else if (name ~ /cjk|yijing|enclosed ideograph|kangxi/) return "han"
    else if (name ~ /arabic/) return "arabic"
    else if (name ~ /^greek/) return "greek"
    else if (name ~ /^coptic/) return "coptic"
    else if (name ~ /cuneiform number/) return "cuneiform-numbers-and-punctuation"
    else if (name ~ /cuneiform/) return "cuneiform"
    else if (name ~ /mathematical alphanumeric symbol/) return "mathematical"
    else if (name ~ /punctuation|mathematical|arrows|currency|superscript|small form variants|geometric|dingbats|enclosed|alchemical|pictograph|emoticon|transport/) return "symbol"
    else if (name ~ /canadian aboriginal/) return "canadian-aboriginal"
    else if (name ~ /katakana|hiragana/) return "kana"
    else if (name ~ /myanmar/) return "burmese"
    else if (name ~ /hangul/) return "hangul"
    else if (name ~ /khmer/) return "khmer"
    else if (name ~ /braille/) return "braille"
    else if (name ~ /^yi /) return "yi"
    else if (name ~ /surrogates|private use|variation selectors/) return 0
    else if (name ~/^(specials|tags)$/) return 0
    else if (name ~ /linear b/) return "linear-b"
    else if (name ~ /aramaic/) return "aramaic"
    else if (name ~ /rumi num/) return "rumi-number"
    else if (name ~ /duployan|shorthand/) return "duployan-shorthand"
    else if (name ~ /sutton signwriting/) return "sutton-sign-writing"

    sub(/^small /, "", name)
    sub(/ (extended|extensions*|supplement).*/, "", name)
    sub(/numbers/, "number", name)
    sub(/numerals/, "numeral", name)
    sub(/symbols/, "symbol", name)
    sub(/forms$/, "form", name)
    sub(/tiles$/, "tile", name)
    sub(/^new /, "", name)
    sub(/ (characters|hieroglyphs|cursive|hieroglyph format controls)$/, "", name)
    gsub(/ /, "-", name)

    return name
}

/^[0-9A-F]/ {
    sep = index($1, "..")
    len = length($1)
    s = substr($1,1,sep-1)
    e = substr($1,sep+2,len-sep-2)
    $1 = ""
    sub(/^ */, "", $0)
    i++
    start[i] = fix_start[s] ? fix_start[s] : s
    end[i] = fix_end[e] ? fix_end[e]: e
    name[i] = $0

    alt[i] = name2alias(name[i])

    if (!alt[i])
    {
        i--
        next
    }

    ## Combine adjacent ranges with the same name.
    if (alt[i] == alt[i-1] && decode_hex(start[i]) == 1 + decode_hex(end[i-1]))
    {
        end[i-1] = end[i]
        name[i-1] = (name[i-1] ", " name[i])
        i--
    }

    ## Some hard-coded splits.
    if (start[i] == "0370")
    {
        end[i] = "03E1"
        i++
        start[i] = "03E2"
        end[i] = "03EF"
        alt[i] = "coptic"
        i++
        start[i] = "03F0"
        end[i] = "03FF"
        alt[i] = "greek"
    }
    else if (start[i] == "FB00")
    {
        end[i] = "FB06"
        i++
        start[i] = "FB13"
        end[i] = "FB17"
        alt[i] = "armenian"
        i++
        start[i] = "FB1D"
        end[i] = "FB4F"
        alt[i] = "hebrew"
    }
    else if (start[i] == "FF00")
    {
        end[i] = "FF60"
        i++
        start[i] = "FF61"
        end[i] = "FF9F"
        alt[i] = "kana"
        i++
        start[i] = "FFA0"
        end[i] = "FFDF"
        alt[i] = "hangul"
        i++
        start[i] = "FFE0"
        end[i] = "FFEF"
        alt[i] = "cjk-misc"
    }
}

END {
    print ";;; charscript.el --- character script table"
    print ";;; Automatically generated from admin/unidata/Blocks.txt"
    print "(let (script-list)"
    print "  (dolist (elt '("

    for (j=1;j<=i;j++)
    {
        printf("    (#x%s #x%s %s)", start[j], end[j], alt[j])
        ## Fuzz to decide whether worth printing original name as a comment.
        if (name[j] && alt[j] != tolower(name[j]) && alt[j] !~ /-/)
            printf(" ; %s", name[j])
        printf("\n")
    }

    print "    ))"
    print "    (set-char-table-range char-script-table"
    print "			  (cons (car elt) (nth 1 elt)) (nth 2 elt))"
    print "    (or (memq (nth 2 elt) script-list)"
    print "	(setq script-list (cons (nth 2 elt) script-list))))"
    print "  (set-char-table-extra-slot char-script-table 0 (nreverse script-list)))"
    print ""
    print "(provide 'charscript)"
}
