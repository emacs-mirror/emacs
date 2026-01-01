#!/usr/bin/awk -f

## Copyright (C) 2015-2026 Free Software Foundation, Inc.

## Author: Glenn Morris <rgm@gnu.org>
## Maintainer: emacs-devel@gnu.org

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
## (https://www.unicode.org/Public/UNIDATA/Blocks.txt)
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
    alias["miscellaneous symbols supplement"] = "symbol"
    alias["symbols for legacy computing"] = "symbol"
    alias["symbols for legacy computing supplement"] = "symbol"
    alias["cjk strokes"] = "cjk-misc"
    alias["cjk symbols and punctuation"] = "cjk-misc"
    alias["halfwidth and fullwidth forms"] = "cjk-misc"
    alias["yijing hexagram symbols"] = "cjk-misc"
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
    else if (name ~ /cjk|enclosed ideograph|kangxi/) return "han"
    else if (name ~ /arabic/) return "arabic"
    else if (name ~ /^greek/) return "greek"
    else if (name ~ /^coptic/) return "coptic"
    else if (name ~ /cuneiform number/) return "cuneiform"
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
    else if (name ~ /rumi num/) return "arabic"
    else if (name ~ /duployan|shorthand/) return "duployan-shorthand"
    else if (name ~ /sutton signwriting/) return "sutton-sign-writing"
    else if (name ~ /sinhala archaic number/) return "sinhala"
    else if (name ~ /tangut components/) return "tangut"

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

FILENAME ~ "Blocks.txt" && /^[0-9A-F]/ {
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

    # Hard-coded splits that must be processed before name2alias and
    # before combining same-named adjacent ranges.
    if (start[i] == "3300") # See Scripts.txt
    {
	end[i] = "3357"
	name[i] = "Katakana"
	alt[i] = "kana"
	i++
	start[i] = "3358"
	end[i] = "33FF"
	name[i] = "CJK Compatibility"
    }

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

FILENAME ~ "emoji-data.txt" && /^[0-9A-F].*; Emoji_Presentation / {
    sep = index($1, "..")
    len = length($1)
    if (sep > 0)  {
        s = substr($1,1,sep-1)
        e = substr($1,sep+2,len-sep-1)
    } else {
        s = $1
        e = $1
    }
    $1 = ""
    i++
    start[i] = s
    end[i] = e
    alt[i] = "emoji"
    name[i] = "Autogenerated emoji"
}

END {
    idx = 0
    ## This is here so that font_range can choose Emoji presentation
    ## for the preceding codepoint when it encounters a VS-16
    ## (U+FE0F).  See also font_range and the comments in composite.el
    ## around the setup of `composition-function-table' for
    ## U+FE00..U+FE0E.
    ## It originally covered the whole FE00-FE0F range, but that
    ## turned out to be a mistake.
    override_start[idx] = "FE0F"
    override_end[idx] = "FE0F"

    for (k in override_start)
    {
        i++
        start[i] = override_start[k]
        end[i] = override_end[k]
        alt[i] = "emoji"
        name[i] = "Autogenerated emoji (override)"
    }

    print ";;; charscript.el --- character script table  -*- lexical-binding:t -*-"
    print ";;; Automatically generated from admin/unidata/{Blocks,emoji-data}.txt"
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
    print "\n(map-char-table"
    print " (lambda (ch script)"
    print "   (and (eq script 'symbol)"
    print "	(modify-category-entry ch ?5)))"
    print " char-script-table)"
    print "\n(provide 'charscript)"
}
