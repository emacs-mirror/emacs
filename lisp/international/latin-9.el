;;; latin-9.el --- set up case-conversion and syntax tables for ISO Latin-9

;; Copyright (C) 1988,1997,1999 Free Software Foundation, Inc.

;; Author: Dave Love
;; Maintainer: FSF
;; Keywords: i18n

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Case-conversion and syntax tables for ISO-8859-15 (Latin-9),
;; adapted from latin-1.el.

;; <URL:http://czyborra.com/charsets/iso8859.html> (edited to remove
;;   non-ASCII characters!):
;; The new Latin9 nicknamed Latin0 aims to update Latin1 by
;; replacing less needed symbols with forgotten French and
;; Finnish letters and placing the U+20AC Euro sign in the cell =A4 of
;; the former international currency sign.

;; The differences from Latin-1 are starred on the comments below.

;; This is now ISO/IEC 8859-15:1999.  As of 1999-05, there was a `final
;; proof' at <URL:http://www.indigo.ie/egt/standards/iso8859/8859-15-en.pdf>.
;; See also <URL:http://www.itscj.ipsj.or.jp/ISO-IR/203.pdf>.

;;; Code:

(require 'case-table)

(let ((tbl (standard-case-table)))
  (set-case-syntax 160 " " tbl)		;no-break space
  (set-case-syntax 161 "." tbl)		;inverted exclamation mark
  (set-case-syntax 162 "w" tbl)		;cent sign
  (set-case-syntax 163 "w" tbl)		;pound sign
  (set-case-syntax 164 "w" tbl)		;euro sign *
  (set-case-syntax 165 "w" tbl)		;yen sign
  (set-case-syntax-pair 166 168 tbl)	;latin letter s with caron *
  (set-case-syntax 167 "." tbl)		;section sign
  (set-case-syntax 169 "_" tbl)		;copyright sign
  (set-case-syntax 170 "w" tbl)		;feminine ordinal indicator
  (set-case-syntax-delims 171 187 tbl)	;left-pointing double angle quotation mark
  (set-case-syntax 172 "_" tbl)		;not sign
  (set-case-syntax 173 "_" tbl)		;soft hyphen
  (set-case-syntax 174 "_" tbl)		;registered sign
  (set-case-syntax 175 "w" tbl)		;macron
  (set-case-syntax 176 "_" tbl)		;degree sign
  (set-case-syntax 177 "_" tbl)		;plus-minus sign
  (set-case-syntax 178 "w" tbl)		;superscript two
  (set-case-syntax 179 "w" tbl)		;superscript three
  (set-case-syntax-pair 180 184 tbl)	;latin letter z with caron *
  (set-case-syntax 181 "_" tbl)		;micro sign
  (set-case-syntax 182 "." tbl)		;pilcrow sign
  (set-case-syntax 183 "_" tbl)		;middle dot
  (set-case-syntax 185 "w" tbl)		;superscript one
  (set-case-syntax 186 "w" tbl)		;masculine ordinal indicator
  (set-case-syntax-pair 188 189 tbl)	;latin ligature oe *
  (set-case-syntax-pair 190 255 tbl)	;latin letter y with diaeresis *
  (set-case-syntax 191 "." tbl)		;inverted question mark
  (set-case-syntax-pair 192 224 tbl)	;latin letter a with grave
  (set-case-syntax-pair 193 225 tbl)	;latin letter a with acute
  (set-case-syntax-pair 194 226 tbl)	;latin letter a with circumflex
  (set-case-syntax-pair 195 227 tbl)	;latin letter a with tilde
  (set-case-syntax-pair 196 228 tbl)	;latin letter a with diaeresis
  (set-case-syntax-pair 197 229 tbl)	;latin letter a with ring above
  (set-case-syntax-pair 198 230 tbl)	;latin letter ae
  (set-case-syntax-pair 199 231 tbl)	;latin letter c with cedilla
  (set-case-syntax-pair 200 232 tbl)	;latin letter e with grave
  (set-case-syntax-pair 201 233 tbl)	;latin letter e with acute
  (set-case-syntax-pair 202 234 tbl)	;latin letter e with circumflex
  (set-case-syntax-pair 203 235 tbl)	;latin letter e with diaeresis
  (set-case-syntax-pair 204 236 tbl)	;latin letter i with grave
  (set-case-syntax-pair 205 237 tbl)	;latin letter i with acute
  (set-case-syntax-pair 206 238 tbl)	;latin letter i with circumflex
  (set-case-syntax-pair 207 239 tbl)	;latin letter i with diaeresis
  (set-case-syntax-pair 208 240 tbl)	;latin letter eth
  (set-case-syntax-pair 209 241 tbl)	;latin letter n with tilde
  (set-case-syntax-pair 210 242 tbl)	;latin letter o with grave
  (set-case-syntax-pair 211 243 tbl)	;latin letter o with acute
  (set-case-syntax-pair 212 244 tbl)	;latin letter o with circumflex
  (set-case-syntax-pair 213 245 tbl)	;latin letter o with tilde
  (set-case-syntax-pair 214 246 tbl)	;latin letter o with diaeresis
  (set-case-syntax 215 "_" tbl)		;multiplication sign
  (set-case-syntax-pair 216 248 tbl)	;latin letter o with stroke
  (set-case-syntax-pair 217 249 tbl)	;latin letter u with grave
  (set-case-syntax-pair 218 250 tbl)	;latin letter u with acute
  (set-case-syntax-pair 219 251 tbl)	;latin letter u with circumflex
  (set-case-syntax-pair 220 252 tbl)	;latin letter u with diaeresis
  (set-case-syntax-pair 221 253 tbl)	;latin letter y with acute
  (set-case-syntax-pair 222 254 tbl)	;latin letter thorn
  (set-case-syntax 223 "w" tbl)		;latin small letter sharp s
  (set-case-syntax 247 "_" tbl))	;division sign

;; When preloading this file, don't provide the feature.
;; Explicit `require' is used to load this for 8-bit characters.
(or set-case-syntax-set-multibyte
    (provide 'latin-9))

;;; Don't compile this file: src/Makefile.in instructs make-docfile
;;; to look at the .el file!
;;; Local Variables:
;;; no-byte-compile: t
;;; End:

;;; latin-9.el ends here
