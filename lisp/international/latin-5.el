;;; latin-5.el --- set up case-conversion and syntax tables for ISO latin-5

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: Erik Naggum
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

;; Written by Erik Naggum.  See case-table.el for details.

;;; Code:

(require 'case-table)

(let ((tbl (standard-case-table)))
  (set-case-syntax 160 " " tbl)		;no-break space
  (set-case-syntax 161 "." tbl)		;inverted exclamation mark
  (set-case-syntax 162 "w" tbl)		;cent sign
  (set-case-syntax 163 "w" tbl)		;pound sign
  (set-case-syntax 164 "w" tbl)		;currency sign
  (set-case-syntax 165 "w" tbl)		;yen sign
  (set-case-syntax 166 "_" tbl)		;broken bar
  (set-case-syntax 167 "." tbl)		;section sign
  (set-case-syntax 168 "w" tbl)		;diaeresis
  (set-case-syntax 169 "_" tbl)		;copyright sign
  (set-case-syntax 170 "w" tbl)		;feminine ordinal indicator
  (set-case-syntax-delims 171 187 tbl)	;left-pointing double angle quotation mark
  (set-case-syntax 172 "_" tbl)		;not sign
  (set-case-syntax 173 "_" tbl)		;soft hyphen
  (set-case-syntax 174 "_" tbl)		;registered sign
  (set-case-syntax 175 "w" tbl)		;overline
  (set-case-syntax 176 "_" tbl)		;degree sign
  (set-case-syntax 177 "_" tbl)		;plus-minus sign
  (set-case-syntax 178 "w" tbl)		;superscript digit two
  (set-case-syntax 179 "w" tbl)		;superscript digit three
  (set-case-syntax 180 "w" tbl)		;acute accent
  (set-case-syntax 181 "_" tbl)		;micro sign
  (set-case-syntax 182 "." tbl)		;pilcrow sign
  (set-case-syntax 183 "_" tbl)		;middle dot
  (set-case-syntax 184 "w" tbl)		;cedilla
  (set-case-syntax 185 "w" tbl)		;superscript digit one
  (set-case-syntax 186 "w" tbl)		;masculine ordinal indicator
  (set-case-syntax 188 "_" tbl)		;vulgar fraction one quarter
  (set-case-syntax 189 "_" tbl)		;vulgar fraction one half
  (set-case-syntax 190 "_" tbl)		;vulgar fraction three quarters
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
  (set-case-syntax 202 "w" tbl)		;latin capital letter e with circumflex
  (set-case-syntax-pair 203 235 tbl)	;latin letter e with diaeresis
  (set-case-syntax 204 "w" tbl)		;latin capital letter i with grave
  (set-case-syntax-pair 205 237 tbl)	;latin letter i with acute
  (set-case-syntax-pair 206 238 tbl)	;latin letter i with circumflex
  (set-case-syntax 207 "w" tbl)		;latin capital letter i with diaeresis
  (set-case-syntax-pair 208 240 tbl)	;latin letter g with breve
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
  (set-case-syntax 221 "w" tbl)		;latin capital letter i with dot above
  (set-case-syntax-pair 222 254 tbl)	;latin letter s with cedilla
  (set-case-syntax 223 "w" tbl)		;latin small letter sharp s
  (set-case-syntax 234 "w" tbl)		;latin small letter e with ogonek
  (set-case-syntax 236 "w" tbl)		;latin small letter e with dot above
  (set-case-syntax 239 "w" tbl)		;latin small letter i with macron
  (set-case-syntax 247 "_" tbl)		;division sign
  (set-case-syntax 253 "w" tbl)		;latin small letter i dotless
  (set-case-syntax 255 "w" tbl))	;latin small letter y with diaeresis

;; When preloading this file, don't provide the feature.
;; Explicit `require' is used to load this for 8-bit characters.
(or set-case-syntax-set-multibyte
    (provide 'latin-5))

;;; Don't compile this file: src/Makefile.in instructs make-docfile
;;; to look at the .el file!
;;; Local Variables:
;;; no-byte-compile: t
;;; End:

;;; latin-5.el ends here
