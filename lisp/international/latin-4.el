;;; latin-4.el --- set up case-conversion and syntax tables for ISO Latin-4

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
  (set-case-syntax-pair 161 177 tbl)	;latin letter a with ogonek
  (set-case-syntax 162 "w" tbl)		;latin letter kra
  (set-case-syntax-pair 163 179 tbl)	;latin letter r with cedilla
  (set-case-syntax 164 "w" tbl)		;currency sign
  (set-case-syntax-pair 165 181 tbl)	;latin letter i with tilde
  (set-case-syntax-pair 166 182 tbl)	;latin letter l with cedilla
  (set-case-syntax 167 "." tbl)		;section sign
  (set-case-syntax 168 "w" tbl)		;diaeresis
  (set-case-syntax-pair 169 185 tbl)	;latin letter s with caron
  (set-case-syntax-pair 170 186 tbl)	;latin letter e with macron
  (set-case-syntax-pair 171 187 tbl)	;latin letter g with cedilla
  (set-case-syntax-pair 172 188 tbl)	;latin letter t with stroke
  (set-case-syntax 173 "_" tbl)		;soft hyphen
  (set-case-syntax-pair 174 190 tbl)	;latin letter z with caron
  (set-case-syntax 175 "_" tbl)		;overline
  (set-case-syntax 176 "_" tbl)		;degree sign
  (set-case-syntax 178 "w" tbl)		;ogonek
  (set-case-syntax 180 "w" tbl)		;acute accent
  (set-case-syntax 183 "w" tbl)		;caron
  (set-case-syntax 184 "w" tbl)		;cedilla
  (set-case-syntax-pair 189 191 tbl)	;eng
  (set-case-syntax-pair 192 224 tbl)	;latin letter a with macron
  (set-case-syntax-pair 193 225 tbl)	;latin letter a with acute
  (set-case-syntax-pair 194 226 tbl)	;latin letter a with circumflex
  (set-case-syntax-pair 195 227 tbl)	;latin letter a with tilde
  (set-case-syntax-pair 196 228 tbl)	;latin letter a with diaeresis
  (set-case-syntax-pair 197 229 tbl)	;latin letter a with ring above
  (set-case-syntax-pair 198 230 tbl)	;latin letter ae
  (set-case-syntax-pair 199 231 tbl)	;latin letter i with ogonek
  (set-case-syntax-pair 200 232 tbl)	;latin letter c with caron
  (set-case-syntax-pair 201 233 tbl)	;latin letter e with acute
  (set-case-syntax-pair 202 234 tbl)	;latin letter e with ogonek
  (set-case-syntax-pair 203 235 tbl)	;latin letter e with diaeresis
  (set-case-syntax-pair 204 236 tbl)	;latin letter e with dot above
  (set-case-syntax-pair 205 237 tbl)	;latin letter i with acute
  (set-case-syntax-pair 206 238 tbl)	;latin letter i with circumflex
  (set-case-syntax-pair 207 239 tbl)	;latin letter i with macron
  (set-case-syntax-pair 208 240 tbl)	;latin letter d with stroke
  (set-case-syntax-pair 209 241 tbl)	;latin letter n with cedilla
  (set-case-syntax-pair 210 242 tbl)	;latin letter o with macron
  (set-case-syntax-pair 211 243 tbl)	;latin letter k with cedilla
  (set-case-syntax-pair 212 244 tbl)	;latin letter o with circumflex
  (set-case-syntax-pair 213 245 tbl)	;latin letter o with tilde
  (set-case-syntax-pair 214 246 tbl)	;latin letter o with diaeresis
  (set-case-syntax 215 "_" tbl)		;multiplication sign
  (set-case-syntax-pair 216 248 tbl)	;latin letter o with stroke
  (set-case-syntax-pair 217 249 tbl)	;latin letter u with ogonek
  (set-case-syntax-pair 218 250 tbl)	;latin letter u with acute
  (set-case-syntax-pair 219 251 tbl)	;latin letter u with circumflex
  (set-case-syntax-pair 220 252 tbl)	;latin letter u with diaeresis
  (set-case-syntax-pair 221 253 tbl)	;latin letter u with tilde
  (set-case-syntax-pair 222 254 tbl)	;latin letter u with macron
  (set-case-syntax 223 "w" tbl)		;latin letter sharp s
  (set-case-syntax 247 "_" tbl)		;division sign
  (set-case-syntax 255 "w" tbl))	;dot above

;; When preloading this file, don't provide the feature.
;; Explicit `require' is used to load this for 8-bit characters.
(or set-case-syntax-set-multibyte
    (provide 'latin-4))

;;; Don't compile this file: src/Makefile.in instructs make-docfile
;;; to look at the .el file!
;;; Local Variables:
;;; no-byte-compile: t
;;; End:

;;; latin-4.el ends here
