;;; latin-4.el --- support for ISO Latin 4 (ISO 8859-4)

;; Copyright (C) 1988,1997 Free Software Foundation, Inc.

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

(let ((tbl (standard-case-table))
      (set-case-syntax-offset
       (if enable-multibyte-characters
	   (- (make-char 'latin-iso8859-4) 128)
	 0)))
  (set-case-syntax 160 "w" tbl)		;NO-BREAK SPACE
  (set-case-syntax-pair 161 177 tbl)	;LATIN LETTER A WITH OGONEK
  (set-case-syntax 162 "w" tbl)		;LATIN LETTER KRA
  (set-case-syntax-pair 163 179 tbl)	;LATIN LETTER R WITH CEDILLA
  (set-case-syntax 164 "w" tbl)		;CURRENCY SIGN
  (set-case-syntax-pair 165 181 tbl)	;LATIN LETTER I WITH TILDE
  (set-case-syntax-pair 166 182 tbl)	;LATIN LETTER L WITH CEDILLA
  (set-case-syntax 167 "." tbl)		;SECTION SIGN
  (set-case-syntax 168 "w" tbl)		;DIAERESIS
  (set-case-syntax-pair 169 185 tbl)	;LATIN LETTER S WITH CARON
  (set-case-syntax-pair 170 186 tbl)	;LATIN LETTER E WITH MACRON
  (set-case-syntax-pair 171 187 tbl)	;LATIN LETTER G WITH CEDILLA
  (set-case-syntax-pair 172 188 tbl)	;LATIN LETTER T WITH STROKE
  (set-case-syntax 173 "w" tbl)		;SOFT HYPHEN
  (set-case-syntax-pair 174 190 tbl)	;LATIN LETTER Z WITH CARON
  (set-case-syntax 175 "_" tbl)		;OVERLINE
  (set-case-syntax 176 "_" tbl)		;DEGREE SIGN
  (set-case-syntax 178 "w" tbl)		;OGONEK
  (set-case-syntax 180 "w" tbl)		;ACUTE ACCENT
  (set-case-syntax 183 "w" tbl)		;CARON
  (set-case-syntax 184 "w" tbl)		;CEDILLA
  (set-case-syntax-pair 189 191 tbl)	;ENG
  (set-case-syntax-pair 192 224 tbl)	;LATIN LETTER A WITH MACRON
  (set-case-syntax-pair 193 225 tbl)	;LATIN LETTER A WITH ACUTE
  (set-case-syntax-pair 194 226 tbl)	;LATIN LETTER A WITH CIRCUMFLEX
  (set-case-syntax-pair 195 227 tbl)	;LATIN LETTER A WITH TILDE
  (set-case-syntax-pair 196 228 tbl)	;LATIN LETTER A WITH DIAERESIS
  (set-case-syntax-pair 197 229 tbl)	;LATIN LETTER A WITH RING ABOVE
  (set-case-syntax-pair 198 230 tbl)	;LATIN LETTER AE
  (set-case-syntax-pair 199 231 tbl)	;LATIN LETTER I WITH OGONEK
  (set-case-syntax-pair 200 232 tbl)	;LATIN LETTER C WITH CARON
  (set-case-syntax-pair 201 233 tbl)	;LATIN LETTER E WITH ACUTE
  (set-case-syntax-pair 202 234 tbl)	;LATIN LETTER E WITH OGONEK
  (set-case-syntax-pair 203 235 tbl)	;LATIN LETTER E WITH DIAERESIS
  (set-case-syntax-pair 204 236 tbl)	;LATIN LETTER E WITH DOT ABOVE
  (set-case-syntax-pair 205 237 tbl)	;LATIN LETTER I WITH ACUTE
  (set-case-syntax-pair 206 238 tbl)	;LATIN LETTER I WITH CIRCUMFLEX
  (set-case-syntax-pair 207 239 tbl)	;LATIN LETTER I WITH MACRON
  (set-case-syntax-pair 208 240 tbl)	;LATIN LETTER D WITH STROKE
  (set-case-syntax-pair 209 241 tbl)	;LATIN LETTER N WITH CEDILLA
  (set-case-syntax-pair 210 242 tbl)	;LATIN LETTER O WITH MACRON
  (set-case-syntax-pair 211 243 tbl)	;LATIN LETTER K WITH CEDILLA
  (set-case-syntax-pair 212 244 tbl)	;LATIN LETTER O WITH CIRCUMFLEX
  (set-case-syntax-pair 213 245 tbl)	;LATIN LETTER O WITH TILDE
  (set-case-syntax-pair 214 246 tbl)	;LATIN LETTER O WITH DIAERESIS
  (set-case-syntax 215 "_" tbl)		;MULTIPLICATION SIGN
  (set-case-syntax-pair 216 248 tbl)	;LATIN LETTER O WITH STROKE
  (set-case-syntax-pair 217 249 tbl)	;LATIN LETTER U WITH OGONEK
  (set-case-syntax-pair 218 250 tbl)	;LATIN LETTER U WITH ACUTE
  (set-case-syntax-pair 219 251 tbl)	;LATIN LETTER U WITH CIRCUMFLEX
  (set-case-syntax-pair 220 252 tbl)	;LATIN LETTER U WITH DIAERESIS
  (set-case-syntax-pair 221 253 tbl)	;LATIN LETTER U WITH TILDE
  (set-case-syntax-pair 222 254 tbl)	;LATIN LETTER U WITH MACRON
  (set-case-syntax 223 "w" tbl)		;LATIN LETTER SHARP S
  (set-case-syntax 247 "_" tbl)		;DIVISION SIGN
  (set-case-syntax 255 "w" tbl))	;DOT ABOVE

(provide 'latin-4)

;;; latin-4.el ends here
