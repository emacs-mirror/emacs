;;; iso8859-1.el --- set up case-conversion and syntax tables for ISO 8859/1

;; Copyright (C) 1988 Free Software Foundation, Inc.

;; Author: Howard Gayle
;; Maintainer: FSF
;; Keywords: i14n

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Written by Howard Gayle.  See case-table.el for details.

;;; Code:

(require 'case-table)

(let ((table (standard-case-table)))
  (standard-case-syntax 160 " " table)	; NBSP (no-break space)
  (standard-case-syntax 161 "." table)	; inverted exclamation mark
  (standard-case-syntax 162 "w" table)	; cent sign
  (standard-case-syntax 163 "w" table)	; pound sign
  (standard-case-syntax 164 "w" table)	; general currency sign
  (standard-case-syntax 165 "w" table)	; yen sign
  (standard-case-syntax 166 "_" table)	; broken vertical line
  (standard-case-syntax 167 "w" table)	; section sign
  (standard-case-syntax 168 "w" table)	; diaeresis
  (standard-case-syntax 169 "_" table)	; copyright sign
  (standard-case-syntax 170 "w" table)	; ordinal indicator, feminine
  (standard-case-syntax-delims 171 187 table) ; angle quotation marks
  (standard-case-syntax 172 "_" table)	; not sign
  (standard-case-syntax 173 "_" table)	; soft hyphen
  (standard-case-syntax 174 "_" table)	; registered sign
  (standard-case-syntax 175 "w" table)	; macron
  (standard-case-syntax 176 "_" table)	; degree sign
  (standard-case-syntax 177 "_" table)	; plus or minus sign
  (standard-case-syntax 178 "w" table)	; superscript two
  (standard-case-syntax 179 "w" table)	; superscript three
  (standard-case-syntax 180 "w" table)	; acute accent
  (standard-case-syntax 181 "_" table)	; micro sign
  (standard-case-syntax 182 "w" table)	; pilcrow
  (standard-case-syntax 183 "_" table)	; middle dot
  (standard-case-syntax 184 "w" table)	; cedilla
  (standard-case-syntax 185 "w" table)	; superscript one
  (standard-case-syntax 186 "w" table)	; ordinal indicator, masculine
  ;;    	       	      187          ; See 171 above.
  (standard-case-syntax 188 "_" table)	; fraction one-quarter
  (standard-case-syntax 189 "_" table)	; fraction one-half
  (standard-case-syntax 190 "_" table)	; fraction three-quarters
  (standard-case-syntax 191 "." table)	; inverted question mark
  (standard-case-syntax-pair 192 224 table) ; A with grave accent
  (standard-case-syntax-pair 193 225 table) ; A with acute accent
  (standard-case-syntax-pair 194 226 table) ; A with circumflex accent
  (standard-case-syntax-pair 195 227 table) ; A with tilde
  (standard-case-syntax-pair 196 228 table) ; A with diaeresis or umlaut mark
  (standard-case-syntax-pair 197 229 table) ; A with ring
  (standard-case-syntax-pair 198 230 table) ; AE diphthong
  (standard-case-syntax-pair 199 231 table) ; C with cedilla
  (standard-case-syntax-pair 200 232 table) ; E with grave accent
  (standard-case-syntax-pair 201 233 table) ; E with acute accent
  (standard-case-syntax-pair 202 234 table) ; E with circumflex accent
  (standard-case-syntax-pair 203 235 table) ; E with diaeresis or umlaut mark
  (standard-case-syntax-pair 204 236 table) ; I with grave accent
  (standard-case-syntax-pair 205 237 table) ; I with acute accent
  (standard-case-syntax-pair 206 238 table) ; I with circumflex accent
  (standard-case-syntax-pair 207 239 table) ; I with diaeresis or umlaut mark
  (standard-case-syntax-pair 208 240 table) ; D with stroke, Icelandic eth
  (standard-case-syntax-pair 209 241 table) ; N with tilde
  (standard-case-syntax-pair 210 242 table) ; O with grave accent
  (standard-case-syntax-pair 211 243 table) ; O with acute accent
  (standard-case-syntax-pair 212 244 table) ; O with circumflex accent
  (standard-case-syntax-pair 213 245 table) ; O with tilde
  (standard-case-syntax-pair 214 246 table) ; O with diaeresis or umlaut mark
  (standard-case-syntax 215 "_" table)	; multiplication sign
  (standard-case-syntax-pair 216 248 table) ; O with slash
  (standard-case-syntax-pair 217 249 table) ; U with grave accent
  (standard-case-syntax-pair 218 250 table) ; U with acute accent
  (standard-case-syntax-pair 219 251 table) ; U with circumflex accent
  (standard-case-syntax-pair 220 252 table) ; U with diaeresis or umlaut mark
  (standard-case-syntax-pair 221 253 table) ; Y with acute accent
  (standard-case-syntax-pair 222 254 table) ; thorn, Icelandic
  (standard-case-syntax 223 "w" table)	; small sharp s, German
  (standard-case-syntax 247 "_" table)	; division sign
  (standard-case-syntax 255 "w" table)	; small y with diaeresis or umlaut mark
  (set-standard-case-table (list (car table))))

(provide 'iso8859-1)

;;; iso8859-1.el ends here
