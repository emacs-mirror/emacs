;;; iso02-syn.el --- set up case-conversion and syntax tables for ISO 8859-2
;;;                   (ISO latin2, i.e. East Block character set)
;; Copyright (C) 1995 Free Software Foundation, Inc.

;; Author: Michael Gschwind (mike@vlsivie.tuwien.ac.at)
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

;; Written by Michael Gschwind.  See case-table.el for details.

;;; Code:

(require 'case-table)

(let ((downcase (standard-case-table)))
  (set-case-syntax 160 " " downcase)	  ; NBSP (no-break space)
  (set-case-syntax-pair 161 177 downcase) ; A with hook
  (set-case-syntax 162 "w" downcase)	  ; u accent
  (set-case-syntax-pair 163 179 downcase) ; L with slash
  (set-case-syntax 164 "w" downcase)	  ; general currency sign
  (set-case-syntax-pair 165 181 downcase) ; L with v
  (set-case-syntax-pair 166 182 downcase) ; S with acute accent
  (set-case-syntax 167 "w" downcase)	  ; section sign
  (set-case-syntax 168 "w" downcase)	  ; diaeresis
  (set-case-syntax-pair 169 185 downcase) ; S with v
  (set-case-syntax-pair 170 186 downcase) ; S with cedilla
  (set-case-syntax-pair 171 187 downcase) ; T with v
  (set-case-syntax-pair 172 188 downcase) ; Z with acute accent
  (set-case-syntax 173 "_" downcase)	  ; soft hyphen
  (set-case-syntax-pair 174 190 downcase) ; Z with v
  (set-case-syntax-pair 175 191 downcase) ; Z with dot
  (set-case-syntax 176 "_" downcase)	; degree sign
  (set-case-syntax 178 "w" downcase)	; hook accent
  (set-case-syntax 180 "w" downcase)	; acute accent
  (set-case-syntax 183 "_" downcase)	; v accent
  (set-case-syntax 184 "w" downcase)	; cedilla
  (set-case-syntax 189 "w" downcase)	; Hungarian '' accent
  (set-case-syntax-pair 192 224 downcase) ; R with acute accent
  (set-case-syntax-pair 193 225 downcase) ; A with acute accent
  (set-case-syntax-pair 194 226 downcase) ; A with circumflex accent
  (set-case-syntax-pair 195 227 downcase) ; A with u accent
  (set-case-syntax-pair 196 228 downcase) ; A with diaeresis or umlaut mark
  (set-case-syntax-pair 197 229 downcase) ; L with acute accent
  (set-case-syntax-pair 198 230 downcase) ; C with acute accent
  (set-case-syntax-pair 199 231 downcase) ; C with cedilla
  (set-case-syntax-pair 200 232 downcase) ; C with v accent
  (set-case-syntax-pair 201 233 downcase) ; E with acute accent
  (set-case-syntax-pair 202 234 downcase) ; E with hook
  (set-case-syntax-pair 203 235 downcase) ; E with diaeresis 
  (set-case-syntax-pair 204 236 downcase) ; E with v accent
  (set-case-syntax-pair 205 237 downcase) ; I with acute accent
  (set-case-syntax-pair 206 238 downcase) ; I with circumflex accent
  (set-case-syntax-pair 207 239 downcase) ; D with v accent
  (set-case-syntax-pair 208 240 downcase) ; D with stroke
  (set-case-syntax-pair 209 241 downcase) ; N with acute accent
  (set-case-syntax-pair 210 242 downcase) ; N with v accent
  (set-case-syntax-pair 211 243 downcase) ; O with acute accent
  (set-case-syntax-pair 212 244 downcase) ; O with circumflex accent
  (set-case-syntax-pair 213 245 downcase) ; O with Hungarian accent
  (set-case-syntax-pair 214 246 downcase) ; O with diaeresis or umlaut mark
  (set-case-syntax 215 "_" downcase)	; multiplication sign
  (set-case-syntax-pair 216 248 downcase) ; R with v accent
  (set-case-syntax-pair 217 249 downcase) ; U with ring
  (set-case-syntax-pair 218 250 downcase) ; U with acute accent
  (set-case-syntax-pair 219 251 downcase) ; U with Hungaraian accent
  (set-case-syntax-pair 220 252 downcase) ; U with diaeresis or umlaut mark
  (set-case-syntax-pair 221 253 downcase) ; Y with acute accent
  (set-case-syntax-pair 222 254 downcase) ; T with hook
  (set-case-syntax 223 "w" downcase)	; small sharp s, German
  (set-case-syntax 247 "_" downcase)	; division sign
  (set-case-syntax 255 "w" downcase)	; dot accent
)

(provide 'iso02-syn)

;;; iso-syntax.el ends here
