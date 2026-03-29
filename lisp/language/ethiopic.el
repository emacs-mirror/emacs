;;; ethiopic.el --- support for Ethiopic	-*- coding: utf-8-emacs; lexical-binding: t; -*-

;; Copyright (C) 1997, 2001-2026 Free Software Foundation, Inc.
;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H14PRO021

;; Keywords: multilingual, Ethiopic

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;; Author: TAKAHASHI Naoto <ntakahas@m17n.org>

;;; Commentary:

;; Note: This file includes several codepoints outside of the Unicode
;; 0..#x10FFFF range, which are characters that were not unified into
;; Unicode.  Therefore, this file is encoded in utf-8-emacs, because
;; UTF-8 cannot encode such codepoints.  We include these codepoints
;; literally in the file to have them displayed by suitable fonts,
;; which makes maintenance easier.

;;; Code:

(set-language-info-alist
 "Ethiopic" '((setup-function . setup-ethiopic-environment-internal)
	      (exit-function . exit-ethiopic-environment)
	      (charset ethiopic)
	      (coding-system utf-8-emacs)
	      (coding-priority utf-8-emacs)
	      (input-method . "ethiopic")
	      (features ethio-util)
	      (sample-text . "áŠá‹°áˆ")
	      (documentation .
"This language environment provides these function key bindings:
    [f3]   ethio-fidel-to-sera-buffer
    [S-f3] ethio-fidel-to-sera-region
    [C-f3] ethio-fidel-to-sera-marker

    [f4]   ethio-sera-to-fidel-buffer
    [S-f4] ethio-sera-to-fidel-region
    [C-f4] ethio-sera-to-fidel-marker

    [S-f5] ethio-toggle-punctuation
    [S-f6] ethio-modify-vowel
    [S-f7] ethio-replace-space

    [S-f9] ethio-replace-space
    [C-f9] ethio-toggle-space"
)))

;; For automatic composition
(aset composition-function-table ?ö ‡Š #'ethio-composition-function)
(aset composition-function-table ?áŸ #'ethio-composition-function)

(provide 'ethiopic)

;;; ethiopic.el ends here
