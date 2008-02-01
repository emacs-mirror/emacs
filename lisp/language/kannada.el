;;; kannada.el --- Support for Kannada -*- coding: iso-2022-7bit; no-byte-compile: t -*-

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

;; Maintainer:  CHOWKSEY, Kailash C. <klchxbec@m-net.arbornet.org>
;; Keywords: multilingual, Indian, Kannada

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file defines language-info of Kannada script.

;;; Code:

(set-language-info-alist
 "Kannada" '((charset mule-unicode-0100-24ff indian-is13194
                         indian-2-column indian-glyph ;; comment out later
                         )
		(coding-system mule-utf-8)
		(coding-priority mule-utf-8)
		(input-method . "kannada-itrans")
		(features knd-util)
		(sample-text
		 . (kannada-compose-string
		    (copy-sequence "Kannada ($,1>u?(?M?(?!(B)	$,1?(?.?8?M>u?>?0(B")))
		(documentation . "\
Kannada language and script is supported in this language
environment.")) 
 '("Indian"))

;; For automatic composition.
(set-char-table-range composition-function-table '(#x0c80 . #x0cff)
		      '(("[\x0c80-\x0CFF]+" . font-shape-text)))

(provide 'kannada)

;;; arch-tag: 880ba90b-f6f5-4131-bc1d-930705b78416
;;; kannada.el ends here
