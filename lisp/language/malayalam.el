;;; malayalam.el --- Support for Malayalam -*- coding: iso-2022-7bit; no-byte-compile: t -*-

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008
;    Free Software Foundation, Inc.

;; Maintainer:  KAWABATA, Taichi <kawabata@m17n.org>
;; Keywords: multilingual, Indian, Malayalam

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

;; This file defines language-info of Malayalam script.

;;; Code:

(set-language-info-alist
 "Malayalam" '((charset mule-unicode-0100-24ff indian-glyph )
               ;;          indian-2-column 
               ;; comment out later
               ;;          )
		(coding-system utf-8)
		(coding-priority utf-8)
		(input-method . "malayalam-itrans")
		(features mlm-util)
		(documentation . "\
South Indian language Malayalam is supported in this language environment."))
 '("Indian"))

;; For automatic composition.
(set-char-table-range composition-function-table '(#x0d00 . #x0d7f)
		      '(("[\x0D00-\x0D7F]+" . font-shape-text)))

(provide 'malayalam)

;;; arch-tag: 5f500e53-1e4f-4bb2-aa93-ad8736f0349f
;;; malayalam.el ends here
