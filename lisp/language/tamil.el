;;; tamil.el --- Support for Tamil -*- coding: iso-2022-7bit; no-byte-compile: t -*-

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation, Inc.

;; Maintainer: KAWABATA, Taichi <batta@beige.ocn.ne.jp>
;; Keywords: multilingual, Indian, Tamil

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

;; This file defines language-info of Tamil script.

;;; Code:

(set-language-info-alist
 "Tamil" '((charset mule-unicode-0100-24ff indian-glyph )
		(coding-system utf-8)
		(coding-priority utf-8)
		(input-method . "tamil-itrans")
		(features tml-util)
		(documentation . "\
South Indian Language Tamil supported in this language environment."))
 '("Indian"))

;; For automatic composition.
(set-char-table-range composition-function-table '(#x0b80 . #x0bff)
		      '(("[\x0B80-\x0BFF]+" . font-shape-text)))
(provide 'tamil)

;;; arch-tag: 2201ac78-7d1e-4674-9bcb-9923c7a2bd9c
;;; tamil.el ends here
