;;; cham.el --- support for Cham -*- coding: utf-8; no-byte-compile: t -*-

;; Copyright (C) 2008
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Cham, i18n

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

;; Tai Viet is being included in the Unicode at the range U+AA80..U+AADF.

;;; Code:

(set-char-table-range composition-function-table
		      '(#xAA00 . #xAA5F)
		      '(("[\xAA00-\xAA5F]+" . font-shape-text)))

(set-language-info-alist
 "Cham" '((charset unicode)
	      (coding-system utf-8)
	      (coding-priority utf-8)))

(provide 'cham)

;; Local Variables:
;; coding: utf-8
;; End:

;; arch-tag: a393ea52-445b-4e22-a967-c244afc88cf6
