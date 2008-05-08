;;; burmese.el --- support for Burmese -*- coding: utf-8; no-byte-compile: t -*-

;; Copyright (C) 2008
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Burma, i18n

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Aung San Suu Kyi says to call her country "Burma".
;; The murderous generals say to call it "Myanmar".
;; We will call it "Burma". -- rms, Chief GNUisance.

;;; Code:

(set-language-info-alist
 "Burmese" '((charset unicode)
	     (coding-system utf-8)
	     (coding-priority utf-8)
	     (sample-text . "Burmese (မ္ရန္‌မာ)	မင္‍ဂလာပာ")
	     (documentation . t)))

(set-char-table-range composition-function-table '(#x1000 . #x107F)
		      '(("[\x1000-\x107F\x200C\x200D]+" . font-shape-text)))

;; arch-tag: 8ba5f4cd-ef89-4008-b784-397edd0cb32e
