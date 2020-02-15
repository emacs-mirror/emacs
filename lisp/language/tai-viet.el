;;; tai-viet.el --- support for Tai Viet -*- coding: utf-8 -*-

;; Copyright (C) 2007-2020 Free Software Foundation, Inc.
;; Copyright (C) 2007, 2008, 2009, 2010, 2011
;;   National Institute of Advanced Industrial Science and Technology (AIST)
;;   Registration Number H13PRO009

;; Keywords: multilingual, Tai Viet, i18n

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

;;; Commentary:

;; Tai Viet is being included in the Unicode at the range U+AA80..U+AADF.

;;; Code:

(set-char-table-range composition-function-table
		      '(#xAA80 . #xAADF)
		      'tai-viet-composition-function)

(set-language-info-alist
 "TaiViet" '((charset unicode)
	      (coding-system utf-8)
	      (coding-priority utf-8)
	      (input-method . "tai-sonla")
	      (sample-text . "TaiViet (ꪁꪫꪱꪣ ꪼꪕ)\t\tꪅꪰꪙꫂ ꪨꪮꫂ ꪁꪫꪱ / ꪅꪽ ꪨꪷ ꪁꪫꪱ")
	      (documentation . "\
TaiViet refers to the Tai script, which is used to write several
Tai languages of northwestern Vietnam and surrounding areas.  These
languages are Tai Dam (also known as Black Tai or Tai Noir),
Tai Dón (also known as White Tai or Tai Blanc), Tày Tac,
Tai Daeng (also known as Red Tai or Tai Rouge),
and Thai Song (also known as Lao Song).  However, some people
consider Tai Dam, Tai Dón and Tai Daeng to be dialects of the
same language, and call them collectively \"Tai Viet\".

Both the script and languages have the same origin as that of Thai
language/script used in Thailand, but now they differ from each
other in a significant way (especially the scripts are).

The language name is spelled as \"ꪁꪫꪱꪣ ꪼꪕ\", and the script name is
spelled as \"ꪎꪳ ꪼꪕ\".")))

(provide 'tai-viet)
