;;; lao.el --- support for Lao -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.
;; Copyright (C) 2001 Free Software Foundation, Inc.

;; Keywords: multilingual, Lao

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

;;; Code:

(define-coding-system 'lao
  "8-bit encoding for ASCII (MSB=0) and LAO (MSB=1)."
  :coding-type 'charset
  :mnemonic ?L
  :charset-list '(lao))

(set-language-info-alist
 "Lao" '((charset lao)
	 (coding-system lao)
	 (coding-priority lao)
	 (input-method . "lao")
	 (unibyte-display . lao)
	 (features lao-util)
	 (documentation . t)))

;; Register a function to compose Lao characters.
(set-char-table-range composition-function-table
		      '(#x0F00 . #x0F7F) 
		      '(("\\c0\\c9?\\(\\(\\c2\\|\\c3\\)\\c4?\\|\\c4\\)?"
			 . lao-composition-function)))

(provide 'lao)

;;; lao.el ends here
