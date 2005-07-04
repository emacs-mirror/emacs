;;; romanian.el --- support for Romanian -*- coding: iso-latin-2; no-byte-compile: t -*-

;; Copyright (C) 1998 Free Software Foundation.

;; Author:    Dan Nicolaescu <done@ece.arizona.edu>
;; Keywords: multilingual, Romanian

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Romanian ISO 8859-2 environment.

;;; Code:

(set-language-info-alist
 "Romanian" '((charset . (ascii latin-iso8859-2))
	      (coding-system . (iso-8859-2))
	      (coding-priority . (iso-8859-2))
	      (nonascii-translation . latin-iso8859-2)
	      (input-method . "latin-2-postfix")
	      (unibyte-syntax . "latin-2")
	      (unibyte-display . iso-8859-2)
	      (tutorial . "TUTORIAL.ro")
	      (sample-text . "Bunã ziua, bine aþi venit!")
	      (documentation . t))
 '("European"))

(provide 'romanian)

;;; arch-tag: a0bf93ee-2f02-4678-a477-c08acc35366b
;;; romanian.el ends here
