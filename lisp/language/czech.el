;;; czech.el --- support for Czech -*- coding: iso-2022-7bit; no-byte-compile: t -*-

;; Copyright (C) 1998, 2001, 2002, 2003, 2004, 2005, 2006, 2007
;;   Free Software Foundation.

;; Author: Milan Zamazal <pdm@zamazal.org>
;; Maintainer: Pavel Jan,Am(Bk <Pavel@Janik.cz>
;; Keywords: multilingual, Czech

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

;; Czech ISO 8859-2 environment.

;;; Code:

(set-language-info-alist
 "Czech" '((charset . (ascii latin-iso8859-2))
	   (coding-system . (iso-8859-2))
	   (coding-priority . (iso-8859-2))
	   (input-method . "czech")
	   (nonascii-translation . latin-iso8859-2)
	   (unibyte-syntax . "latin-2")
	   (unibyte-display . iso-8859-2)
	   (tutorial . "TUTORIAL.cs")
	   (sample-text . "P,Bx(Bejeme v,Ba(Bm hezk,B}(B den!")
	   (documentation . "\
This language environment is almost the same as Latin-2,
but sets the default input method to \"czech\",
and selects the Czech tutorial."))
 '("European"))

(provide 'czech)

;;; arch-tag: 45ac0d83-ca13-4b5e-9e82-821e44080c24
;;; czech.el ends here
