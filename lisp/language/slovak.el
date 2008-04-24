;;; slovak.el --- support for Slovak -*- coding: iso-2022-7bit; no-byte-compile: t -*-

;; Copyright (C) 1998, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
;;   Free Software Foundation.

;; Authors:    Tibor ,B)(Bimko <tibor.simko@fmph.uniba.sk>,
;;             Milan Zamazal <pdm@zamazal.org>
;; Maintainer: Pavel Jan,Am(Bk <Pavel@Janik.cz>
;; Keywords: multilingual, Slovak

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

;; Slovak ISO 8859-2 environment.

;;; Code:

(set-language-info-alist
 "Slovak" '((charset . (ascii latin-iso8859-2))
	    (coding-system . (iso-8859-2))
	    (coding-priority . (iso-8859-2))
	    (nonascii-translation . iso-8859-2)
	    (input-method . "slovak")
	    (unibyte-display . iso-8859-2)
	    (tutorial . "TUTORIAL.sk")
	    (sample-text . "Prajeme V,Ba(Bm pr,Bm(Bjemn,B}(B de,Br(B!")
	    (documentation . "\
This language environment is almost the same as Latin-2,
but sets the default input method to \"slovak\",
and selects the Slovak tutorial."))
 '("European"))

(provide 'slovak)

;; arch-tag: 1bae098a-33b2-4426-8c29-59e44fe05484
;;; slovak.el ends here
