;;; greek.el --- Support for Greek

;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.
;; Licensed to the Free Software Foundation.

;; Keywords: multilingual, Greek

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

;; For Greek, the character set ISO8859-7 is supported.

;;; Code:

(make-coding-system
 'greek-iso-8bit 2 ?7
 "ISO 2022 based 8-bit encoding for Greek (MIME:ISO-8859-7)"
 '((ascii t) (greek-iso8859-7 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil))

(define-coding-system-alias 'iso-8859-7 'greek-iso-8bit)

(defun setup-greek-environment ()
  "Setup multilingual environment (MULE) for Greek."
  (interactive)
  (setup-8-bit-environment "Greek" 'greek-iso8859-7 'greek-iso-8bit
			   "greek"))

(set-language-info-alist
 "Greek" '((setup-function . setup-greek-environment)
	   (charset . (greek-iso8859-7))
	   (coding-system . (greek-iso-8bit))
	   (sample-text . "Greek (,FGkk]mija(B)	,FCei\(B ,Fsar(B")
	   (documentation . t)))

;;; greek.el ends here
