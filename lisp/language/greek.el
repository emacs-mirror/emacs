;;; greek.el --- Support for Greek

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

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
 'iso-8859-7 2 ?7 "MIME ISO-8859-7"
 '((ascii t) (greek-iso8859-7 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil))

(register-input-method
 "Greek" '("quail-greek" quail-use-package "quail/greek"))

(defun setup-greek-environment ()
  (setq coding-category-iso-8-1 'iso-8859-7)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-8-1))

  (setq-default buffer-file-coding-system 'iso-8859-7)
  (set-terminal-coding-system 'iso-8859-7)
  (set-keyboard-coding-system 'iso-8859-7)

  (setq default-input-method '("Greek" . "quail-greek"))
  )

(set-language-info-alist
 "Greek" '((setup-function . setup-greek-environment)
	   (charset . (greek-iso8859-7))
	   (coding-system . (iso-8859-7))
	   (documentation . t)
	   (sample-text . "Greek (,FGkk]mija(B)	,FCei\(B ,Fsar(B")))

;;; greek.el ends here
