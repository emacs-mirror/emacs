;;; hebrew.el --- Support for Hebrew

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

;; Keywords: multilingual, Hebrew

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

;; For Hebrew, the character sets ISO8859-8 is supported.

;;; Code:

(make-coding-system
 'iso-8859-8 2 ?8 "MIME ISO-8859-8"
 '((ascii t) (hebrew-iso8859-8 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil nil t))

(register-input-method
 "Hebrew" '("quail-hebrew" quail-use-package "quail/hebrew"))

(defun setup-hebrew-environment ()
  (setq coding-category-iso-8-1 'iso-8859-8)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-8-1
     coding-category-iso-8-2))

  (setq-default buffer-file-coding-system 'iso-8859-8)
  (set-terminal-coding-system 'iso-8859-8)
  (set-keyboard-coding-system 'iso-8859-8)

  (setq default-input-method '("Hebrew" . "quail-hebrew"))
  )

(set-language-info-alist
 "Hebrew" '((setup-function . setup-hebrew-environment)
	    (charset . (hebrew-iso8859-8))
	    (coding-system . (iso-8859-8))
	    (documentation . "Right-to-left writing is Not yet supported")
	    (sample-text . "Hebrew	,Hylem(B")))

;;; hebew.el ends here
