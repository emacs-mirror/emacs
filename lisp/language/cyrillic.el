;;; cyrillic.el --- Support for languages which use Cyrillic characters

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

;; Keywords: multilingual, Cyrillic

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

;; The character set ISO8859-5 is supported.

;;; Code:

(make-coding-system
 'coding-system-iso-8859-5 2 ?5 "MIME ISO-8859-5"
 '((ascii t) (cyrillic-iso8859-5 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil))

(register-input-method
 "Cyrillic" '("quail-jcuken" quail-use-package "quail/cyrillic"))
(register-input-method
 "Cyrillic" '("quail-macedonian" quail-use-package "quail/cyrillic"))
(register-input-method
 "Cyrillic" '("quail-serbian" quail-use-package "quail/cyrillic"))
(register-input-method
 "Cyrillic" '("quail-beylorussian" quail-use-package "quail/cyrillic"))
(register-input-method
 "Cyrillic" '("quail-ukrainian" quail-use-package "quail/cyrillic"))
(register-input-method
 "Cyrillic" '("quail-yawerty" quail-use-package "quail/cyrillic"))

(defun setup-cyrillic-environment ()
  (setq primary-language "Cyrillic")

  (setq coding-category-iso-8-1 'coding-system-iso-8859-5)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-8-1))

  (setq-default buffer-file-coding-system 'coding-system-iso-8859-5)
  (set-terminal-coding-system 'coding-system-iso-8859-5)
  (set-keyboard-coding-system 'coding-system-iso-8859-5)

  (setq default-input-method '("Cyrillic" . "quail-yawerty"))
  )

(set-language-info-alist
 "Cyrillic" '((setup-function . setup-cyrillic-environment)
	      (charset . (cyrillic-iso8859-5))
	      (coding-system . (coding-system-iso-8859-5))
	      (documentation . t)
	      (sample-text . "Russian (,L@caaZXY(B)	,L7T`PRabRcYbU(B!")))

;;; cyrillic.el ends here
