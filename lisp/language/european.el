;;; european.el --- Support for European languages

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995 Electrotechnical Laboratory, JAPAN.

;; Keywords: multilingual, European

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; For Europeans, five character sets ISO8859-1,2,3,4,9 are supported.

;;; Code:

(make-coding-system
 'coding-system-iso-8859-1 2 ?X
 "Coding-system used in X as Compound Text Encoding."
 '((ascii t) (latin-iso8859-1 t) nil nil
   nil ascii-eol ascii-cntl))

;; CTEXT is an alias for ISO-8859-1
(put 'coding-system-ctext 'coding-system 'coding-system-iso-8859-1)

(make-coding-system
 'coding-system-iso-8859-2 2 ?2 "MIME ISO-8859-2"
 '((ascii t) (latin-iso8859-2 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil))

(make-coding-system
 'coding-system-iso-8859-3 2 ?3 "MIME ISO-8859-3"
 '((ascii t) (latin-iso8859-3 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil))

(make-coding-system
 'coding-system-iso-8859-4 2 ?4 "MIME ISO-8859-4"
 '((ascii t) (latin-iso8859-4 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil))

(make-coding-system
 'coding-system-iso-8859-9 2 ?9 "MIME ISO-8859-9"
 '((ascii t) (latin-iso8859-9 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil))

(register-input-method "European"
		       '("quail-latin-1" quail-use-package "quail/latin"))
(register-input-method "European"
		       '("quail-latin-2" quail-use-package "quail/latin"))
(register-input-method "European"
		       '("quail-latin-3" quail-use-package "quail/latin"))
(register-input-method "European"
		       '("quail-latin-4" quail-use-package "quail/latin"))
(register-input-method "European"
		       '("quail-latin-5" quail-use-package "quail/latin"))

(defun setup-european-environment ()
  (setq coding-category-iso-8-1 'coding-system-iso-8859-1)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-8-1
     coding-category-iso-8-2))

  (setq-default buffer-file-coding-system 'coding-system-iso-8859-1)
  (set-terminal-coding-system 'coding-system-iso-8859-1)
  (set-keyboard-coding-system 'coding-system-iso-8859-1)

  (setq default-input-method '("European" . "quail-latin-1"))
  )

(set-language-info "English" 'tutorial "TUTORIAL")

(register-input-method "French"
		       '("quail-latin-1" quail-use-package "quail/latin"))
(register-input-method "French"
		       '("quail-latin-1" quail-use-package "quail/latin"))

(set-language-info-alist
 "European" '((setup-function . setup-european-environment)
	      (charset . (ascii latin-iso8859-1 latin-iso8859-2
			   latin-iso8859-3 latin-iso8859-4 latin-iso8859-9))
	      (coding-system . (coding-system-iso-8859-1
				coding-system-iso-8859-2
				coding-system-iso-8859-3
				coding-system-iso-8859-4
				coding-system-iso-8859-9))
	      (documentation . t)
	      (sample-text
	       . "Hello, Hej, Tere, Hei, Bonjour, Gr,A|_(B Gott, Ciao, ,A!(BHola!")))

;;; european.el ends here
