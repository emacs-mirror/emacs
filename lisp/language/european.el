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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; For Europeans, five character sets ISO8859-1,2,3,4,9 are supported.

;;; Code:

(define-prefix-command 'describe-european-support-map)
(define-key-after describe-language-support-map [European]
  '("European" . describe-european-support-map)
  t)

(define-prefix-command 'setup-european-environment-map)
(define-key-after setup-language-environment-map [European]
  '("European" . setup-european-environment-map)
  t)

;; Setup for a langauge which uses one-byte 8-bit CHARSET, one-byte
;; 8-bit CODING-SYSTEM, and INPUT-METHOD.
(defun setup-8-bit-environment (charset coding-system input-method)
  (setup-english-environment)
  (setq-default buffer-file-coding-system coding-system)
  (setq coding-category-iso-8-1 coding-system
	coding-category-iso-8-2 coding-system)
  (set-terminal-coding-system-internal coding-system)
  (set-keyboard-coding-system-internal coding-system)
  (setq sendmail-coding-system nil
	rmail-file-coding-system coding-system)

  (if charset
      (let ((nonascii-offset (make-char charset)))
	(setq nonascii-insert-offset nonascii-offset
	      set-case-syntax-offset nonascii-offset)))

  (if input-method
      (setq default-input-method input-method)))

;; Latin1 (ISO-8859-1)

(make-coding-system
 'iso-8859-1 2 ?X
 "MIME ISO-8859-1 Compound Text Encoding."
 '((ascii t) (latin-iso8859-1 t) nil nil
   nil ascii-eol ascii-cntl))

;; CTEXT is an alias for ISO-8859-1
(define-coding-system-alias 'iso-8859-1 'ctext)

(register-input-method "Latin1"
		       '("quail-latin-1" quail-use-package "quail/latin"))

(defun setup-latin1-environment ()
  "Setup multilingual environment (MULE) for European Latin1 users."
  (interactive)
  (setup-8-bit-environment 'latin-iso8859-1 'iso-8859-1
			   '("Latin1" . "quail-latin-1")))

(set-language-info-alist
 "Latin1" '((setup-function . (setup-latin1-environment
			       . setup-european-environment-map))
	    (charset . (ascii latin-iso8859-1))
	    (coding-system . (iso-8859-1))
	    (sample-text
	     . "Hello, Hej, Tere, Hei, Bonjour, Gr,A|_(B Gott, Ciao, ,A!(BHola!")
	    (documentation . ("\
The following languages is supported by Latin1 (ISO-8859-1) character set.
" . describe-european-support-map))
	    ))

;; Latin2 (ISO-8859-2)

(make-coding-system
 'iso-8859-2 2 ?2 "MIME ISO-8859-2"
 '((ascii t) (latin-iso8859-2 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil))

(register-input-method "Latin2"
		       '("quail-latin-2" quail-use-package "quail/latin"))

(defun setup-latin2-environment ()
  "Setup multilingual environment (MULE) for European Latin2 users."
  (interactive)
  (setup-8-bit-environment 'latin-iso8859-2 'iso-8859-2
			   '("Latin2" . "quail-latin-2")))

(set-language-info-alist
 "Latin2" '((setup-function . (setup-latin2-environment
			       . setup-european-environment-map))
	    (charset . (ascii latin-iso8859-2))
	    (coding-system . (iso-8859-2))
	    (documentation . ("\
The following languages is supported by Latin2 (ISO-8859-2) character set.
" . describe-european-support-map))
	    ))

;; Latin3 (ISO-8859-3)

(make-coding-system
 'iso-8859-3 2 ?3 "MIME ISO-8859-3"
 '((ascii t) (latin-iso8859-3 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil))

(register-input-method "Latin3"
		       '("quail-latin-3" quail-use-package "quail/latin"))

(defun setup-latin3-environment ()
  "Setup multilingual environment (MULE) for European Latin3 users."
  (interactive)
  (setup-8-bit-environment 'latin-iso8859-3 'iso-8859-3
			   '("Latin3" . "quail-latin-3")))

(set-language-info-alist
 "Latin3" '((setup-function . (setup-latin3-environment
			       . setup-european-environment-map))
	    (charset . (ascii latin-iso8859-3))
	    (coding-system . (iso-8859-3))
	    (documentation . ("\
The following languages is supported by Latin3 (ISO-8859-3) character set.
" . describe-european-support-map))
	    ))

;; Latin4 (ISO-8859-4)

(make-coding-system
 'iso-8859-4 2 ?4 "MIME ISO-8859-4"
 '((ascii t) (latin-iso8859-4 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil))

(register-input-method "European"
		       '("quail-latin-4" quail-use-package "quail/latin"))

(defun setup-latin4-environment ()
  "Setup multilingual environment (MULE) for European Latin4 users."
  (interactive)
  (setup-8-bit-environment 'latin-iso8859-4 'iso-8859-4
			   '("Latin4" . "quail-latin-4")))

(set-language-info-alist
 "Latin4" '((setup-function . (setup-latin4-environment
			       . setup-european-environment-map))
	    (charset . (ascii latin-iso8859-1))
	    (coding-system . (iso-8859-4))
	    (documentation . ("\
The following languages is supported by Latin4 (ISO-8859-4) character set.
" . describe-european-support-map))
	    ))

;; Latin4 (ISO-8859-9)

(make-coding-system
 'iso-8859-9 2 ?9 "MIME ISO-8859-9"
 '((ascii t) (latin-iso8859-9 t) nil nil
   nil ascii-eol ascii-cntl nil nil nil nil))

(register-input-method "European"
		       '("quail-latin-5" quail-use-package "quail/latin"))

(defun setup-latin5-environment ()
  "Setup multilingual environment (MULE) for European Latin5 users."
  (interactive)
  (setup-8-bit-environment 'latin-iso8859-9 'iso-8859-9
			   '("Latin5" . "quail-latin-5")))

(set-language-info-alist
 "Latin5" '((setup-function . (setup-latin5-environment
			       . setup-european-environment-map))
	    (charset . (ascii latin-iso8859-9))
	    (coding-system . (iso-8859-9))
	    (documentation . ("\
The following languages is supported by Latin5 (ISO-8859-9) character set.
" . describe-european-support-map))
	    ))

(let ((languages '("French" "German" "Spanish" "Italian"
		   ;; We have to list much more European languages here.
		   ))
      (val '("quail-latin-1" quail-use-package "quail/latin")))
  (while languages
    (register-input-method (car languages) val)
    (setq languages (cdr languages))))

;;; european.el ends here
