;;; english.el --- English support

;; Copyright (C) 1997 Free Software Foundation, Inc.
;; Copyright (C) 1997 Electrotechnical Laboratory, JAPAN.

;; Keywords: multibyte character, character set, syntax, category

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

;; We need nothing special to support English on Emacs.  Selecting
;; English as a language environment is one of the ways to reset
;; various multilingual environment to the original settting.

;;; Code

(defun setup-english-environment ()
  "Reset multilingual environment of Emacs to the default status.
The default status is as follows.

  The default value of enable-multibyte-characters is t.

  The default value of buffer-file-coding-system is iso-8859-1.
  The coding system for terminal output is nil.
  The coding system for keyboard input is nil.

  The order of priorities of coding categories and the coding system
  bound to each category are as follows
	coding category			coding system
	--------------------------------------------------
	coding-category-iso-7		iso-2022-7
	coding-category-iso-8-2		iso-8859-1
	coding-category-iso-8-1		iso-8859-1
	coding-category-iso-else	iso-8859-1
	coding-category-internal 	internal
	coding-category-binary		no-conversion
	coding-category-sjis		sjis
	coding-category-big5		big5
"
  (interactive)
  (setq-default enable-multibyte-characters t)
  (if (local-variable-p 'enable-multibyte-characters)
      (setq enable-multibyte-characters t))

  (setq coding-category-internal	'internal
	coding-category-iso-7		'iso-2022-7
	coding-category-iso-8-1		'iso-8859-1
	coding-category-iso-8-2		'iso-8859-1
	coding-category-iso-else	'iso-8859-1
	coding-category-sjis		'sjis
	coding-category-big5		'big5
	coding-category-binary		'no-conversion)

  (set-coding-priority
   '(coding-category-iso-7
     coding-category-iso-8-2
     coding-category-iso-8-1
     coding-category-iso-else
     coding-category-internal 
     coding-category-binary
     coding-category-sjis
     coding-category-big5))

  (setq-default buffer-file-coding-system 'iso-8859-1)
  (set-terminal-coding-system nil)
  (set-keyboard-coding-system nil)

  (setq sendmail-coding-system nil
	rmail-file-coding-system nil)
  )

(defun describe-english-support ()
  "Describe how Emacs support English."
  (interactive)
  (describe-language-support-internal "English"))

(set-language-info-alist
 "English" '((setup-function . setup-english-environment)
	     (describe-function . describe-english-support)
	     (tutorial . "TUTORIAL")
	     (charset . (ascii))
	     (sample-text . "Hello!, Hi!, How are you?")
	     (documentation . "\
There's nothing special you should care to handle English in Emacs.
You can use English both with enable-multibyte-characters t and nil.")
	     ))

(register-input-method "English"
		       '("quail-dvorak" quail-use-package "quail/latin"))

;;; english.el ends here
