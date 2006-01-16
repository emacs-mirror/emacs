;;; esh-maint.el --- init code for building eshell -*- no-byte-compile: t -*-

;; Copyright (C) 1999, 2000, 2002, 2003, 2004,
;;   2005 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>

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

;;; Code:

(provide 'esh-maint)

(and (fboundp 'font-lock-add-keywords)
     (font-lock-add-keywords
      'emacs-lisp-mode
      '(("(eshell-for\\>"            . font-lock-keyword-face)
	("(eshell-deftest\\>"        . font-lock-keyword-face)
	("(eshell-condition-case\\>" . font-lock-keyword-face))))

(if (file-directory-p "../pcomplete")
    (add-to-list 'load-path "../pcomplete"))

(if (locate-library "pcomplete")
    (require 'pcomplete))

(eval-when-compile
  (require 'cl)
  (setq cl-optimize-speed 9))

;; (defun eshell-generate-autoloads ()
;;   (interactive)
;;   (require 'autoload)
;;   (setq generated-autoload-file
;;	(expand-file-name (car command-line-args-left)))
;;   (setq command-line-args-left (cdr command-line-args-left))
;;   (batch-update-autoloads))

(require 'eshell)
(require 'esh-mode)    ; brings in eshell-util
(require 'esh-opt)
(require 'esh-test)

;; (defun eshell-generate-main-menu ()
;;   "Create the main menu for the eshell documentation."
;;   (insert "@menu
;; * The Emacs shell::                 eshell.

;; Core Functionality\n")
;;   (eshell-for module
;;       (sort (eshell-subgroups 'eshell)
;;	    (function
;;	     (lambda (a b)
;;	       (string-lessp (symbol-name a)
;;			     (symbol-name b)))))
;;     (insert (format "* %-34s"
;;		    (concat (get module 'custom-tag) "::"))
;;	    (symbol-name module) ".\n"))
;;   (insert "\nOptional Functionality\n")
;;   (eshell-for module
;;       (sort (eshell-subgroups 'eshell-module)
;;	    (function
;;	     (lambda (a b)
;;	       (string-lessp (symbol-name a)
;;			     (symbol-name b)))))
;;     (insert (format "* %-34s"
;;		    (concat (get module 'custom-tag) "::"))
;;	    (symbol-name module) ".\n"))
;;   (insert "@end menu\n"))

;; (defun eshell-make-texi ()
;;   "Make the eshell.texi file."
;;   (interactive)
;;   (require 'eshell-auto)
;;   (require 'texidoc)
;;   (require 'pcomplete)
;;   (apply 'texidoc-files 'eshell-generate-main-menu "eshell.doci"
;;	 (append
;;	  (list "eshell.el")
;;	  (sort (mapcar
;;		 (function
;;		  (lambda (sym)
;;		    (let ((name (symbol-name sym)))
;;		      (if (string-match "\\`eshell-\\(.*\\)" name)
;;			  (setq name (concat "esh-" (match-string 1 name))))
;;		      (concat name ".el"))))
;;		 (eshell-subgroups 'eshell))
;;		'string-lessp)
;;	  (sort (mapcar
;;		 (function
;;		  (lambda (sym)
;;		    (let ((name (symbol-name sym)))
;;		      (if (string-match "\\`eshell-\\(.*\\)" name)
;;			  (setq name (concat "em-" (match-string 1 name))))
;;		      (concat name ".el"))))
;;		 (eshell-subgroups 'eshell-module))
;;		'string-lessp)
;;	  (list "eshell.texi"))))

;; (defun eshell-make-readme ()
;;   "Make the README file from eshell.el."
;;   (interactive)
;;   (require 'eshell-auto)
;;   (require 'texidoc)
;;   (require 'pcomplete)
;;   (texidoc-files nil "eshell.doci" "eshell.el" "README.texi")
;;   (set-buffer (get-buffer "README.texi"))
;;   (goto-char (point-min))
;;   (search-forward "@chapter")
;;   (beginning-of-line)
;;   (forward-line -1)
;;   (kill-line 2)
;;   (re-search-forward "^@section User Options")
;;   (beginning-of-line)
;;   (delete-region (point) (point-max))
;;   (insert "@bye\n")
;;   (save-buffer)
;;   (with-temp-buffer
;;     (call-process "makeinfo" nil t nil "--no-headers" "README.texi")
;;     (goto-char (point-min))
;;     (search-forward "The Emacs Shell")
;;     (beginning-of-line)
;;     (delete-region (point-min) (point))
;;     (write-file "README"))
;;   (delete-file "README.texi")
;;   (kill-buffer "README.texi"))

;;; arch-tag: 662089b6-78ec-48c5-b94f-d212279e8902
;;; esh-maint.el ends here
