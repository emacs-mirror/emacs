;;; cus-test.el --- functions for testing custom variable definitions

;; Copyright (C) 1998, 2000, 2002 Free Software Foundation, Inc.

;; Author: Markus Rost <markus.rost@mathematik.uni-regensburg.de>
;; Maintainer: Markus Rost <rost@math.ohio-state.edu>
;; Created: 13 Sep 1998
;; Keywords: maint

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

;; Some user options in GNU Emacs have been defined with incorrect
;; customization types.  As a result the customization of these
;; options is disabled.  This file provides functions to detect such
;; options.  It contains also simple tests for loading libraries and
;; custom dependencies.
;;
;; Usage: Load this file.  Then
;;
;;    M-x cus-test-apropos REGEXP RET
;;
;; checks the options matching REGEXP.	In particular
;;
;;    M-x cus-test-apropos RET
;;
;; checks all options.  The detected options are stored in the
;; variable `cus-test-errors'.
;;
;; Only those options are checked which have been already loaded.
;; Therefore `cus-test-apropos' is more efficient after loading many
;; libraries.
;;
;;    M-x cus-test-load-custom-loads RET
;;
;; loads all (!) custom dependencies.
;;
;; Options with a custom-get property, usually defined by a :get
;; declaration, are stored in the variable
;;
;; `cus-test-vars-with-custom-get'
;;
;; Options with a state of 'changed ("changed outside the customize
;; buffer") are stored in the variable
;;
;; `cus-test-vars-with-changed-state'
;;
;; These lists are prepared just in case one wants to investigate
;; those options further.
;;
;; For a maximal test of custom options invoke
;;
;;    M-x cus-test-opts
;;
;; Other test routines are `cus-test-deps' and `cus-test-libs'.
;; These functions are suitable for batch mode.  Invoke them with
;;
;;   src/emacs -batch -l admin/cus-test.el -f cus-test-opts
;;
;;   src/emacs -batch -l admin/cus-test.el -f cus-test-deps
;;
;;   src/emacs -batch -l admin/cus-test.el -f cus-test-libs
;;
;; in the emacs source directory.
;;
;; To make cus-test work one has usually to work-around some existing
;; bugs/problems.  Therefore this file contains "Fixme" and
;; "Workarounds" sections, to be edited once in a while.
;;
;; Results from Oct 10, 2002:
;;
;; Cus Test tested 4514 options.
;; The following variables might have problems:
;; (ps-mule-font-info-database-default)

;; Cus Test Deps loaded 332 files.
;; The following load problems appeared:
;; ((killing x-win (file-error Cannot open load file x-win)))

;; Cus Test Libs loaded 424 files.
;; No load problems encountered by Cus Test Libs

;;; Code:

;;; Variables for workarounds:

(defvar cus-test-after-load-libs-hook nil
  "Hook to repair the worst side effects of loading buggy libraries.")

(defvar cus-test-libs-noloads nil
  "List of libraries not to load by `cus-test-libs'.")

;;; Fixme:

;; Loading filesets.el currently disables mini-buffer echoes.
;; (add-to-list 'cus-test-libs-noloads "filesets")
(add-hook
 'cus-test-after-load-libs-hook
 (lambda nil
   (remove-hook 'menu-bar-update-hook 'filesets-build-menu-maybe)
   (remove-hook 'kill-emacs-hook 'filesets-exit)
   (remove-hook 'kill-buffer-hook 'filesets-remove-from-ubl)
   (remove-hook 'first-change-hook 'filesets-reset-filename-on-change)
   ))
;; (setq cus-test-after-load-libs-hook nil)

;; eshell must be loaded before em-script.  eshell loads esh-util,
;; which must be loaded before em-cmpl, em-dirs and similar libraries.
(load "eshell")

;; reftex must be loaded before reftex-vars.
(load "reftex")

;;; Workarounds:

;; The file eudc-export.el loads libraries "bbdb" and "bbdb-com" which
;; are not part of GNU Emacs:  (locate-library "bbdb") => nil

;; This avoids the resulting errors from loading eudc-export.el.
(provide 'bbdb)
(provide 'bbdb-com)

;; Loading dunnet in batch mode leads to a Dead end.
(let (noninteractive)
  (load "dunnet"))
(add-to-list 'cus-test-libs-noloads "dunnet")

;;; Silencing:

;; Don't create a file `filesets-menu-cache-file'.
(setq filesets-menu-cache-file "")

;; Don't create a file `save-place-file'.
(eval-after-load "saveplace"
  '(remove-hook 'kill-emacs-hook 'save-place-kill-emacs-hook))

;; Don't create a file `abbrev-file-name'.
(setq save-abbrevs nil)

;; Avoid compile logs from adviced functions.
(eval-after-load "bytecomp"
  '(setq ad-default-compilation-action 'never))

;; We want to log all messages.
(setq message-log-max t)


;;; Main Code:

(require 'cus-edit)
(require 'cus-load)

(defvar cus-test-tested-variables nil
  "Options tested by last call of `cus-test-apropos'.")

(defvar cus-test-errors nil
  "List of problematic variables found by `cus-test-apropos'.")

(defvar cus-test-deps-errors nil
  "List of require/load problems found by `cus-test-deps'.")

(defvar cus-test-deps-tested nil
  "Dependencies loaded by `cus-test-deps'.")

(defvar cus-test-libs-errors nil
  "List of load problems found by `cus-test-libs'.")

(defvar cus-test-libs-loaded nil
  "Files loaded by `cus-test-libs'.")

;; I haven't understood this :get stuff.  However, there are only very
;; few variables with a custom-get property.  Such symbols are stored
;; in `cus-test-vars-with-custom-get'.
(defvar cus-test-vars-with-custom-get nil
  "Set by `cus-test-apropos' to a list of options with :get property.")

(defvar cus-test-vars-with-changed-state nil
  "Set by `cus-test-apropos' to a list of options with state 'changed.")

(defun cus-test-apropos (regexp)
  "Check the options matching REGEXP.
The detected problematic options are stored in `cus-test-errors'."
  (interactive "sVariable regexp: ")
  (setq cus-test-errors nil)
  (setq cus-test-tested-variables nil)
  (mapcar
   (lambda (symbol)
     (push symbol cus-test-tested-variables)
     (unless noninteractive
       (message "Cus Test Running...[%s]"
		(length cus-test-tested-variables)))
     (condition-case alpha
	 (let* ((type (custom-variable-type symbol))
		(conv (widget-convert type))
		(get (or (get symbol 'custom-get) 'default-value))
		values
		mismatch)
	   (when (default-boundp symbol)
	     (push (funcall get symbol) values)
	     (push (eval (car (get symbol 'standard-value))) values))
	   (if (boundp symbol)
	       (push (symbol-value symbol) values))
	   ;; That does not work.
	   ;; (push (widget-get conv :value) values)

	   ;; Check the values
	   (mapcar (lambda (value)
		     (unless (widget-apply conv :match value)
		       (setq mismatch 'mismatch)))
		   values)

	   ;; Store symbols with a custom-get property.
	   (when (get symbol 'custom-get)
	     (push symbol cus-test-vars-with-custom-get))

	   ;; Changed outside the customize buffer?
	   ;; This routine is not very much tested.
	   (let ((c-value
		  (or (get symbol 'customized-value)
		      (get symbol 'saved-value)
		      (get symbol 'standard-value))))
	     (and (consp c-value)
		  (boundp symbol)
		  (not (equal (eval (car c-value)) (symbol-value symbol)))
		  (push symbol cus-test-vars-with-changed-state)))

	   (if mismatch
	       (push symbol cus-test-errors)))

       (error
	(push symbol cus-test-errors)
	(message "Error for %s: %s" symbol alpha))))
   (cus-test-get-options regexp))
  (message "Cus Test tested %s options."
	   (length cus-test-tested-variables))
  (cus-test-errors-display))

(defun cus-test-get-options (regexp)
  "Return a list of custom options matching REGEXP."
  (let (found)
    (mapatoms
     (lambda (symbol)
       (and
	(or
	 ;; (user-variable-p symbol)
	 (get symbol 'standard-value)
	 ;; (get symbol 'saved-value)
	 (get symbol 'custom-type))
	(string-match regexp (symbol-name symbol))
	;;	(not (member symbol cus-test-strange-vars))
	(push symbol found))))
    found))

(defun cus-test-errors-display ()
  "Report about the errors found by cus-test."
  (with-output-to-temp-buffer "*cus-test-errors*"
    (set-buffer standard-output)
    (insert (format "Cus Test tested %s variables.\
  See `cus-test-tested-variables'.\n\n"
		    (length cus-test-tested-variables)))
    (if cus-test-errors
	(let ((L cus-test-errors))
	  (insert "The following variables seem to have errors:\n\n")
	  (while L (insert (symbol-name (car L))) (insert "\n")
		 (setq L (cdr L))))
      (insert "No errors found by cus-test."))))

(defun cus-test-load-custom-loads nil
  "Call `custom-load-symbol' on all atoms."
  (interactive)
  (mapatoms 'custom-load-symbol)
  (run-hooks 'cus-test-after-load-libs-hook))

;;; The routines for batch mode:

(defun cus-test-opts nil
  "Test custom options.
This function is suitable for batch mode.  E.g., invoke

  src/emacs -batch -l admin/cus-test.el -f cus-test-opts

in the emacs source directory."
  (interactive)
  (message "Running %s" 'cus-test-load-custom-loads)
  (cus-test-load-custom-loads)
  (message "Running %s" 'cus-test-apropos)
  (cus-test-apropos "")
  (if cus-test-errors
      (message "The following options might have problems:\n%s"
	       cus-test-errors)
    (message "No problems found by Cus Test Opts")))

(defun cus-test-deps nil
  "Run a verbose version of `custom-load-symbol' on all atoms.
This function is suitable for batch mode.  E.g., invoke

  src/emacs -batch -l admin/cus-test.el -f cus-test-deps

in the emacs source directory."
  (interactive)
  (setq cus-test-deps-errors nil)
  (setq cus-test-deps-tested nil)
  (mapatoms
   ;; This code is mainly from `custom-load-symbol'.
   (lambda (symbol)
     (unless custom-load-recursion
       (let ((custom-load-recursion t))
	 (dolist (load (get symbol 'custom-loads))
	   (cond
	    ((symbolp load)
	     ;; (condition-case nil (require load) (error nil))
	     (condition-case alpha
		 (require load)
	       (error
		(push (list symbol load alpha) cus-test-deps-errors)
		(message "Require problem: %s %s: %s" symbol load alpha)))
	     (push (list symbol load) cus-test-deps-tested))
	    ;; This is subsumed by the test below, but it's much
	    ;; faster.
	    ((assoc load load-history))
	    ;; This was just
	    ;; (assoc (locate-library load) load-history)
	    ;; but has been optimized not to load locate-library
	    ;; if not necessary.
	    ((let ((regexp (concat "\\(\\`\\|/\\)" (regexp-quote load)
				   "\\(\\'\\|\\.\\)"))
		   (found nil))
	       (dolist (loaded load-history)
		 (and (stringp (car loaded))
		      (string-match regexp (car loaded))
		      (setq found t)))
	       found))
	    ;; Without this, we would load cus-edit recursively.
	    ;; We are still loading it when we call this,
	    ;; and it is not in load-history yet.
	    ((equal load "cus-edit"))
	    (t
	     ;; (condition-case nil (load load) (error nil))
	     (condition-case alpha
		 (load load)
	       (error
		(push (list symbol load alpha) cus-test-deps-errors)
		(message "Load Problem: %s %s: %s" symbol load alpha)))
	     (push (list symbol load) cus-test-deps-tested))
	    ))))))
  (message "Cus Test Deps loaded %s files."
	   (length cus-test-deps-tested))
  (if cus-test-deps-errors
      (message "The following load problems appeared:\n%s"
	       cus-test-deps-errors)
    (message "No load problems encountered by Cus Test Deps"))
  (run-hooks 'cus-test-after-load-libs-hook))

(defun cus-test-libs ()
  "Load the libraries with autoloads in loaddefs.el.
Don't load libraries in `cus-test-libs-noloads'.

This function is useful to detect load problems of libraries.
It is suitable for batch mode.  E.g., invoke

  src/emacs -batch -l admin/cus-test.el -f cus-test-libs

in the emacs source directory."
  (interactive)
  (setq cus-test-libs-errors nil)
  (setq cus-test-libs-loaded nil)
  (set-buffer (find-file-noselect (locate-library "loaddefs")))
  (goto-char (point-min))
  (let (file)
    (while
	(search-forward "\n;;; Generated autoloads from " nil t)
      (goto-char (match-end 0))
      (setq file (buffer-substring (point)
				   (progn (end-of-line) (point))))
      ;; If it is, load that library.
      (when file
	(setq file (file-name-nondirectory file))
	(when (string-match "\\.el\\'" file)
	  (setq file (substring file 0 (match-beginning 0)))))
      (condition-case alpha
	  (unless (member file cus-test-libs-noloads)
	    (load-library file)
	    (push file cus-test-libs-loaded))
	(error
	 (push (cons file alpha) cus-test-libs-errors)
	 (message "Error for %s: %s" file alpha)))))
  (message "Cus Test Libs loaded %s files."
	   (length cus-test-libs-loaded))
  (if cus-test-libs-errors
      (message "The following load problems appeared:\n%s"
	       cus-test-libs-errors)
    (message "No load problems encountered by Cus Test Libs"))
  (run-hooks 'cus-test-after-load-libs-hook))

(provide 'cus-test)

;;; cus-test.el ends here
