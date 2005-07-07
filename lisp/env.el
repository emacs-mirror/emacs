;;; env.el --- functions to manipulate environment variables

;; Copyright (C) 1991, 1994, 2000, 2001, 2003 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: processes, unix

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

;; UNIX processes inherit a list of name-to-string associations from their
;; parents called their `environment'; these are commonly used to control
;; program options.  This package permits you to set environment variables
;; to be passed to any sub-process run under Emacs.

;; Note that the environment string `process-environment' is not
;; decoded, but the args of `setenv' and `getenv' are normally
;; multibyte text and get coding conversion.

;;; Code:

;; History list for environment variable names.
(defvar read-envvar-name-history nil)

(defun read-envvar-name (prompt &optional mustmatch)
  "Read environment variable name, prompting with PROMPT.
Optional second arg MUSTMATCH, if non-nil, means require existing envvar name.
If it is also not t, RET does not exit if it does non-null completion."
  (completing-read prompt
		   (mapcar (lambda (enventry)
			     (list (if enable-multibyte-characters
				       (decode-coding-string
					(substring enventry 0
						   (string-match "=" enventry))
					locale-coding-system t)
				     (substring enventry 0
						(string-match "=" enventry)))))
			   process-environment)
		   nil mustmatch nil 'read-envvar-name-history))

;; History list for VALUE argument to setenv.
(defvar setenv-history nil)


(defun substitute-env-vars (string)
  "Substitute environment variables referred to in STRING.
`$FOO' where FOO is an environment variable name means to substitute
the value of that variable.  The variable name should be terminated
with a character not a letter, digit or underscore; otherwise, enclose
the entire variable name in braces.  For instance, in `ab$cd-x',
`$cd' is treated as an environment variable.

Use `$$' to insert a single dollar sign."
  (let ((start 0))
    (while (string-match
	    (eval-when-compile
	      (rx (or (and "$" (submatch (1+ (regexp "[[:alnum:]_]"))))
		      (and "${" (submatch (minimal-match (0+ anything))) "}")
		      "$$")))
	    string start)
      (cond ((match-beginning 1)
	     (let ((value (getenv (match-string 1 string))))
	       (setq string (replace-match (or value "") t t string)
		     start (+ (match-beginning 0) (length value)))))
	    ((match-beginning 2)
	     (let ((value (getenv (match-string 2 string))))
	       (setq string (replace-match (or value "") t t string)
		     start (+ (match-beginning 0) (length value)))))
	    (t
	     (setq string (replace-match "$" t t string)
		   start (+ (match-beginning 0) 1)))))
    string))

;; Fixme: Should `process-environment' be recoded if LC_CTYPE &c is set?

(defun setenv (variable &optional value unset substitute-env-vars)
  "Set the value of the environment variable named VARIABLE to VALUE.
VARIABLE should be a string.  VALUE is optional; if not provided or
nil, the environment variable VARIABLE will be removed.  UNSET
if non-nil means to remove VARIABLE from the environment.
SUBSTITUTE-ENV-VARS, if non-nil, means to substitute environment
variables in VALUE with `substitute-env-vars', where see.
Value is the new value if VARIABLE, or nil if removed from the
environment.

Interactively, a prefix argument means to unset the variable.
Interactively, the current value (if any) of the variable
appears at the front of the history list when you type in the new value.
Interactively, always replace environment variables in the new value.

This function works by modifying `process-environment'.

As a special case, setting variable `TZ' calls `set-time-zone-rule' as
a side-effect."
  (interactive
   (if current-prefix-arg
       (list (read-envvar-name "Clear environment variable: " 'exact) nil t)
     (let* ((var (read-envvar-name "Set environment variable: " nil))
	    (value (getenv var)))
       (when value
	 (push value setenv-history))
       ;; Here finally we specify the args to give call setenv with.
       (list var
	     (read-from-minibuffer (format "Set %s to value: " var)
				   nil nil nil 'setenv-history
				   value)
	     nil
	     t))))
  (if (and (multibyte-string-p variable) locale-coding-system)
      (let ((codings (find-coding-systems-string (concat variable value))))
	(unless (or (eq 'undecided (car codings))
		    (memq (coding-system-base locale-coding-system) codings))
	  (error "Can't encode `%s=%s' with `locale-coding-system'"
		 variable (or value "")))))
  (if unset
      (setq value nil)
    (if substitute-env-vars
	(setq value (substitute-env-vars value))))
  (if (multibyte-string-p variable)
      (setq variable (encode-coding-string variable locale-coding-system)))
  (if (and value (multibyte-string-p value))
      (setq value (encode-coding-string value locale-coding-system)))
  (if (string-match "=" variable)
      (error "Environment variable name `%s' contains `='" variable)
    (let ((pattern (concat "\\`" (regexp-quote (concat variable "="))))
	  (case-fold-search nil)
	  (scan process-environment)
	  found)
      (if (string-equal "TZ" variable)
	  (set-time-zone-rule value))
      (while scan
	(cond ((string-match pattern (car scan))
	       (setq found t)
	       (if (eq nil value)
		   (setq process-environment (delq (car scan)
						   process-environment))
		 (setcar scan (concat variable "=" value)))
	       (setq scan nil)))
	(setq scan (cdr scan)))
      (or found
	  (if value
	      (setq process-environment
		    (cons (concat variable "=" value)
			  process-environment))))))
  value)

(defun getenv (variable)
  "Get the value of environment variable VARIABLE.
VARIABLE should be a string.  Value is nil if VARIABLE is undefined in
the environment.  Otherwise, value is a string.

This function consults the variable `process-environment'
for its value."
  (interactive (list (read-envvar-name "Get environment variable: " t)))
  (let ((value (getenv-internal (if (multibyte-string-p variable)
				    (encode-coding-string
				     variable locale-coding-system)
				  variable))))
    (if (and enable-multibyte-characters value)
	(setq value (decode-coding-string value locale-coding-system)))
    (when (interactive-p)
      (message "%s" (if value value "Not set")))
    value))

(provide 'env)

;;; arch-tag: b7d6a8f7-bc81-46db-8e39-8d721d4ed0b8
;;; env.el ends here
