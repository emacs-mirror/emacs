;;; env.el --- functions to manipulate environment variables

;; Copyright (C) 1991, 1994, 2000, 2001, 2002, 2003, 2004,
;;   2005, 2006, 2007 Free Software Foundation, Inc.

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

(eval-when-compile (require 'cl))

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
			   (append process-environment
				   (frame-parameter (frame-with-environment) 'environment)))
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


(defun setenv-internal (env variable value keep-empty)
  "Set VARIABLE to VALUE in ENV, adding empty entries if KEEP-EMPTY.
Changes ENV by side-effect, and returns its new value."
  (let ((pattern (concat "\\`" (regexp-quote variable) "\\(=\\|\\'\\)"))
	(case-fold-search nil)
	(scan env)
	prev found)
    ;; Handle deletions from the beginning of the list specially.
    (if (and (null value)
	     (not keep-empty)
	     env
	     (stringp (car env))
	     (string-match pattern (car env)))
	(cdr env)
      ;; Try to find existing entry for VARIABLE in ENV.
      (while (and scan (stringp (car scan)))
	(when (string-match pattern (car scan))
	  (if value
	      (setcar scan (concat variable "=" value))
	    (if keep-empty
		(setcar scan variable)
	      (setcdr prev (cdr scan))))
	  (setq found t
		scan nil))
	(setq prev scan
	      scan (cdr scan)))
      (if (and (not found) (or value keep-empty))
	  (cons (if value
		    (concat variable "=" value)
		  variable)
		env)
	env))))

;; Fixme: Should the environment be recoded if LC_CTYPE &c is set?

(defun setenv (variable &optional value substitute-env-vars frame)
  "Set the value of the environment variable named VARIABLE to VALUE.
VARIABLE should be a string.  VALUE is optional; if not provided or
nil, the environment variable VARIABLE will be removed.

Interactively, a prefix argument means to unset the variable, and
otherwise the current value (if any) of the variable appears at
the front of the history list when you type in the new value.
This function always replaces environment variables in the new
value when called interactively.

SUBSTITUTE-ENV-VARS, if non-nil, means to substitute environment
variables in VALUE with `substitute-env-vars', which see.
This is normally used only for interactive calls.

If optional parameter FRAME is non-nil, this function modifies
only the frame-local value of VARIABLE on FRAME, ignoring
`process-environment'.  Note that frames on the same terminal
device usually share their environment, so calling `setenv' on
one of them affects the others as well.

If FRAME is nil, `setenv' changes the global value of VARIABLE by
modifying `process-environment'.  Note that the global value
overrides any frame-local values.

The return value is the new value of VARIABLE, or nil if
it was removed from the environment.

As a special case, setting variable `TZ' calls `set-time-zone-rule' as
a side-effect."
  (interactive
   (if current-prefix-arg
       (list (read-envvar-name "Clear environment variable: " 'exact) nil)
     (let* ((var (read-envvar-name "Set environment variable: " nil))
	    (value (getenv var)))
       (when value
	 (add-to-history 'setenv-history value))
       ;; Here finally we specify the args to give call setenv with.
       (list var
	     (read-from-minibuffer (format "Set %s to value: " var)
				   nil nil nil 'setenv-history
				   value)
	     t))))
  (if (and (multibyte-string-p variable) locale-coding-system)
      (let ((codings (find-coding-systems-string (concat variable value))))
	(unless (or (eq 'undecided (car codings))
		    (memq (coding-system-base locale-coding-system) codings))
	  (error "Can't encode `%s=%s' with `locale-coding-system'"
		 variable (or value "")))))
  (and value
       substitute-env-vars
       (setq value (substitute-env-vars value)))
  (if (multibyte-string-p variable)
      (setq variable (encode-coding-string variable locale-coding-system)))
  (if (and value (multibyte-string-p value))
      (setq value (encode-coding-string value locale-coding-system)))
  (if (string-match "=" variable)
      (error "Environment variable name `%s' contains `='" variable))
  (if (string-equal "TZ" variable)
      (set-time-zone-rule value))
  (if (null frame)
      (setq process-environment (setenv-internal process-environment
						 variable value t))
    (setq frame (frame-with-environment frame))
    (set-frame-parameter frame 'environment
			 (setenv-internal (frame-parameter frame 'environment)
					  variable value nil)))
  value)

(defun getenv (variable &optional frame)
  "Get the value of environment variable VARIABLE.
VARIABLE should be a string.  Value is nil if VARIABLE is undefined in
the environment.  Otherwise, value is a string.

If optional parameter FRAME is non-nil, then it should be a
frame.  This function will look up VARIABLE in its 'environment
parameter.

Otherwise, this function searches `process-environment' for
VARIABLE.  If it is not found there, then it continues the search
in the environment list of the selected frame."
  (interactive (list (read-envvar-name "Get environment variable: " t)))
  (let ((value (getenv-internal (if (multibyte-string-p variable)
				    (encode-coding-string
				     variable locale-coding-system)
				  variable)
				frame)))
    (if (and enable-multibyte-characters value)
	(setq value (decode-coding-string value locale-coding-system)))
    (when (interactive-p)
      (message "%s" (if value value "Not set")))
    value))

(defun environment ()
  "Return a list of environment variables with their values.
Each entry in the list is a string of the form NAME=VALUE.

The returned list can not be used to change environment
variables, only read them.  See `setenv' to do that.

The list is constructed by concatenating the elements of
`process-environment' and the 'environment parameter of the
selected frame, and removing duplicated and empty values.

Non-ASCII characters are encoded according to the initial value of
`locale-coding-system', i.e. the elements must normally be decoded for use.
See `setenv' and `getenv'."
  (let* ((env (append process-environment
		      (frame-parameter (frame-with-environment)
				       'environment)
		      nil))
	 (scan env)
	 prev seen)
    ;; Remove unset variables from the beginning of the list.
    (while (and env
		(or (not (stringp (car env)))
		    (not (string-match "=" (car env)))))
      (or (member (car env) seen)
	  (setq seen (cons (car env) seen)))
      (setq env (cdr env)
	    scan env))
    (let (name)
      (while scan
	(cond ((or (not (stringp (car scan)))
		   (not (string-match "=" (car scan))))
	       ;; Unset variable.
	       (or (member (car scan) seen)
		   (setq seen (cons (car scan) seen)))
	       (setcdr prev (cdr scan)))
	      ((member (setq name (substring (car scan) 0 (string-match "=" (car scan)))) seen)
	       ;; Duplicated variable.
	       (setcdr prev (cdr scan)))
	      (t
	       ;; New variable.
	       (setq seen (cons name seen))))
	(setq prev scan
	      scan (cdr scan))))
    env))

(defmacro let-environment (varlist &rest body)
  "Evaluate BODY with environment variables set according to VARLIST.
The environment variables are then restored to their previous
values.
The value of the last form in BODY is returned.

Each element of VARLIST is either a string (which variable is
then removed from the environment), or a list (NAME
VALUEFORM) (which sets NAME to the value of VALUEFORM, a string).
All the VALUEFORMs are evaluated before any variables are set."
  (declare (indent 2))
  (let ((old-env (make-symbol "old-env"))
	(name (make-symbol "name"))
	(value (make-symbol "value"))
	(entry (make-symbol "entry"))
	(frame (make-symbol "frame")))
    `(let ((,frame (selected-frame))
	    ,old-env)
       ;; Evaluate VALUEFORMs and replace them in VARLIST with their values.
       (dolist (,entry ,varlist)
	 (unless (stringp ,entry)
	   (if (cdr (cdr ,entry))
	       (error "`let-environment' bindings can have only one value-form"))
	   (setcdr ,entry (eval (cadr ,entry)))))
       ;; Set the variables.
       (dolist (,entry ,varlist)
	 (let ((,name (if (stringp ,entry) ,entry (car ,entry)))
	       (,value (if (consp ,entry) (cdr ,entry))))
	   (setq ,old-env (cons (cons ,name (getenv ,name)) ,old-env))
	   (setenv ,name ,value)))
       (unwind-protect
	   (progn ,@body)
	 ;; Restore old values.
	 (with-selected-frame (if (frame-live-p ,frame)
				  ,frame
				(selected-frame))
	   (dolist (,entry ,old-env)
	     (setenv (car ,entry) (cdr ,entry))))))))

(provide 'env)

;;; arch-tag: b7d6a8f7-bc81-46db-8e39-8d721d4ed0b8
;;; env.el ends here
