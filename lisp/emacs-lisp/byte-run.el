;;; byte-run.el --- byte-compiler support for inlining

;; Copyright (C) 1992, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007 Free Software Foundation, Inc.

;; Author: Jamie Zawinski <jwz@lucid.com>
;;	Hallvard Furuseth <hbf@ulrik.uio.no>
;; Maintainer: FSF
;; Keywords: internal

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

;; interface to selectively inlining functions.
;; This only happens when source-code optimization is turned on.

;;; Code:

;; We define macro-declaration-function here because it is needed to
;; handle declarations in macro definitions and this is the first file
;; loaded by loadup.el that uses declarations in macros.

(defun macro-declaration-function (macro decl)
  "Process a declaration found in a macro definition.
This is set as the value of the variable `macro-declaration-function'.
MACRO is the name of the macro being defined.
DECL is a list `(declare ...)' containing the declarations.
The return value of this function is not used."
  ;; We can't use `dolist' or `cadr' yet for bootstrapping reasons.
  (let (d)
    ;; Ignore the first element of `decl' (it's always `declare').
    (while (setq decl (cdr decl))
      (setq d (car decl))
      (cond ((and (consp d) (eq (car d) 'indent))
	     (put macro 'lisp-indent-function (car (cdr d))))
	    ((and (consp d) (eq (car d) 'debug))
	     (put macro 'edebug-form-spec (car (cdr d))))
	    ((and (consp d) (eq (car d) 'doc-string))
	     (put macro 'doc-string-elt (car (cdr d))))
	    (t
	     (message "Unknown declaration %s" d))))))

(setq macro-declaration-function 'macro-declaration-function)


;; Redefined in byte-optimize.el.
;; This is not documented--it's not clear that we should promote it.
(fset 'inline 'progn)
(put 'inline 'lisp-indent-function 0)

;;; Interface to inline functions.

;; (defmacro proclaim-inline (&rest fns)
;;   "Cause the named functions to be open-coded when called from compiled code.
;; They will only be compiled open-coded when byte-compile-optimize is true."
;;   (cons 'eval-and-compile
;; 	(mapcar '(lambda (x)
;; 		   (or (memq (get x 'byte-optimizer)
;; 			     '(nil byte-compile-inline-expand))
;; 		       (error
;; 			"%s already has a byte-optimizer, can't make it inline"
;; 			x))
;; 		   (list 'put (list 'quote x)
;; 			 ''byte-optimizer ''byte-compile-inline-expand))
;; 		fns)))

;; (defmacro proclaim-notinline (&rest fns)
;;   "Cause the named functions to no longer be open-coded."
;;   (cons 'eval-and-compile
;; 	(mapcar '(lambda (x)
;; 		   (if (eq (get x 'byte-optimizer) 'byte-compile-inline-expand)
;; 		       (put x 'byte-optimizer nil))
;; 		   (list 'if (list 'eq (list 'get (list 'quote x) ''byte-optimizer)
;; 				   ''byte-compile-inline-expand)
;; 			 (list 'put x ''byte-optimizer nil)))
;; 		fns)))

;; This has a special byte-hunk-handler in bytecomp.el.
(defmacro defsubst (name arglist &rest body)
  "Define an inline function.  The syntax is just like that of `defun'."
  (declare (debug defun))
  (or (memq (get name 'byte-optimizer)
	    '(nil byte-compile-inline-expand))
      (error "`%s' is a primitive" name))
  `(prog1
       (defun ,name ,arglist ,@body)
     (eval-and-compile
       (put ',name 'byte-optimizer 'byte-compile-inline-expand))))

(defun make-obsolete (obsolete-name current-name &optional when)
  "Make the byte-compiler warn that OBSOLETE-NAME is obsolete.
The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message.
If provided, WHEN should be a string indicating when the function
was first made obsolete, for example a date or a release number."
  (interactive "aMake function obsolete: \nxObsoletion replacement: ")
  (let ((handler (get obsolete-name 'byte-compile)))
    (if (eq 'byte-compile-obsolete handler)
	(setq handler (nth 1 (get obsolete-name 'byte-obsolete-info)))
      (put obsolete-name 'byte-compile 'byte-compile-obsolete))
    (put obsolete-name 'byte-obsolete-info (list current-name handler when)))
  obsolete-name)

(defmacro define-obsolete-function-alias (obsolete-name current-name
						   &optional when docstring)
  "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.

\(define-obsolete-function-alias 'old-fun 'new-fun \"22.1\" \"old-fun's doc.\")

is equivalent to the following two lines of code:

\(defalias 'old-fun 'new-fun \"old-fun's doc.\")
\(make-obsolete 'old-fun 'new-fun \"22.1\")

See the docstrings of `defalias' and `make-obsolete' for more details."
  (declare (doc-string 4))
  `(progn
     (defalias ,obsolete-name ,current-name ,docstring)
     (make-obsolete ,obsolete-name ,current-name ,when)))

(defun make-obsolete-variable (obsolete-name current-name &optional when)
  "Make the byte-compiler warn that OBSOLETE-NAME is obsolete.
The warning will say that CURRENT-NAME should be used instead.
If CURRENT-NAME is a string, that is the `use instead' message.
If provided, WHEN should be a string indicating when the variable
was first made obsolete, for example a date or a release number."
  (interactive
   (list
    (let ((str (completing-read "Make variable obsolete: " obarray 'boundp t)))
      (if (equal str "") (error ""))
      (intern str))
    (car (read-from-string (read-string "Obsoletion replacement: ")))))
  (put obsolete-name 'byte-obsolete-variable (cons current-name when))
  obsolete-name)

(defmacro define-obsolete-variable-alias (obsolete-name current-name
						 &optional when docstring)
  "Make OBSOLETE-NAME a variable alias for CURRENT-NAME and mark it obsolete.

\(define-obsolete-variable-alias 'old-var 'new-var \"22.1\" \"old-var's doc.\")

is equivalent to the following two lines of code:

\(defvaralias 'old-var 'new-var \"old-var's doc.\")
\(make-obsolete-variable 'old-var 'new-var \"22.1\")

See the docstrings of `defvaralias' and `make-obsolete-variable' or
Info node `(elisp)Variable Aliases' for more details."
  (declare (doc-string 4))
  `(progn
     (defvaralias ,obsolete-name ,current-name ,docstring)
     (make-obsolete-variable ,obsolete-name ,current-name ,when)))

(defmacro dont-compile (&rest body)
  "Like `progn', but the body always runs interpreted (not compiled).
If you think you need this, you're probably making a mistake somewhere."
  (declare (debug t) (indent 0))
  (list 'eval (list 'quote (if (cdr body) (cons 'progn body) (car body)))))


;; interface to evaluating things at compile time and/or load time
;; these macro must come after any uses of them in this file, as their
;; definition in the file overrides the magic definitions on the
;; byte-compile-macro-environment.

(defmacro eval-when-compile (&rest body)
  "Like `progn', but evaluates the body at compile time if you're compiling.
Thus, the result of the body appears to the compiler as a quoted constant.
In interpreted code, this is entirely equivalent to `progn'."
  (declare (debug t) (indent 0))
  ;; Not necessary because we have it in b-c-initial-macro-environment
  ;; (list 'quote (eval (cons 'progn body)))
  (cons 'progn body))

(defmacro eval-and-compile (&rest body)
  "Like `progn', but evaluates the body at compile time and at load time."
  (declare (debug t) (indent 0))
  ;; Remember, it's magic.
  (cons 'progn body))

(put 'with-no-warnings 'lisp-indent-function 0)
(defun with-no-warnings (&rest body)
  "Like `progn', but prevents compiler warnings in the body."
  ;; The implementation for the interpreter is basically trivial.
  (car (last body)))


;; I nuked this because it's not a good idea for users to think of using it.
;; These options are a matter of installation preference, and have nothing to
;; with particular source files; it's a mistake to suggest to users
;; they should associate these with particular source files.
;; There is hardly any reason to change these parameters, anyway.
;; --rms.

;; (put 'byte-compiler-options 'lisp-indent-function 0)
;; (defmacro byte-compiler-options (&rest args)
;;   "Set some compilation-parameters for this file.  This will affect only the
;; file in which it appears; this does nothing when evaluated, and when loaded
;; from a .el file.
;;
;; Each argument to this macro must be a list of a key and a value.
;;
;;   Keys:		  Values:		Corresponding variable:
;;
;;   verbose	  t, nil		byte-compile-verbose
;;   optimize	  t, nil, source, byte	byte-compile-optimize
;;   warnings	  list of warnings	byte-compile-warnings
;; 		      Legal elements: (callargs redefine free-vars unresolved)
;;   file-format	  emacs18, emacs19	byte-compile-compatibility
;;
;; For example, this might appear at the top of a source file:
;;
;;     (byte-compiler-options
;;       (optimize t)
;;       (warnings (- free-vars))		; Don't warn about free variables
;;       (file-format emacs19))"
;;   nil)

;; arch-tag: 76f8328a-1f66-4df2-9b6d-5c3666dc05e9
;;; byte-run.el ends here
