;;; byte-run.el --- byte-compiler support for inlining

;; Copyright (C) 1992 Free Software Foundation, Inc.

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
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; interface to selectively inlining functions.
;; This only happens when source-code optimization is turned on.

;;; Code:

;; Redefined in byte-optimize.el.
;; This is not documented--it's not clear that we should promote it.
(fset 'inline 'progn)
(put 'inline 'lisp-indent-hook 0)


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
  (or (memq (get name 'byte-optimizer)
	    '(nil byte-compile-inline-expand))
      (error "`%s' is a primitive" name))
  (list 'prog1
	(cons 'defun (cons name (cons arglist body)))
	(list 'eval-and-compile
	      (list 'put (list 'quote name)
		    ''byte-optimizer ''byte-compile-inline-expand))))

(defun make-obsolete (fn new &optional when)
  "Make the byte-compiler warn that FUNCTION is obsolete.
The warning will say that NEW should be used instead.
If NEW is a string, that is the `use instead' message.
If provided, WHEN should be a string indicating when the function
was first made obsolete, for example a date or a release number."
  (interactive "aMake function obsolete: \nxObsoletion replacement: ")
  (let ((handler (get fn 'byte-compile)))
    (if (eq 'byte-compile-obsolete handler)
	(setq handler (nth 1 (get fn 'byte-obsolete-info)))
      (put fn 'byte-compile 'byte-compile-obsolete))
    (put fn 'byte-obsolete-info (list new handler when)))
  fn)

(defun make-obsolete-variable (var new &optional when)
  "Make the byte-compiler warn that VARIABLE is obsolete,
and NEW should be used instead.  If NEW is a string, then that is the
`use instead' message.
If provided, WHEN should be a string indicating when the variable
was first made obsolete, for example a date or a release number."
  (interactive
   (list
    (let ((str (completing-read "Make variable obsolete: " obarray 'boundp t)))
      (if (equal str "") (error ""))
      (intern str))
    (car (read-from-string (read-string "Obsoletion replacement: ")))))
  (put var 'byte-obsolete-variable (cons new when))
  var)

(put 'dont-compile 'lisp-indent-hook 0)
(defmacro dont-compile (&rest body)
  "Like `progn', but the body always runs interpreted (not compiled).
If you think you need this, you're probably making a mistake somewhere."
  (list 'eval (list 'quote (if (cdr body) (cons 'progn body) (car body)))))


;;; interface to evaluating things at compile time and/or load time
;;; these macro must come after any uses of them in this file, as their
;;; definition in the file overrides the magic definitions on the
;;; byte-compile-macro-environment.

(put 'eval-when-compile 'lisp-indent-hook 0)
(defmacro eval-when-compile (&rest body)
  "Like `progn', but evaluates the body at compile time.
The result of the body appears to the compiler as a quoted constant."
  ;; Not necessary because we have it in b-c-initial-macro-environment
  ;; (list 'quote (eval (cons 'progn body)))
  (cons 'progn body))

(put 'eval-and-compile 'lisp-indent-hook 0)
(defmacro eval-and-compile (&rest body)
  "Like `progn', but evaluates the body at compile time and at load time."
  ;; Remember, it's magic.
  (cons 'progn body))

(defun with-no-warnings (&optional first &rest body)
  "Like `progn', but prevents compiler warnings in the body."
  ;; The implementation for the interpreter is basically trivial.
  (if body (car (last body))
    first))


;;; I nuked this because it's not a good idea for users to think of using it.
;;; These options are a matter of installation preference, and have nothing to
;;; with particular source files; it's a mistake to suggest to users
;;; they should associate these with particular source files.
;;; There is hardly any reason to change these parameters, anyway.
;;; --rms.

;; (put 'byte-compiler-options 'lisp-indent-hook 0)
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

;;; arch-tag: 76f8328a-1f66-4df2-9b6d-5c3666dc05e9
;;; byte-run.el ends here
