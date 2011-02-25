;;; eieio-comp.el -- eieio routines to help with byte compilation

;; Copyright (C) 1995-1996, 1998-2002, 2005, 2008-2011
;;   Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Version: 0.2
;; Keywords: lisp, tools
;; Package: eieio

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Byte compiler functions for defmethod.  This will affect the new GNU
;; byte compiler for Emacs 19 and better.  This function will be called by
;; the byte compiler whenever a `defmethod' is encountered in a file.
;; It will output a function call to `eieio-defmethod' with the byte
;; compiled function as a parameter.

;;; Code:

(declare-function eieio-defgeneric-form "eieio" (method doc-string))

;; Some compatibility stuff
(eval-and-compile
  (if (not (fboundp 'byte-compile-compiled-obj-to-list))
      (defun byte-compile-compiled-obj-to-list (moose) nil))

  (if (not (boundp 'byte-compile-outbuffer))
      (defvar byte-compile-outbuffer nil))
  )

;; This teaches the byte compiler how to do this sort of thing.
(put 'defmethod 'byte-hunk-handler 'byte-compile-file-form-defmethod)

(defun byte-compile-file-form-defmethod (form)
  "Mumble about the method we are compiling.
This function is mostly ripped from `byte-compile-file-form-defun',
but it's been modified to handle the special syntax of the `defmethod'
command.  There should probably be one for `defgeneric' as well, but
that is called but rarely.  Argument FORM is the body of the method."
  (setq form (cdr form))
  (let* ((meth (car form))
	 (key (progn (setq form (cdr form))
		     (cond ((or (eq ':BEFORE (car form))
				(eq ':before (car form)))
			    (setq form (cdr form))
			    ":before ")
			   ((or (eq ':AFTER (car form))
				(eq ':after (car form)))
			    (setq form (cdr form))
			    ":after ")
			   ((or (eq ':PRIMARY (car form))
				(eq ':primary (car form)))
			    (setq form (cdr form))
			    ":primary ")
			   ((or (eq ':STATIC (car form))
				(eq ':static (car form)))
			    (setq form (cdr form))
			    ":static ")
			   (t ""))))
	 (params (car form))
	 (lamparams (byte-compile-defmethod-param-convert params))
	 (arg1 (car params))
	 (class (if (listp arg1) (nth 1 arg1) nil))
	 (my-outbuffer (if (eval-when-compile (featurep 'xemacs))
			   byte-compile-outbuffer
			 (cond ((boundp 'bytecomp-outbuffer)
				bytecomp-outbuffer) ; Emacs >= 23.2
			       ((boundp 'outbuffer) outbuffer)
			       (t (error "Unable to set outbuffer"))))))
    (let ((name (format "%s::%s" (or class "#<generic>") meth)))
      (if byte-compile-verbose
	  ;; #### filename used free
	  (message "Compiling %s... (%s)"
		   (cond ((boundp 'bytecomp-filename) bytecomp-filename)
			 ((boundp 'filename) filename)
			 (t ""))
		   name))
      (setq byte-compile-current-form name) ; for warnings
      )
    ;; Flush any pending output
    (byte-compile-flush-pending)
    ;; Byte compile the body.  For the byte compiled forms, add the
    ;; rest arguments, which will get ignored by the engine which will
    ;; add them later (I hope)
    (let* ((new-one (byte-compile-lambda
		     (append (list 'lambda lamparams)
			     (cdr form))))
	   (code (byte-compile-byte-code-maker new-one)))
      (princ "\n(eieio-defmethod '" my-outbuffer)
      (princ meth my-outbuffer)
      (princ " '(" my-outbuffer)
      (princ key my-outbuffer)
      (prin1 params my-outbuffer)
      (princ " " my-outbuffer)
      (prin1 code my-outbuffer)
      (princ "))" my-outbuffer)
      )
    ;; Now add this function to the list of known functions.
    ;; Don't bother with a doc string.   Not relevant here.
    (add-to-list 'byte-compile-function-environment
		 (cons meth
		       (eieio-defgeneric-form meth "")))

    ;; Remove it from the undefined list if it is there.
    (let ((elt (assq meth byte-compile-unresolved-functions)))
      (if elt (setq byte-compile-unresolved-functions
		    (delq elt byte-compile-unresolved-functions))))

    ;; nil prevents cruft from appearing in the output buffer.
    nil))

(defun byte-compile-defmethod-param-convert (paramlist)
  "Convert method params into the params used by the `defmethod' thingy.
Argument PARAMLIST is the parameter list to convert."
  (let ((argfix nil))
    (while paramlist
      (setq argfix (cons (if (listp (car paramlist))
			     (car (car paramlist))
			   (car paramlist))
			 argfix))
      (setq paramlist (cdr paramlist)))
    (nreverse argfix)))

(provide 'eieio-comp)

;;; eieio-comp.el ends here
