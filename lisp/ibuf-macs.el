;;; ibuf-macs.el --- macros for ibuffer

;; Copyright (C) 2000, 2001, 2002 Free Software Foundation, Inc.

;; Author: Colin Walters <walters@verbum.org>
;; Created: 6 Dec 2001
;; Keywords: buffer, convenience

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(eval-when-compile
  (require 'cl))

;; From Paul Graham's "ANSI Common Lisp", adapted for Emacs Lisp here.
(defmacro ibuffer-aif (test true-body &rest false-body)
  "Evaluate TRUE-BODY or FALSE-BODY depending on value of TEST.
If TEST returns non-nil, bind `it' to the value, and evaluate
TRUE-BODY.  Otherwise, evaluate forms in FALSE-BODY as if in `progn'.
Compare with `if'."
  (let ((sym (gensym "--ibuffer-aif-")))
    `(let ((,sym ,test))
       (if ,sym
	   (let ((it ,sym))
	     ,true-body)
	 (progn
	   ,@false-body)))))
;; (put 'ibuffer-aif 'lisp-indent-function 2)

(defmacro ibuffer-awhen (test &rest body)
  "Evaluate BODY if TEST returns non-nil.
During evaluation of body, bind `it' to the value returned by TEST."
  `(ibuffer-aif ,test
       (progn ,@body)
     nil))
;; (put 'ibuffer-awhen 'lisp-indent-function 1)

(defmacro ibuffer-save-marks (&rest body)
  "Save the marked status of the buffers and execute BODY; restore marks."
  (let ((bufsym (gensym)))
    `(let ((,bufsym (current-buffer))
	   (ibuffer-save-marks-tmp-mark-list (ibuffer-current-state-list)))
       (unwind-protect
	   (progn
	     (save-excursion
	       ,@body))
	 (with-current-buffer ,bufsym
	   (ibuffer-insert-buffers-and-marks
	    ;; Get rid of dead buffers
	    (delq nil
		  (mapcar #'(lambda (e) (when (buffer-live-p (car e))
					  e))
			  ibuffer-save-marks-tmp-mark-list)))
	   (ibuffer-redisplay t))))))
;; (put 'ibuffer-save-marks 'lisp-indent-function 0)

;;;###autoload
(defmacro* define-ibuffer-column (symbol (&key name inline props
					       summarizer) &rest body)
  "Define a column SYMBOL for use with `ibuffer-formats'.

BODY will be called with `buffer' bound to the buffer object, and
`mark' bound to the current mark on the buffer.  The current buffer
will be `buffer'.

If NAME is given, it will be used as a title for the column.
Otherwise, the title will default to a capitalized version of the
SYMBOL's name.  PROPS is a plist of additional properties to add to
the text, such as `mouse-face'.  And SUMMARIZER, if given, is a
function which will be passed a list of all the strings in its column;
it should return a string to display at the bottom.

Note that this macro expands into a `defun' for a function named
ibuffer-make-column-NAME.  If INLINE is non-nil, then the form will be
inlined into the compiled format versions.  This means that if you
change its definition, you should explicitly call
`ibuffer-recompile-formats'."
  (let* ((sym (intern (concat "ibuffer-make-column-"
			      (symbol-name symbol))))
	 (bod-1 `(with-current-buffer buffer
		   ,@body))
	 (bod (if props
		 `(propertize
		   ,bod-1
		   ,@props)
		bod-1)))
    `(progn
       ,(if inline
	    `(push '(,sym ,bod) ibuffer-inline-columns)
	  `(defun ,sym (buffer mark)
	     ,bod))
       (put (quote ,sym) 'ibuffer-column-name
	    ,(if (stringp name)
		 name
	       (capitalize (symbol-name symbol))))
       ,(if summarizer
	    ;; Store the name of the summarizing function.
	    `(put (quote ,sym) 'ibuffer-column-summarizer
		  (quote ,summarizer)))
       ,(if summarizer
	    ;; This will store the actual values of the column
	    ;; summary.
	    `(put (quote ,sym) 'ibuffer-column-summary nil))
       :autoload-end)))
;; (put 'define-ibuffer-column 'lisp-indent-function 'defun)

;;;###autoload
(defmacro* define-ibuffer-sorter (name documentation
				       (&key 
					description)
				       &rest body)
  "Define a method of sorting named NAME.
DOCUMENTATION is the documentation of the function, which will be called
`ibuffer-do-sort-by-NAME'.
DESCRIPTION is a short string describing the sorting method.

For sorting, the forms in BODY will be evaluated with `a' bound to one
buffer object, and `b' bound to another.  BODY should return a non-nil
value if and only if `a' is \"less than\" `b'."
  `(progn
     (defun ,(intern (concat "ibuffer-do-sort-by-" (symbol-name name))) ()
       ,(or documentation "No :documentation specified for this sorting method.")
       (interactive)
       (setq ibuffer-sorting-mode ',name)
       (ibuffer-redisplay t))
     (push (list ',name ,description
		 #'(lambda (a b)
		     ,@body))
	   ibuffer-sorting-functions-alist)
     :autoload-end))
;; (put 'define-ibuffer-sorter 'lisp-indent-function 1)

;;;###autoload
(defmacro* define-ibuffer-op (op args
				 documentation
				 (&key 
				  interactive
				  mark
				  modifier-p
				  dangerous
				  (opstring "operated on")
				  (active-opstring "Operate on")
				  complex)
				 &rest body)
  "Generate a function named `ibuffer-do-OP', which operates on a buffer.
When an operation is performed, this function will be called once for
each marked buffer, with that buffer current.

ARGS becomes the formal parameters of the function.
DOCUMENTATION becomes the docstring of the function.
INTERACTIVE becomes the interactive specification of the function.
MARK describes which type of mark (:deletion, or nil) this operation
uses.  :deletion means the function operates on buffers marked for
deletion, otherwise it acts on normally marked buffers.
MODIFIER-P describes how the function modifies buffers.  This is used
to set the modification flag of the Ibuffer buffer itself.  Valid
values are:
 nil - the function never modifiers buffers
 t - the function it always modifies buffers
 :maybe - attempt to discover this information by comparing the
  buffer's modification flag.
DANGEROUS is a boolean which should be set if the user should be
prompted before performing this operation.
OPSTRING is a string which will be displayed to the user after the
operation is complete, in the form:
 \"Operation complete; OPSTRING x buffers\"
ACTIVE-OPSTRING is a string which will be displayed to the user in a
confirmation message, in the form:
 \"Really ACTIVE-OPSTRING x buffers?\"
COMPLEX means this function is special; see the source code of this
macro for exactly what it does."
  `(progn
    (defun ,(intern (concat "ibuffer-do-" (symbol-name op))) ,args
     ,(if (stringp documentation)
	  documentation
	(format "%s marked buffers." active-opstring))
     ,(if (not (null interactive))
	  `(interactive ,interactive)
	'(interactive))
     (assert (eq major-mode 'ibuffer-mode))
     (setq ibuffer-did-modification nil)
     (let ((marked-names  (,(case mark
			      (:deletion
			       'ibuffer-deletion-marked-buffer-names)
			      (t
			       'ibuffer-marked-buffer-names)))))
       (when (null marked-names)
	 (setq marked-names (list (buffer-name (ibuffer-current-buffer))))
	 (ibuffer-set-mark ,(case mark
			      (:deletion
			       'ibuffer-deletion-char)
			      (t
			       'ibuffer-marked-char))))
       ,(let* ((finish (append
			'(progn)
			(if (eq modifier-p t)
			    '((setq ibuffer-did-modification t))
			  ())
			`((ibuffer-redisplay t)
			  (message ,(concat "Operation finished; " opstring " %s buffers") count))))
	       (inner-body (if complex
			       `(progn ,@body)
			     `(progn
				(with-current-buffer buf
				  (save-excursion
				    ,@body))
				t)))
	       (body `(let ((count
			     (,(case mark
				 (:deletion
				  'ibuffer-map-deletion-lines)
				 (t
				  'ibuffer-map-marked-lines))
			      #'(lambda (buf mark)
				  ,(if (eq modifier-p :maybe)
				       `(let ((ibuffer-tmp-previous-buffer-modification
					       (buffer-modified-p buf)))
					  (prog1 ,inner-body
					    (when (not (eq ibuffer-tmp-previous-buffer-modification
							   (buffer-modified-p buf)))
					      (setq ibuffer-did-modification t))))
				     inner-body)))))
			,finish)))
	  (if dangerous
	      `(when (ibuffer-confirm-operation-on ,active-opstring marked-names)
		 ,body)
	    body))))
    :autoload-end))
;; (put 'define-ibuffer-op 'lisp-indent-function 2)

;;;###autoload
(defmacro* define-ibuffer-filter (name documentation
				       (&key 
					reader
					description)
				       &rest body)
  "Define a filter named NAME.
DOCUMENTATION is the documentation of the function.
READER is a form which should read a qualifier from the user.
DESCRIPTION is a short string describing the filter.

BODY should contain forms which will be evaluated to test whether or
not a particular buffer should be displayed or not.  The forms in BODY
will be evaluated with BUF bound to the buffer object, and QUALIFIER
bound to the current value of the filter."
  (let ((fn-name (intern (concat "ibuffer-filter-by-" (symbol-name name)))))
    `(progn 
       (defun ,fn-name (qualifier)
	 ,(concat (or documentation "This filter is not documented."))
	 (interactive (list ,reader))
	 (ibuffer-push-filter (cons ',name qualifier))
	 (message
	  (format ,(concat (format "Filter by %s added: " description)
			   " %s")
		  qualifier))
	 (ibuffer-update nil t))
       (push (list ',name ,description
		   #'(lambda (buf qualifier)
		       ,@body))
	     ibuffer-filtering-alist)
       :autoload-end)))
;; (put 'define-ibuffer-filter 'lisp-indent-function 2)

(provide 'ibuf-macs)

;;; ibuf-macs.el ends here
