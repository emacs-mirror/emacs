;;; cust-print.el --- handles print-level and print-circle.

;; Copyright (C) 1992 Free Software Foundation, Inc.

;; Author: Daniel LaLiberte <liberte@cs.uiuc.edu>
;; Version: 1.0
;; Adapted-By: ESR
;; Keywords: extensions

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This package provides a general print handler for prin1 and princ
;; that supports print-level and print-circle, and by the way,
;; print-length since the standard routines are being replaced.  Also,
;; to print custom types constructed from lists and vectors, use
;; custom-print-list and custom-print-vector.  See the documentation
;; strings of these variables for more details.  

;; If the results of your expressions contain circular references to
;; other parts of the same structure, the standard Emacs print
;; subroutines may fail to print with an untrappable error,
;; "Apparently circular structure being printed".  If you only use cdr
;; circular lists (where cdrs of lists point back; what is the right
;; term here?), you can limit the length of printing with
;; print-length.  But car circular lists and circular vectors generate
;; the above mentioned untrappable error in Emacs version 18.  Version
;; 19 will support print-level, but it is often useful to get a better
;; print representation of circular structures; the print-circle
;; option may be used to print more concise representations.

;; There are two main ways to use this package.  First, you may
;; replace prin1, princ, and some subroutines that use them by calling
;; install-custom-print-funcs so that any use of these functions in
;; lisp code will be affected.  Second, you could call the custom
;; routines directly, thus only affecting the printing that requires
;; them.

;; Note that subroutines which call print subroutines directly will not
;; use the custom print functions.  In particular, the evaluation
;; functions like eval-region call the print subroutines directly.
;; Therefore, evaluating (aref circ-list 0), which calls error
;; directly (because circ-list is not an array), will jump to the top
;; level instead of printing the circular list.

;; Obviously the right way to implement this custom-print facility
;; is in C.  Please volunteer since I don't have the time or need.

;; Implementation design: we want to use the same list and vector
;; processing algorithm for all versions of prin1 and princ, since how
;; the processing is done depends on print-length, print-level, and
;; print-circle.  For circle printing, a preprocessing step is
;; required before the final printing.  Thanks to Jamie Zawinski
;; for motivation and algorithms.

;;=========================================================
;; export list:

;; print-level
;; print-circle

;; custom-print-list
;; custom-print-vector
;; add-custom-print-list
;; add-custom-print-vector

;; install-custom-print-funcs
;; uninstall-custom-print-funcs

;; custom-prin1
;; custom-princ
;; custom-prin1-to-string
;; custom-print
;; custom-format
;; custom-message
;; custom-error

;;; Code:

(provide 'custom-print)

;;(defvar print-length nil
;;  "*Controls how many elements of a list, at each level, are printed.
;;This is defined by emacs.")

(defvar print-level nil
  "*Controls how many levels deep a nested data object will print.  

If nil, printing proceeds recursively and may lead to
max-lisp-eval-depth being exceeded or an untrappable error may occur:
`Apparently circular structure being printed.'
Also see `print-length' and `print-circle'.

If non-nil, components at levels equal to or greater than `print-level'
are printed simply as `#'.  The object to be printed is at level 0,
and if the object is a list or vector, its top-level components are at
level 1.")


(defvar print-circle nil
  "*Controls the printing of recursive structures.  

If nil, printing proceeds recursively and may lead to
`max-lisp-eval-depth' being exceeded or an untrappable error may occur:
\"Apparently circular structure being printed.\"  Also see
`print-length' and `print-level'.

If non-nil, shared substructures anywhere in the structure are printed
with `#N=' before the first occurrence (in the order of the print
representation) and `#N#' in place of each subsequent occurrence,
where N is a positive decimal integer.

Currently, there is no way to read this representation in Emacs.")


(defconst custom-print-list
  nil
  ;; e.g.  '((floatp . float-to-string))
  "An alist for custom printing of lists.  
Pairs are of the form (PRED . CONVERTER).  If PREDICATE is true
for an object, then CONVERTER is called with the object and should
return a string to be printed with `princ'.  
Also see `custom-print-vector'.")

(defconst custom-print-vector
  nil
  "An alist for custom printing of vectors.  
Pairs are of the form (PRED . CONVERTER).  If PREDICATE is true
for an object, then CONVERTER is called with the object and should
return a string to be printed with `princ'.  
Also see `custom-print-list'.")


(defun add-custom-print-list (pred converter)
  "Add a pair of PREDICATE and CONVERTER to `custom-print-list'.
Any pair that has the same PREDICATE is first removed."
  (setq custom-print-list (cons (cons pred converter) 
				(delq (assq pred custom-print-list)
				      custom-print-list))))
;; e.g. (add-custom-print-list 'floatp 'float-to-string)


(defun add-custom-print-vector (pred converter)
  "Add a pair of PREDICATE and CONVERTER to `custom-print-vector'.
Any pair that has the same PREDICATE is first removed."
  (setq custom-print-vector (cons (cons pred converter) 
				  (delq (assq pred custom-print-vector)
					custom-print-vector))))


;;====================================================
;; Saving and restoring internal printing routines.

(defun cust-print-set-function-cell (symbol-pair)
  (defalias (car symbol-pair) 
	(symbol-function (car (cdr symbol-pair)))))


(if (not (fboundp 'cust-print-internal-prin1))
    (mapcar 'cust-print-set-function-cell
	    '((cust-print-internal-prin1 prin1)
	      (cust-print-internal-princ princ)
	      (cust-print-internal-print print)
	      (cust-print-internal-prin1-to-string prin1-to-string)
	      (cust-print-internal-format format)
	      (cust-print-internal-message message)
	      (cust-print-internal-error error))))


(defun install-custom-print-funcs ()
  "Replace print functions with general, customizable, Lisp versions.
The internal subroutines are saved away, and you can reinstall them
by running `uninstall-custom-print-funcs'."
  (interactive)
  (mapcar 'cust-print-set-function-cell
	  '((prin1 custom-prin1)
	    (princ custom-princ)
	    (print custom-print)
	    (prin1-to-string custom-prin1-to-string)
	    (format custom-format)
	    (message custom-message)
	    (error custom-error)
	    )))
  
(defun uninstall-custom-print-funcs ()
  "Reset print functions to their internal subroutines."
  (interactive)
  (mapcar 'cust-print-set-function-cell
	  '((prin1 cust-print-internal-prin1)
	    (princ cust-print-internal-princ)
	    (print cust-print-internal-print)
	    (prin1-to-string cust-print-internal-prin1-to-string)
	    (format cust-print-internal-format)
	    (message cust-print-internal-message)
	    (error cust-print-internal-error)
	    )))


;;===============================================================
;; Lisp replacements for prin1 and princ and for subrs that use prin1 
;; (or princ) -- so far only the printing and formatting subrs.

(defun custom-prin1 (object &optional stream)
  "Replacement for standard `prin1'.
Uses the appropriate printer depending on the values of `print-level'
and `print-circle' (which see).

Output the printed representation of OBJECT, any Lisp object.
Quoting characters are printed when needed to make output that `read'
can handle, whenever this is possible.
Output stream is STREAM, or value of `standard-output' (which see)."
  (cust-print-top-level object stream 'cust-print-internal-prin1))


(defun custom-princ (object &optional stream)
  "Same as `custom-prin1' except no quoting."
  (cust-print-top-level object stream 'cust-print-internal-princ))

(defvar custom-prin1-chars)

(defun custom-prin1-to-string-func (c)
  "Stream function for `custom-prin1-to-string'."
  (setq custom-prin1-chars (cons c custom-prin1-chars)))

(defun custom-prin1-to-string (object)
  "Replacement for standard `prin1-to-string'."
  (let ((custom-prin1-chars nil))
    (custom-prin1 object 'custom-prin1-to-string-func)
    (concat (nreverse custom-prin1-chars))))


(defun custom-print (object &optional stream)
  "Replacement for standard `print'."
  (cust-print-internal-princ "\n")
  (custom-prin1 object stream)
  (cust-print-internal-princ "\n"))


(defun custom-format (fmt &rest args)
  "Replacement for standard `format'.

Calls format after first making strings for list or vector args.
The format specification for such args should be `%s' in any case, so a
string argument will also work.  The string is generated with
`custom-prin1-to-string', which quotes quotable characters."
  (apply 'cust-print-internal-format fmt
	 (mapcar (function (lambda (arg)
			     (if (or (listp arg) (vectorp arg))
				 (custom-prin1-to-string arg)
			       arg)))
		 args)))
	    
  

(defun custom-message (fmt &rest args)
  "Replacement for standard `message' that works like `custom-format'."
  ;; It doesn't work to princ the result of custom-format
  ;; because the echo area requires special handling
  ;; to avoid duplicating the output.  cust-print-internal-message does it right.
  ;; (cust-print-internal-princ (apply 'custom-format fmt args))
  (apply 'cust-print-internal-message  fmt
	 (mapcar (function (lambda (arg)
			     (if (or (listp arg) (vectorp arg))
				 (custom-prin1-to-string arg)
			       arg)))
		 args)))
	    

(defun custom-error (fmt &rest args)
  "Replacement for standard `error' that uses `custom-format'"
  (signal 'error (list (apply 'custom-format fmt args))))


;;=========================================
;; Support for custom prin1 and princ

(defvar circle-table)
(defvar circle-tree)
(defvar circle-level)

(defun cust-print-top-level (object stream internal-printer)
  "Set up for printing."
  (let ((standard-output (or stream standard-output))
	(circle-table (and print-circle (cust-print-preprocess-circle-tree object)))
	(circle-level (or print-level -1))
	)

    (defalias 'cust-print-internal-printer internal-printer)
    (defalias 'cust-print-low-level-prin 
	  (cond
	   ((or custom-print-list
		custom-print-vector
		print-level ; comment out for version 19
		)
	    'cust-print-custom-object)
	   (circle-table
	    'cust-print-object)
	   (t 'cust-print-internal-printer)))
    (defalias 'cust-print-prin (if circle-table 'cust-print-circular 'cust-print-low-level-prin))

    (cust-print-prin object)
    object))


;; Test object type and print accordingly.
(defun cust-print-object (object)
  ;; Could be called as either cust-print-low-level-prin or cust-print-prin.
  (cond 
   ((null object) (cust-print-internal-printer object))
   ((consp object) (cust-print-list object))
   ((vectorp object) (cust-print-vector object))
   ;; All other types, just print.
   (t (cust-print-internal-printer object))))


;; Test object type and print accordingly.
(defun cust-print-custom-object (object)
  ;; Could be called as either cust-print-low-level-prin or cust-print-prin.
  (cond 
   ((null object) (cust-print-internal-printer object))

   ((consp object) 
    (or (and custom-print-list
	     (cust-print-custom-object1 object custom-print-list))
	(cust-print-list object)))

   ((vectorp object) 
    (or (and custom-print-vector
	     (cust-print-custom-object1 object custom-print-vector))
	(cust-print-vector object)))

   ;; All other types, just print.
   (t (cust-print-internal-printer object))))


;; Helper for cust-print-custom-object.
;; Print the custom OBJECT using the custom type ALIST.
;; For the first predicate that matches the object, the corresponding
;; converter is evaluated with the object and the string that results is
;; printed with princ.  Return nil if no predicate matches the object.
(defun cust-print-custom-object1 (object alist)
  (while (and alist (not (funcall (car (car alist)) object)))
    (setq alist (cdr alist)))
  ;; If alist is not null, then something matched.
  (if alist
      (cust-print-internal-princ
       (funcall (cdr (car alist)) object) ; returns string
       )))


(defun cust-print-circular (object)
  "Printer for `prin1' and `princ' that handles circular structures.
If OBJECT appears multiply, and has not yet been printed,
prefix with label; if it has been printed, use `#N#' instead.
Otherwise, print normally."
  (let ((tag (assq object circle-table)))
    (if tag
	(let ((id (cdr tag)))
	  (if (> id 0)
	      (progn
		;; Already printed, so just print id.
		(cust-print-internal-princ "#")
		(cust-print-internal-princ id)
		(cust-print-internal-princ "#"))
	    ;; Not printed yet, so label with id and print object.
	    (setcdr tag (- id)) ; mark it as printed
	    (cust-print-internal-princ "#")
	    (cust-print-internal-princ (- id))
	    (cust-print-internal-princ "=")
	    (cust-print-low-level-prin object)
	    ))
      ;; Not repeated in structure.
      (cust-print-low-level-prin object))))


;;================================================
;; List and vector processing for print functions.

;; Print a list using print-length, print-level, and print-circle.
(defun cust-print-list (list)
  (if (= circle-level 0)
      (cust-print-internal-princ "#")
    (let ((circle-level (1- circle-level)))
      (cust-print-internal-princ "(")
      (let ((length (or print-length 0)))

	;; Print the first element always (even if length = 0).
	(cust-print-prin (car list))
	(setq list (cdr list))
	(if list (cust-print-internal-princ " "))
	(setq length (1- length))

	;; Print the rest of the elements.
	(while (and list (/= 0 length))
	  (if (and (listp list)
		   (not (assq list circle-table)))
	      (progn
		(cust-print-prin (car list))
		(setq list (cdr list)))

	    ;; cdr is not a list, or it is in circle-table.
	    (cust-print-internal-princ ". ")
	    (cust-print-prin list)
	    (setq list nil))

	  (setq length (1- length))
	  (if list (cust-print-internal-princ " ")))

	(if (and list (= length 0)) (cust-print-internal-princ "..."))
	(cust-print-internal-princ ")"))))
  list)


;; Print a vector according to print-length, print-level, and print-circle.
(defun cust-print-vector (vector)
  (if (= circle-level 0)
      (cust-print-internal-princ "#")
    (let ((circle-level (1- circle-level))
	  (i 0)
	  (len (length vector)))
      (cust-print-internal-princ "[")

      (if print-length
	  (setq len (min print-length len)))
      ;; Print the elements
      (while (< i len)
	(cust-print-prin (aref vector i))
	(setq i (1+ i))
	(if (< i (length vector)) (cust-print-internal-princ " ")))

      (if (< i (length vector)) (cust-print-internal-princ "..."))
      (cust-print-internal-princ "]")
      ))
  vector)


;;==================================
;; Circular structure preprocessing

(defun cust-print-preprocess-circle-tree (object)
  ;; Fill up the table.  
  (let (;; Table of tags for each object in an object to be printed.
	;; A tag is of the form:
	;; ( <object> <nil-t-or-id-number> )
	;; The id-number is generated after the entire table has been computed.
	;; During walk through, the real circle-table lives in the cdr so we
	;; can use setcdr to add new elements instead of having to setq the
	;; variable sometimes (poor man's locf).
	(circle-table (list nil)))
    (cust-print-walk-circle-tree object)

    ;; Reverse table so it is in the order that the objects will be printed.
    ;; This pass could be avoided if we always added to the end of the
    ;; table with setcdr in walk-circle-tree.
    (setcdr circle-table (nreverse (cdr circle-table)))

    ;; Walk through the table, assigning id-numbers to those
    ;; objects which will be printed using #N= syntax.  Delete those
    ;; objects which will be printed only once (to speed up assq later).
    (let ((rest circle-table)
	  (id -1))
      (while (cdr rest)
	(let ((tag (car (cdr rest))))
	  (cond ((cdr tag)
		 (setcdr tag id)
		 (setq id (1- id))
		 (setq rest (cdr rest)))
		;; Else delete this object.
		(t (setcdr rest (cdr (cdr rest))))))
	))
    ;; Drop the car.
    (cdr circle-table)
    ))



(defun cust-print-walk-circle-tree (object)
  (let (read-equivalent-p tag)
    (while object
      (setq read-equivalent-p (or (numberp object) (symbolp object))
	    tag (and (not read-equivalent-p)
		     (assq object (cdr circle-table))))
      (cond (tag
	     ;; Seen this object already, so note that.
	     (setcdr tag t))

	    ((not read-equivalent-p)
	     ;; Add a tag for this object.
	     (setcdr circle-table
		     (cons (list object)
			   (cdr circle-table)))))
      (setq object
	    (cond 
	     (tag ;; No need to descend since we have already.
	      nil)

	     ((consp object)
	      ;; Walk the car of the list recursively.
	      (cust-print-walk-circle-tree (car object))
	      ;; But walk the cdr with the above while loop
	      ;; to avoid problems with max-lisp-eval-depth.
	      ;; And it should be faster than recursion.
	      (cdr object))

	     ((vectorp object)
	      ;; Walk the vector.
	      (let ((i (length object))
		    (j 0))
		(while (< j i)
		  (cust-print-walk-circle-tree (aref object j))
		  (setq j (1+ j))))))))))



;;=======================================

;; Example.

;;;; Create some circular structures.
;;(setq circ-sym (let ((x (make-symbol "FOO"))) (list x x)))
;;(setq circ-list (list 'a 'b (vector 1 2 3 4) 'd 'e 'f))
;;(setcar (nthcdr 3 circ-list) circ-list)
;;(aset (nth 2 circ-list) 2 circ-list)
;;(setq dotted-circ-list (list 'a 'b 'c))
;;(setcdr (cdr (cdr dotted-circ-list)) dotted-circ-list)
;;(setq circ-vector (vector 1 2 3 4 (list 'a 'b 'c 'd) 6 7))
;;(aset circ-vector 5 (make-symbol "-gensym-"))
;;(setcar (cdr (aref circ-vector 4)) (aref circ-vector 5))

;;(install-custom-print-funcs)
;;;; (setq print-circle t)

;;(let ((print-circle t))
;;  (or (equal (prin1-to-string circ-list) "#1=(a b [1 2 #1# 4] #1# e f)")
;;      (error "circular object with array printing")))

;;(let ((print-circle t))
;;  (or (equal (prin1-to-string dotted-circ-list) "#1=(a b c . #1#)")
;;      (error "circular object with array printing")))

;;(let* ((print-circle t)
;;       (x (list 'p 'q))
;;       (y (list (list 'a 'b) x 'foo x)))
;;  (setcdr (cdr (cdr (cdr y))) (cdr y))
;;  (or (equal (prin1-to-string y) "((a b) . #1=(#2=(p q) foo #2# . #1#))"
;;	     )
;;      (error "circular list example from CL manual")))

;;;; There's no special handling of uninterned symbols in custom-print.
;;(let ((print-circle nil))
;;  (or (equal (prin1-to-string circ-sym) "(#:FOO #:FOO)")
;;      (error "uninterned symbols in list")))
;;(let ((print-circle t))
;;  (or (equal (prin1-to-string circ-sym) "(#1=FOO #1#)")
;;      (error "circular uninterned symbols in list")))
;;(uninstall-custom-print-funcs)

;;; cust-print.el ends here
