;;; gnus-range.el --- range and sequence functions for Gnus
;; Copyright (C) 1996,97,98 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(eval-when-compile (require 'cl))

;;; List and range functions

(defun gnus-last-element (list)
  "Return last element of LIST."
  (while (cdr list)
    (setq list (cdr list)))
  (car list))

(defun gnus-copy-sequence (list)
  "Do a complete, total copy of a list."
  (let (out)
    (while (consp list)
      (if (consp (car list))
	  (push (gnus-copy-sequence (pop list)) out)
	(push (pop list) out)))
    (if list
	(nconc (nreverse out) list)
      (nreverse out))))

(defun gnus-set-difference (list1 list2)
  "Return a list of elements of LIST1 that do not appear in LIST2."
  (let ((list1 (copy-sequence list1)))
    (while list2
      (setq list1 (delq (car list2) list1))
      (setq list2 (cdr list2)))
    list1))

(defun gnus-sorted-complement (list1 list2)
  "Return a list of elements that are in LIST1 or LIST2 but not both.
Both lists have to be sorted over <."
  (let (out)
    (if (or (null list1) (null list2))
	(or list1 list2)
      (while (and list1 list2)
	(cond ((= (car list1) (car list2))
	       (setq list1 (cdr list1)
		     list2 (cdr list2)))
	      ((< (car list1) (car list2))
	       (setq out (cons (car list1) out))
	       (setq list1 (cdr list1)))
	      (t
	       (setq out (cons (car list2) out))
	       (setq list2 (cdr list2)))))
      (nconc (nreverse out) (or list1 list2)))))

(defun gnus-intersection (list1 list2)
  (let ((result nil))
    (while list2
      (when (memq (car list2) list1)
	(setq result (cons (car list2) result)))
      (setq list2 (cdr list2)))
    result))

(defun gnus-sorted-intersection (list1 list2)
  ;; LIST1 and LIST2 have to be sorted over <.
  (let (out)
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setq out (cons (car list1) out)
		   list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setq list1 (cdr list1)))
	    (t
	     (setq list2 (cdr list2)))))
    (nreverse out)))

(defun gnus-set-sorted-intersection (list1 list2)
  ;; LIST1 and LIST2 have to be sorted over <.
  ;; This function modifies LIST1.
  (let* ((top (cons nil list1))
	 (prev top))
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setq prev list1
		   list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setcdr prev (cdr list1))
	     (setq list1 (cdr list1)))
	    (t
	     (setq list2 (cdr list2)))))
    (setcdr prev nil)
    (cdr top)))

(defun gnus-compress-sequence (numbers &optional always-list)
  "Convert list of numbers to a list of ranges or a single range.
If ALWAYS-LIST is non-nil, this function will always release a list of
ranges."
  (let* ((first (car numbers))
	 (last (car numbers))
	 result)
    (if (null numbers)
	nil
      (if (not (listp (cdr numbers)))
	  numbers
	(while numbers
	  (cond ((= last (car numbers)) nil) ;Omit duplicated number
		((= (1+ last) (car numbers)) ;Still in sequence
		 (setq last (car numbers)))
		(t			;End of one sequence
		 (setq result
		       (cons (if (= first last) first
			       (cons first last))
			     result))
		 (setq first (car numbers))
		 (setq last  (car numbers))))
	  (setq numbers (cdr numbers)))
	(if (and (not always-list) (null result))
	    (if (= first last) (list first) (cons first last))
	  (nreverse (cons (if (= first last) first (cons first last))
			  result)))))))

(defalias 'gnus-uncompress-sequence 'gnus-uncompress-range)
(defun gnus-uncompress-range (ranges)
  "Expand a list of ranges into a list of numbers.
RANGES is either a single range on the form `(num . num)' or a list of
these ranges."
  (let (first last result)
    (cond
     ((null ranges)
      nil)
     ((not (listp (cdr ranges)))
      (setq first (car ranges))
      (setq last (cdr ranges))
      (while (<= first last)
	(setq result (cons first result))
	(setq first (1+ first)))
      (nreverse result))
     (t
      (while ranges
	(if (atom (car ranges))
	    (when (numberp (car ranges))
	      (setq result (cons (car ranges) result)))
	  (setq first (caar ranges))
	  (setq last  (cdar ranges))
	  (while (<= first last)
	    (setq result (cons first result))
	    (setq first (1+ first))))
	(setq ranges (cdr ranges)))
      (nreverse result)))))

(defun gnus-add-to-range (ranges list)
  "Return a list of ranges that has all articles from both RANGES and LIST.
Note: LIST has to be sorted over `<'."
  (if (not ranges)
      (gnus-compress-sequence list t)
    (setq list (copy-sequence list))
    (unless (listp (cdr ranges))
      (setq ranges (list ranges)))
    (let ((out ranges)
	  ilist lowest highest temp)
      (while (and ranges list)
	(setq ilist list)
	(setq lowest (or (and (atom (car ranges)) (car ranges))
			 (caar ranges)))
	(while (and list (cdr list) (< (cadr list) lowest))
	  (setq list (cdr list)))
	(when (< (car ilist) lowest)
	  (setq temp list)
	  (setq list (cdr list))
	  (setcdr temp nil)
	  (setq out (nconc (gnus-compress-sequence ilist t) out)))
	(setq highest (or (and (atom (car ranges)) (car ranges))
			  (cdar ranges)))
	(while (and list (<= (car list) highest))
	  (setq list (cdr list)))
	(setq ranges (cdr ranges)))
      (when list
	(setq out (nconc (gnus-compress-sequence list t) out)))
      (setq out (sort out (lambda (r1 r2)
			    (< (or (and (atom r1) r1) (car r1))
			       (or (and (atom r2) r2) (car r2))))))
      (setq ranges out)
      (while ranges
	(if (atom (car ranges))
	    (when (cdr ranges)
	      (if (atom (cadr ranges))
		  (when (= (1+ (car ranges)) (cadr ranges))
		    (setcar ranges (cons (car ranges)
					 (cadr ranges)))
		    (setcdr ranges (cddr ranges)))
		(when (= (1+ (car ranges)) (caadr ranges))
		  (setcar (cadr ranges) (car ranges))
		  (setcar ranges (cadr ranges))
		  (setcdr ranges (cddr ranges)))))
	  (when (cdr ranges)
	    (if (atom (cadr ranges))
		(when (= (1+ (cdar ranges)) (cadr ranges))
		  (setcdr (car ranges) (cadr ranges))
		  (setcdr ranges (cddr ranges)))
	      (when (= (1+ (cdar ranges)) (caadr ranges))
		(setcdr (car ranges) (cdadr ranges))
		(setcdr ranges (cddr ranges))))))
	(setq ranges (cdr ranges)))
      out)))

(defun gnus-remove-from-range (ranges list)
  "Return a list of ranges that has all articles from LIST removed from RANGES.
Note: LIST has to be sorted over `<'."
  ;; !!! This function shouldn't look like this, but I've got a headache.
  (gnus-compress-sequence
   (gnus-sorted-complement
    (gnus-uncompress-range ranges) list)))

(defun gnus-member-of-range (number ranges)
  (if (not (listp (cdr ranges)))
      (and (>= number (car ranges))
	   (<= number (cdr ranges)))
    (let ((not-stop t))
      (while (and ranges
		  (if (numberp (car ranges))
		      (>= number (car ranges))
		    (>= number (caar ranges)))
		  not-stop)
	(when (if (numberp (car ranges))
		  (= number (car ranges))
		(and (>= number (caar ranges))
		     (<= number (cdar ranges))))
	  (setq not-stop nil))
	(setq ranges (cdr ranges)))
      (not not-stop))))

(defun gnus-range-length (range)
  "Return the length RANGE would have if uncompressed."
  (length (gnus-uncompress-range range)))

(defun gnus-sublist-p (list sublist)
  "Test whether all elements in SUBLIST are members of LIST."
  (let ((sublistp t))
    (while sublist
      (unless (memq (pop sublist) list)
	(setq sublistp nil
	      sublist nil)))
    sublistp))

(defun gnus-range-add (range1 range2)
  "Add RANGE2 to RANGE1 destructively."
  (cond
   ;; If either are nil, then the job is quite easy.
   ((or (null range1) (null range2))
    (or range1 range2))
   (t
    ;; I don't like thinking.
    (gnus-compress-sequence
     (sort
      (nconc
       (gnus-uncompress-range range1)
       (gnus-uncompress-range range2))
      '<)))))

(provide 'gnus-range)

;;; gnus-range.el ends here
