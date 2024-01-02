;;; gnus-range.el --- range and sequence functions for Gnus  -*- lexical-binding: t; -*-

;; Copyright (C) 1996-2024 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: news

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
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;; List and range functions

(require 'range)
(define-obsolete-function-alias 'gnus-range-normalize #'range-normalize "29.1")

(defun gnus-last-element (list)
  "Return last element of LIST."
  (while (cdr list)
    (setq list (cdr list)))
  (car list))
(make-obsolete 'gnus-last-element "use `car' of `last' instead." "27.1")

(define-obsolete-function-alias 'gnus-copy-sequence #'copy-tree "27.1")

;; We could be using `seq-difference' here, but it's much slower
;; on these data sets.  See bug#50877.
(defun gnus-set-difference (list1 list2)
  "Return a list of elements of LIST1 that do not appear in LIST2."
  (let ((hash2 (make-hash-table :test 'eq))
        (result nil))
    (dolist (elt list2) (puthash elt t hash2))
    (dolist (elt list1)
      (unless (gethash elt hash2)
        (setq result (cons elt result))))
    (nreverse result)))

(defun gnus-range-nconcat (&rest ranges)
  "Return a range comprising all the RANGES, which are pre-sorted.
RANGES will be destructively altered."
  (setq ranges (delete nil ranges))
  (let* ((result (range-normalize (pop ranges)))
	 (last (last result)))
    (dolist (range ranges)
      (setq range (range-normalize range))
      ;; Normalize the single-number case, so that we don't need to
      ;; special-case that so much.
      (when (numberp (car last))
	(setcar last (cons (car last) (car last))))
      (when (numberp (car range))
	(setcar range (cons (car range) (car range))))
      (if (= (1+ (cdar last)) (caar range))
	  (progn
	    (setcdr (car last) (cdar range))
	    (setcdr last (cdr range)))
	(setcdr last range)
	;; Denormalize back, since we couldn't join the ranges up.
	(when (= (caar range) (cdar range))
	  (setcar range (caar range)))
	(when (= (caar last) (cdar last))
	  (setcar last (caar last))))
      (setq last (last last)))
    (if (and (consp (car result))
	     (= (length result) 1))
	(car result)
      result)))

(define-obsolete-function-alias 'gnus-range-difference
  #'range-difference "29.1")

;;;###autoload
(defun gnus-sorted-difference (list1 list2)
  "Return a list of elements of LIST1 that do not appear in LIST2.
Both lists have to be sorted over <.
The tail of LIST1 is not copied."
  (let (out)
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setq list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setq out (cons (car list1) out))
	     (setq list1 (cdr list1)))
	    (t
	     (setq list2 (cdr list2)))))
    (nconc (nreverse out) list1)))

;;;###autoload
(defun gnus-sorted-ndifference (list1 list2)
  "Return a list of elements of LIST1 that do not appear in LIST2.
Both lists have to be sorted over <.
LIST1 is modified."
  (let* ((top (cons nil list1))
	 (prev top))
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setcdr prev (cdr list1))
	     (setq list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setq prev list1
		   list1 (cdr list1)))
	    (t
	     (setq list2 (cdr list2)))))
    (cdr top)))

;;;###autoload
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

;;;###autoload
(defun gnus-intersection (list1 list2)
  (declare (obsolete seq-intersection "28.1"))
  (nreverse (seq-intersection list1 list2 #'eq)))

;;;###autoload
(defun gnus-sorted-intersection (list1 list2)
  "Return intersection of LIST1 and LIST2.
LIST1 and LIST2 have to be sorted over <."
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

(define-obsolete-function-alias 'gnus-sorted-range-intersection
  #'range-intersection "29.1")

;;;###autoload
(defalias 'gnus-set-sorted-intersection #'gnus-sorted-nintersection)

;;;###autoload
(defun gnus-sorted-nintersection (list1 list2)
  "Return intersection of LIST1 and LIST2 by modifying cdr pointers of LIST1.
LIST1 and LIST2 have to be sorted over <."
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

;;;###autoload
(defun gnus-sorted-union (list1 list2)
  "Return union of LIST1 and LIST2.
LIST1 and LIST2 have to be sorted over <."
  (let (out)
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setq out (cons (car list1) out)
		   list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setq out (cons (car list1) out)
		   list1 (cdr list1)))
	    (t
	     (setq out (cons (car list2) out)
		   list2 (cdr list2)))))
    (while list1
      (setq out (cons (car list1) out)
	    list1 (cdr list1)))
    (while list2
      (setq out (cons (car list2) out)
	    list2 (cdr list2)))
    (nreverse out)))

;;;###autoload
(defun gnus-sorted-nunion (list1 list2)
  "Return union of LIST1 and LIST2 by modifying cdr pointers of LIST1.
LIST1 and LIST2 have to be sorted over <."
  (let* ((top (cons nil list1))
	 (prev top))
    (while (and list1 list2)
      (cond ((= (car list1) (car list2))
	     (setq prev list1
		   list1 (cdr list1)
		   list2 (cdr list2)))
	    ((< (car list1) (car list2))
	     (setq prev list1
		   list1 (cdr list1)))
	    (t
	     (setcdr prev (list (car list2)))
	     (setq prev (cdr prev)
		   list2 (cdr list2))
	     (setcdr prev list1))))
    (while list2
      (setcdr prev (list (car list2)))
      (setq prev (cdr prev)
	    list2 (cdr list2)))
    (cdr top)))

(defun gnus-compress-sequence (numbers &optional always-list)
  "Convert sorted list of numbers to a list of ranges or a single range.
If ALWAYS-LIST is non-nil, this function will always release a list of
ranges."
  (if always-list
      (range-compress-list numbers)
    (range-denormalize (range-compress-list numbers))))

(defalias 'gnus-uncompress-sequence #'gnus-uncompress-range)
(define-obsolete-function-alias 'gnus-uncompress-range
  #'range-uncompress "29.1")

(define-obsolete-function-alias 'gnus-add-to-range
  #'range-add-list "29.1")

(define-obsolete-function-alias 'gnus-remove-from-range
  #'range-remove "29.1")

(define-obsolete-function-alias 'gnus-member-of-range #'range-member-p "29.1")

(define-obsolete-function-alias 'gnus-list-range-intersection
  #'range-list-intersection "29.1")

(defalias 'gnus-inverse-list-range-intersection #'range-list-difference)

(define-obsolete-function-alias 'gnus-list-range-difference
  #'range-list-difference "29.1")

(define-obsolete-function-alias 'gnus-range-length #'range-length "29.1")

(define-obsolete-function-alias 'gnus-range-add #'range-concat "29.1")

;;;###autoload
(defun gnus-add-to-sorted-list (list num)
  "Add NUM into sorted LIST by side effect."
  (let* ((top (cons nil list))
	 (prev top))
    (while (and list (< (car list) num))
      (setq prev list
	    list (cdr list)))
    (unless (eq (car list) num)
      (setcdr prev (cons num list)))
    (cdr top)))

(define-obsolete-function-alias 'gnus-range-map #'range-map "29.1")

(provide 'gnus-range)

;;; gnus-range.el ends here
