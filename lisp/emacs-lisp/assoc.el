;;; assoc.el --- insert/delete/sort functions on association lists

;; Author: Barry A. Warsaw <bwarsaw@cen.com>
;; Keywords: extensions

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor accepts
;; responsibility to anyone for the consequences of using it or for
;; whether it serves any particular purpose or works at all, unless he
;; says so in writing.

;; This software was written as part of the supercite author's
;; official duty as an employee of the United States Government and is
;; thus in the public domain.  You are free to use that particular
;; software as you wish, but WITHOUT ANY WARRANTY WHATSOEVER.  It
;; would be nice, though if when you use any of this code, you give
;; due credit to the author.

;;; Commentary:

;; Association list utilities providing insertion, deletion, sorting
;; fetching off key-value pairs in association lists.

;;; Code:

(defun asort (assoc-symbol key)
  "Move a specified key-value pair to the head of an alist.
The alist is referenced by ALIST-SYMBOL. Key-value pair to move to
head is one matching KEY.  Returns the sorted list and doesn't affect
the order of any other key-value pair.  Side effect sets alist to new
sorted list."
  (set alist-symbol
       (sort (copy-alist (eval alist-symbol))
	     (function (lambda (a b) (equal (car a) key))))))


(defun aelement (key value)
  "Makes a list of a cons cell containing car of KEY and cdr of VALUE.
The returned list is suitable as an element of an alist."
  (list (cons key value)))


(defun aheadsym (alist)
  "Return the key symbol at the head of ALIST."
  (car (car alist)))


(defun anot-head-p (alist key)
  "Find out if a specified key-value pair is not at the head of an alist.
The alist to check is specified by ALIST and the key-value pair is the
one matching the supplied KEY.  Returns nil if ALIST is nil, or if
key-value pair is at the head of the alist.  Returns t if key-value
pair is not at the head of alist.  ALIST is not altered."
  (not (equal (aheadsym alist) key)))


(defun aput (alist-symbol key &optional value)
  "Inserts a key-value pair into an alist.
The alist is referenced by ALIST-SYMBOL. The key-value pair is made
from KEY and optionally, VALUE. Returns the altered alist or nil if
ALIST is nil.

If the key-value pair referenced by KEY can be found in the alist, and
VALUE is supplied non-nil, then the value of KEY will be set to VALUE.
If VALUE is not supplied, or is nil, the key-value pair will not be
modified, but will be moved to the head of the alist. If the key-value
pair cannot be found in the alist, it will be inserted into the head
of the alist (with value nil if VALUE is nil or not supplied)."
  (let ((elem (aelement key value))
	alist)
    (asort alist-symbol key)
    (setq alist (eval alist-symbol))
    (cond ((null alist) (set alist-symbol elem))
	  ((anot-head-p alist key) (set alist-symbol (nconc elem alist)))
	  (value (setcar alist (car elem)))
	  (t alist))))


(defun adelete (alist-symbol key)
  "Delete a key-value pair from the alist.
Alist is referenced by ALIST-SYMBOL and the key-value pair to remove
is pair matching KEY.  Returns the altered alist."
  (asort alist-symbol key)
  (let ((alist (eval alist-symbol)))
    (cond ((null alist) nil)
	  ((anot-head-p alist key) alist)
	  (t (set alist-symbol (cdr alist))))))


(defun aget (alist key &optional keynil-p)
  "Returns the value in ALIST that is associated with KEY.
Optional KEYNIL-P describes what to do if the value associated with
KEY is nil.  If KEYNIL-P is not supplied or is nil, and the value is
nil, then KEY is returned.  If KEYNIL-P is non-nil, then nil would be
returned.

If no key-value pair matching KEY could be found in ALIST, or ALIST is
nil then nil is returned. ALIST is not altered."
  (let ((copy (copy-alist alist)))
    (cond ((null alist) nil)
	  ((progn (asort 'copy key)
		  (anot-head-p copy key)) nil)
	  ((cdr (car copy)))
	  (keynil-p nil)
	  ((car (car copy)))
	  (t nil))))


(defun amake (alist-symbol keylist &optional valuelist)
  "Make an association list.
The association list is attached to the alist referenced by
ALIST-SYMBOL. Each element in the KEYLIST becomes a key and is
associated with the value in VALUELIST with the same index. If
VALUELIST is not supplied or is nil, then each key in KEYLIST is
associated with nil.

KEYLIST and VALUELIST should have the same number of elements, but
this isn't enforced.  If VALUELIST is smaller than KEYLIST, remaining
keys are associated with nil.  If VALUELIST is larger than KEYLIST,
extra values are ignored.  Returns the created alist."
  (let ((keycar (car keylist))
	(keycdr (cdr keylist))
	(valcar (car valuelist))
	(valcdr (cdr valuelist)))
    (cond ((null keycdr)
	   (aput alist-symbol keycar valcar))
	  (t
	   (amake alist-symbol keycdr valcdr)
	   (aput alist-symbol keycar valcar))))
  (eval alist-symbol))

(provide 'assoc)

;;; assoc.el ends here
