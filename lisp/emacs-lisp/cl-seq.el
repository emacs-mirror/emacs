;; cl-seq.el --- Common Lisp extensions for GNU Emacs Lisp (part three)

;; Copyright (C) 1993 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Version: 2.02
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

;; These are extensions to Emacs Lisp that provide a degree of
;; Common Lisp compatibility, beyond what is already built-in
;; in Emacs Lisp.
;;
;; This package was written by Dave Gillespie; it is a complete
;; rewrite of Cesar Quiroz's original cl.el package of December 1986.
;;
;; This package works with Emacs 18, Emacs 19, and Lucid Emacs 19.
;;
;; Bug reports, comments, and suggestions are welcome!

;; This file contains the Common Lisp sequence and list functions
;; which take keyword arguments.

;; See cl.el for Change Log.


;;; Code:

(or (memq 'cl-19 features)
    (error "Tried to load `cl-seq' before `cl'!"))


;;; We define these here so that this file can compile without having
;;; loaded the cl.el file already.

(defmacro cl-push (x place) (list 'setq place (list 'cons x place)))
(defmacro cl-pop (place)
  (list 'car (list 'prog1 place (list 'setq place (list 'cdr place)))))


;;; Keyword parsing.  This is special-cased here so that we can compile
;;; this file independent from cl-macs.

(defmacro cl-parsing-keywords (kwords other-keys &rest body)
  (cons
   'let*
   (cons (mapcar
	  (function
	   (lambda (x)
	     (let* ((var (if (consp x) (car x) x))
		    (mem (list 'car (list 'cdr (list 'memq (list 'quote var)
						     'cl-keys)))))
	       (if (eq var ':test-not)
		   (setq mem (list 'and mem (list 'setq 'cl-test mem) t)))
	       (if (eq var ':if-not)
		   (setq mem (list 'and mem (list 'setq 'cl-if mem) t)))
	       (list (intern
		      (format "cl-%s" (substring (symbol-name var) 1)))
		     (if (consp x) (list 'or mem (car (cdr x))) mem)))))
	  kwords)
	 (append
	  (and (not (eq other-keys t))
	       (list
		(list 'let '((cl-keys-temp cl-keys))
		      (list 'while 'cl-keys-temp
			    (list 'or (list 'memq '(car cl-keys-temp)
					    (list 'quote
						  (mapcar
						   (function
						    (lambda (x)
						      (if (consp x)
							  (car x) x)))
						   (append kwords
							   other-keys))))
				  '(car (cdr (memq (quote :allow-other-keys)
						   cl-keys)))
				  '(error "Bad keyword argument %s"
					  (car cl-keys-temp)))
			    '(setq cl-keys-temp (cdr (cdr cl-keys-temp)))))))
	  body))))
(put 'cl-parsing-keywords 'lisp-indent-function 2)
(put 'cl-parsing-keywords 'edebug-form-spec '(sexp sexp &rest form))

(defmacro cl-check-key (x)
  (list 'if 'cl-key (list 'funcall 'cl-key x) x))

(defmacro cl-check-test-nokey (item x)
  (list 'cond
	(list 'cl-test
	      (list 'eq (list 'not (list 'funcall 'cl-test item x))
		    'cl-test-not))
	(list 'cl-if
	      (list 'eq (list 'not (list 'funcall 'cl-if x)) 'cl-if-not))
	(list 't (list 'if (list 'numberp item)
		       (list 'equal item x) (list 'eq item x)))))

(defmacro cl-check-test (item x)
  (list 'cl-check-test-nokey item (list 'cl-check-key x)))

(defmacro cl-check-match (x y)
  (setq x (list 'cl-check-key x) y (list 'cl-check-key y))
  (list 'if 'cl-test
	(list 'eq (list 'not (list 'funcall 'cl-test x y)) 'cl-test-not)
	(list 'if (list 'numberp x)
	      (list 'equal x y) (list 'eq x y))))

(put 'cl-check-key 'edebug-form-spec 'edebug-forms)
(put 'cl-check-test 'edebug-form-spec 'edebug-forms)
(put 'cl-check-test-nokey 'edebug-form-spec 'edebug-forms)
(put 'cl-check-match 'edebug-form-spec 'edebug-forms)

(defvar cl-test) (defvar cl-test-not)
(defvar cl-if) (defvar cl-if-not)
(defvar cl-key)


(defun reduce (cl-func cl-seq &rest cl-keys)
  "Reduce two-argument FUNCTION across SEQUENCE.
Keywords supported:  :start :end :from-end :initial-value :key"
  (cl-parsing-keywords (:from-end (:start 0) :end :initial-value :key) ()
    (or (listp cl-seq) (setq cl-seq (append cl-seq nil)))
    (setq cl-seq (subseq cl-seq cl-start cl-end))
    (if cl-from-end (setq cl-seq (nreverse cl-seq)))
    (let ((cl-accum (cond ((memq ':initial-value cl-keys) cl-initial-value)
			  (cl-seq (cl-check-key (cl-pop cl-seq)))
			  (t (funcall cl-func)))))
      (if cl-from-end
	  (while cl-seq
	    (setq cl-accum (funcall cl-func (cl-check-key (cl-pop cl-seq))
				    cl-accum)))
	(while cl-seq
	  (setq cl-accum (funcall cl-func cl-accum
				  (cl-check-key (cl-pop cl-seq))))))
      cl-accum)))

(defun fill (seq item &rest cl-keys)
  "Fill the elements of SEQ with ITEM.
Keywords supported:  :start :end"
  (cl-parsing-keywords ((:start 0) :end) ()
    (if (listp seq)
	(let ((p (nthcdr cl-start seq))
	      (n (if cl-end (- cl-end cl-start) 8000000)))
	  (while (and p (>= (setq n (1- n)) 0))
	    (setcar p item)
	    (setq p (cdr p))))
      (or cl-end (setq cl-end (length seq)))
      (if (and (= cl-start 0) (= cl-end (length seq)))
	  (fillarray seq item)
	(while (< cl-start cl-end)
	  (aset seq cl-start item)
	  (setq cl-start (1+ cl-start)))))
    seq))

(defun replace (cl-seq1 cl-seq2 &rest cl-keys)
  "Replace the elements of SEQ1 with the elements of SEQ2.
SEQ1 is destructively modified, then returned.
Keywords supported:  :start1 :end1 :start2 :end2"
  (cl-parsing-keywords ((:start1 0) :end1 (:start2 0) :end2) ()
    (if (and (eq cl-seq1 cl-seq2) (<= cl-start2 cl-start1))
	(or (= cl-start1 cl-start2)
	    (let* ((cl-len (length cl-seq1))
		   (cl-n (min (- (or cl-end1 cl-len) cl-start1)
			      (- (or cl-end2 cl-len) cl-start2))))
	      (while (>= (setq cl-n (1- cl-n)) 0)
		(cl-set-elt cl-seq1 (+ cl-start1 cl-n)
			    (elt cl-seq2 (+ cl-start2 cl-n))))))
      (if (listp cl-seq1)
	  (let ((cl-p1 (nthcdr cl-start1 cl-seq1))
		(cl-n1 (if cl-end1 (- cl-end1 cl-start1) 4000000)))
	    (if (listp cl-seq2)
		(let ((cl-p2 (nthcdr cl-start2 cl-seq2))
		      (cl-n (min cl-n1
				 (if cl-end2 (- cl-end2 cl-start2) 4000000))))
		  (while (and cl-p1 cl-p2 (>= (setq cl-n (1- cl-n)) 0))
		    (setcar cl-p1 (car cl-p2))
		    (setq cl-p1 (cdr cl-p1) cl-p2 (cdr cl-p2))))
	      (setq cl-end2 (min (or cl-end2 (length cl-seq2))
				 (+ cl-start2 cl-n1)))
	      (while (and cl-p1 (< cl-start2 cl-end2))
		(setcar cl-p1 (aref cl-seq2 cl-start2))
		(setq cl-p1 (cdr cl-p1) cl-start2 (1+ cl-start2)))))
	(setq cl-end1 (min (or cl-end1 (length cl-seq1))
			   (+ cl-start1 (- (or cl-end2 (length cl-seq2))
					   cl-start2))))
	(if (listp cl-seq2)
	    (let ((cl-p2 (nthcdr cl-start2 cl-seq2)))
	      (while (< cl-start1 cl-end1)
		(aset cl-seq1 cl-start1 (car cl-p2))
		(setq cl-p2 (cdr cl-p2) cl-start1 (1+ cl-start1))))
	  (while (< cl-start1 cl-end1)
	    (aset cl-seq1 cl-start1 (aref cl-seq2 cl-start2))
	    (setq cl-start2 (1+ cl-start2) cl-start1 (1+ cl-start1))))))
    cl-seq1))

(defun remove* (cl-item cl-seq &rest cl-keys)
  "Remove all occurrences of ITEM in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
Keywords supported:  :test :test-not :key :count :start :end :from-end"
  (cl-parsing-keywords (:test :test-not :key :if :if-not :count :from-end
			(:start 0) :end) ()
    (if (<= (or cl-count (setq cl-count 8000000)) 0)
	cl-seq
      (if (or (nlistp cl-seq) (and cl-from-end (< cl-count 4000000)))
	  (let ((cl-i (cl-position cl-item cl-seq cl-start cl-end
				   cl-from-end)))
	    (if cl-i
		(let ((cl-res (apply 'delete* cl-item (append cl-seq nil)
				     (append (if cl-from-end
						 (list ':end (1+ cl-i))
					       (list ':start cl-i))
					     cl-keys))))
		  (if (listp cl-seq) cl-res
		    (if (stringp cl-seq) (concat cl-res) (vconcat cl-res))))
	      cl-seq))
	(setq cl-end (- (or cl-end 8000000) cl-start))
	(if (= cl-start 0)
	    (while (and cl-seq (> cl-end 0)
			(cl-check-test cl-item (car cl-seq))
			(setq cl-end (1- cl-end) cl-seq (cdr cl-seq))
			(> (setq cl-count (1- cl-count)) 0))))
	(if (and (> cl-count 0) (> cl-end 0))
	    (let ((cl-p (if (> cl-start 0) (nthcdr cl-start cl-seq)
			  (setq cl-end (1- cl-end)) (cdr cl-seq))))
	      (while (and cl-p (> cl-end 0)
			  (not (cl-check-test cl-item (car cl-p))))
		(setq cl-p (cdr cl-p) cl-end (1- cl-end)))
	      (if (and cl-p (> cl-end 0))
		  (nconc (ldiff cl-seq cl-p)
			 (if (= cl-count 1) (cdr cl-p)
			   (and (cdr cl-p)
				(apply 'delete* cl-item
				       (copy-sequence (cdr cl-p))
				       ':start 0 ':end (1- cl-end)
				       ':count (1- cl-count) cl-keys))))
		cl-seq))
	  cl-seq)))))

(defun remove-if (cl-pred cl-list &rest cl-keys)
  "Remove all items satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
Keywords supported:  :key :count :start :end :from-end"
  (apply 'remove* nil cl-list ':if cl-pred cl-keys))

(defun remove-if-not (cl-pred cl-list &rest cl-keys)
  "Remove all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
Keywords supported:  :key :count :start :end :from-end"
  (apply 'remove* nil cl-list ':if-not cl-pred cl-keys))

(defun delete* (cl-item cl-seq &rest cl-keys)
  "Remove all occurrences of ITEM in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
Keywords supported:  :test :test-not :key :count :start :end :from-end"
  (cl-parsing-keywords (:test :test-not :key :if :if-not :count :from-end
			(:start 0) :end) ()
    (if (<= (or cl-count (setq cl-count 8000000)) 0)
	cl-seq
      (if (listp cl-seq)
	  (if (and cl-from-end (< cl-count 4000000))
	      (let (cl-i)
		(while (and (>= (setq cl-count (1- cl-count)) 0)
			    (setq cl-i (cl-position cl-item cl-seq cl-start
						    cl-end cl-from-end)))
		  (if (= cl-i 0) (setq cl-seq (cdr cl-seq))
		    (let ((cl-tail (nthcdr (1- cl-i) cl-seq)))
		      (setcdr cl-tail (cdr (cdr cl-tail)))))
		  (setq cl-end cl-i))
		cl-seq)
	    (setq cl-end (- (or cl-end 8000000) cl-start))
	    (if (= cl-start 0)
		(progn
		  (while (and cl-seq
			      (> cl-end 0)
			      (cl-check-test cl-item (car cl-seq))
			      (setq cl-end (1- cl-end) cl-seq (cdr cl-seq))
			      (> (setq cl-count (1- cl-count)) 0)))
		  (setq cl-end (1- cl-end)))
	      (setq cl-start (1- cl-start)))
	    (if (and (> cl-count 0) (> cl-end 0))
		(let ((cl-p (nthcdr cl-start cl-seq)))
		  (while (and (cdr cl-p) (> cl-end 0))
		    (if (cl-check-test cl-item (car (cdr cl-p)))
			(progn
			  (setcdr cl-p (cdr (cdr cl-p)))
			  (if (= (setq cl-count (1- cl-count)) 0)
			      (setq cl-end 1)))
		      (setq cl-p (cdr cl-p)))
		    (setq cl-end (1- cl-end)))))
	    cl-seq)
	(apply 'remove* cl-item cl-seq cl-keys)))))

(defun delete-if (cl-pred cl-list &rest cl-keys)
  "Remove all items satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
Keywords supported:  :key :count :start :end :from-end"
  (apply 'delete* nil cl-list ':if cl-pred cl-keys))

(defun delete-if-not (cl-pred cl-list &rest cl-keys)
  "Remove all items not satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
Keywords supported:  :key :count :start :end :from-end"
  (apply 'delete* nil cl-list ':if-not cl-pred cl-keys))

(or (and (fboundp 'delete) (subrp (symbol-function 'delete)))
    (defalias 'delete (function (lambda (x y) (delete* x y ':test 'equal)))))
(defun remove (x y) (remove* x y ':test 'equal))
(defun remq (x y) (if (memq x y) (delq x (copy-list y)) y))

(defun remove-duplicates (cl-seq &rest cl-keys)
  "Return a copy of SEQ with all duplicate elements removed.
Keywords supported:  :test :test-not :key :start :end :from-end"
  (cl-delete-duplicates cl-seq cl-keys t))

(defun delete-duplicates (cl-seq &rest cl-keys)
  "Remove all duplicate elements from SEQ (destructively).
Keywords supported:  :test :test-not :key :start :end :from-end"
  (cl-delete-duplicates cl-seq cl-keys nil))

(defun cl-delete-duplicates (cl-seq cl-keys cl-copy)
  (if (listp cl-seq)
      (cl-parsing-keywords (:test :test-not :key (:start 0) :end :from-end :if)
	  ()
	(if cl-from-end
	    (let ((cl-p (nthcdr cl-start cl-seq)) cl-i)
	      (setq cl-end (- (or cl-end (length cl-seq)) cl-start))
	      (while (> cl-end 1)
		(setq cl-i 0)
		(while (setq cl-i (cl-position (cl-check-key (car cl-p))
					       (cdr cl-p) cl-i (1- cl-end)))
		  (if cl-copy (setq cl-seq (copy-sequence cl-seq)
				    cl-p (nthcdr cl-start cl-seq) cl-copy nil))
		  (let ((cl-tail (nthcdr cl-i cl-p)))
		    (setcdr cl-tail (cdr (cdr cl-tail))))
		  (setq cl-end (1- cl-end)))
		(setq cl-p (cdr cl-p) cl-end (1- cl-end)
		      cl-start (1+ cl-start)))
	      cl-seq)
	  (setq cl-end (- (or cl-end (length cl-seq)) cl-start))
	  (while (and (cdr cl-seq) (= cl-start 0) (> cl-end 1)
		      (cl-position (cl-check-key (car cl-seq))
				   (cdr cl-seq) 0 (1- cl-end)))
	    (setq cl-seq (cdr cl-seq) cl-end (1- cl-end)))
	  (let ((cl-p (if (> cl-start 0) (nthcdr (1- cl-start) cl-seq)
			(setq cl-end (1- cl-end) cl-start 1) cl-seq)))
	    (while (and (cdr (cdr cl-p)) (> cl-end 1))
	      (if (cl-position (cl-check-key (car (cdr cl-p)))
			       (cdr (cdr cl-p)) 0 (1- cl-end))
		  (progn
		    (if cl-copy (setq cl-seq (copy-sequence cl-seq)
				      cl-p (nthcdr (1- cl-start) cl-seq)
				      cl-copy nil))
		    (setcdr cl-p (cdr (cdr cl-p))))
		(setq cl-p (cdr cl-p)))
	      (setq cl-end (1- cl-end) cl-start (1+ cl-start)))
	    cl-seq)))
    (let ((cl-res (cl-delete-duplicates (append cl-seq nil) cl-keys nil)))
      (if (stringp cl-seq) (concat cl-res) (vconcat cl-res)))))

(defun substitute (cl-new cl-old cl-seq &rest cl-keys)
  "Substitute NEW for OLD in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
Keywords supported:  :test :test-not :key :count :start :end :from-end"
  (cl-parsing-keywords (:test :test-not :key :if :if-not :count
			(:start 0) :end :from-end) ()
    (if (or (eq cl-old cl-new)
	    (<= (or cl-count (setq cl-from-end nil cl-count 8000000)) 0))
	cl-seq
      (let ((cl-i (cl-position cl-old cl-seq cl-start cl-end)))
	(if (not cl-i)
	    cl-seq
	  (setq cl-seq (copy-sequence cl-seq))
	  (or cl-from-end
	      (progn (cl-set-elt cl-seq cl-i cl-new)
		     (setq cl-i (1+ cl-i) cl-count (1- cl-count))))
	  (apply 'nsubstitute cl-new cl-old cl-seq ':count cl-count
		 ':start cl-i cl-keys))))))

(defun substitute-if (cl-new cl-pred cl-list &rest cl-keys)
  "Substitute NEW for all items satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
Keywords supported:  :key :count :start :end :from-end"
  (apply 'substitute cl-new nil cl-list ':if cl-pred cl-keys))

(defun substitute-if-not (cl-new cl-pred cl-list &rest cl-keys)
  "Substitute NEW for all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
Keywords supported:  :key :count :start :end :from-end"
  (apply 'substitute cl-new nil cl-list ':if-not cl-pred cl-keys))

(defun nsubstitute (cl-new cl-old cl-seq &rest cl-keys)
  "Substitute NEW for OLD in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
Keywords supported:  :test :test-not :key :count :start :end :from-end"
  (cl-parsing-keywords (:test :test-not :key :if :if-not :count
			(:start 0) :end :from-end) ()
    (or (eq cl-old cl-new) (<= (or cl-count (setq cl-count 8000000)) 0)
	(if (and (listp cl-seq) (or (not cl-from-end) (> cl-count 4000000)))
	    (let ((cl-p (nthcdr cl-start cl-seq)))
	      (setq cl-end (- (or cl-end 8000000) cl-start))
	      (while (and cl-p (> cl-end 0) (> cl-count 0))
		(if (cl-check-test cl-old (car cl-p))
		    (progn
		      (setcar cl-p cl-new)
		      (setq cl-count (1- cl-count))))
		(setq cl-p (cdr cl-p) cl-end (1- cl-end))))
	  (or cl-end (setq cl-end (length cl-seq)))
	  (if cl-from-end
	      (while (and (< cl-start cl-end) (> cl-count 0))
		(setq cl-end (1- cl-end))
		(if (cl-check-test cl-old (elt cl-seq cl-end))
		    (progn
		      (cl-set-elt cl-seq cl-end cl-new)
		      (setq cl-count (1- cl-count)))))
	    (while (and (< cl-start cl-end) (> cl-count 0))
	      (if (cl-check-test cl-old (aref cl-seq cl-start))
		  (progn
		    (aset cl-seq cl-start cl-new)
		    (setq cl-count (1- cl-count))))
	      (setq cl-start (1+ cl-start))))))
    cl-seq))

(defun nsubstitute-if (cl-new cl-pred cl-list &rest cl-keys)
  "Substitute NEW for all items satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
Keywords supported:  :key :count :start :end :from-end"
  (apply 'nsubstitute cl-new nil cl-list ':if cl-pred cl-keys))

(defun nsubstitute-if-not (cl-new cl-pred cl-list &rest cl-keys)
  "Substitute NEW for all items not satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
Keywords supported:  :key :count :start :end :from-end"
  (apply 'nsubstitute cl-new nil cl-list ':if-not cl-pred cl-keys))

(defun find (cl-item cl-seq &rest cl-keys)
  "Find the first occurrence of ITEM in LIST.
Return the matching ITEM, or nil if not found.
Keywords supported:  :test :test-not :key :start :end :from-end"
  (let ((cl-pos (apply 'position cl-item cl-seq cl-keys)))
    (and cl-pos (elt cl-seq cl-pos))))

(defun find-if (cl-pred cl-list &rest cl-keys)
  "Find the first item satisfying PREDICATE in LIST.
Return the matching ITEM, or nil if not found.
Keywords supported:  :key :start :end :from-end"
  (apply 'find nil cl-list ':if cl-pred cl-keys))

(defun find-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item not satisfying PREDICATE in LIST.
Return the matching ITEM, or nil if not found.
Keywords supported:  :key :start :end :from-end"
  (apply 'find nil cl-list ':if-not cl-pred cl-keys))

(defun position (cl-item cl-seq &rest cl-keys)
  "Find the first occurrence of ITEM in LIST.
Return the index of the matching item, or nil if not found.
Keywords supported:  :test :test-not :key :start :end :from-end"
  (cl-parsing-keywords (:test :test-not :key :if :if-not
			(:start 0) :end :from-end) ()
    (cl-position cl-item cl-seq cl-start cl-end cl-from-end)))

(defun cl-position (cl-item cl-seq cl-start &optional cl-end cl-from-end)
  (if (listp cl-seq)
      (let ((cl-p (nthcdr cl-start cl-seq)))
	(or cl-end (setq cl-end 8000000))
	(let ((cl-res nil))
	  (while (and cl-p (< cl-start cl-end) (or (not cl-res) cl-from-end))
	    (if (cl-check-test cl-item (car cl-p))
		(setq cl-res cl-start))
	    (setq cl-p (cdr cl-p) cl-start (1+ cl-start)))
	  cl-res))
    (or cl-end (setq cl-end (length cl-seq)))
    (if cl-from-end
	(progn
	  (while (and (>= (setq cl-end (1- cl-end)) cl-start)
		      (not (cl-check-test cl-item (aref cl-seq cl-end)))))
	  (and (>= cl-end cl-start) cl-end))
      (while (and (< cl-start cl-end)
		  (not (cl-check-test cl-item (aref cl-seq cl-start))))
	(setq cl-start (1+ cl-start)))
      (and (< cl-start cl-end) cl-start))))

(defun position-if (cl-pred cl-list &rest cl-keys)
  "Find the first item satisfying PREDICATE in LIST.
Return the index of the matching item, or nil if not found.
Keywords supported:  :key :start :end :from-end"
  (apply 'position nil cl-list ':if cl-pred cl-keys))

(defun position-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item not satisfying PREDICATE in LIST.
Return the index of the matching item, or nil if not found.
Keywords supported:  :key :start :end :from-end"
  (apply 'position nil cl-list ':if-not cl-pred cl-keys))

(defun count (cl-item cl-seq &rest cl-keys)
  "Count the number of occurrences of ITEM in LIST.
Keywords supported:  :test :test-not :key :start :end"
  (cl-parsing-keywords (:test :test-not :key :if :if-not (:start 0) :end) ()
    (let ((cl-count 0) cl-x)
      (or cl-end (setq cl-end (length cl-seq)))
      (if (consp cl-seq) (setq cl-seq (nthcdr cl-start cl-seq)))
      (while (< cl-start cl-end)
	(setq cl-x (if (consp cl-seq) (cl-pop cl-seq) (aref cl-seq cl-start)))
	(if (cl-check-test cl-item cl-x) (setq cl-count (1+ cl-count)))
	(setq cl-start (1+ cl-start)))
      cl-count)))

(defun count-if (cl-pred cl-list &rest cl-keys)
  "Count the number of items satisfying PREDICATE in LIST.
Keywords supported:  :key :start :end"
  (apply 'count nil cl-list ':if cl-pred cl-keys))

(defun count-if-not (cl-pred cl-list &rest cl-keys)
  "Count the number of items not satisfying PREDICATE in LIST.
Keywords supported:  :key :start :end"
  (apply 'count nil cl-list ':if-not cl-pred cl-keys))

(defun mismatch (cl-seq1 cl-seq2 &rest cl-keys)
  "Compare SEQ1 with SEQ2, return index of first mismatching element.
Return nil if the sequences match.  If one sequence is a prefix of the
other, the return value indicates the end of the shorted sequence.
Keywords supported:  :test :test-not :key :start1 :end1 :start2 :end2 :from-end"
  (cl-parsing-keywords (:test :test-not :key :from-end
			(:start1 0) :end1 (:start2 0) :end2) ()
    (or cl-end1 (setq cl-end1 (length cl-seq1)))
    (or cl-end2 (setq cl-end2 (length cl-seq2)))
    (if cl-from-end
	(progn
	  (while (and (< cl-start1 cl-end1) (< cl-start2 cl-end2)
		      (cl-check-match (elt cl-seq1 (1- cl-end1))
				      (elt cl-seq2 (1- cl-end2))))
	    (setq cl-end1 (1- cl-end1) cl-end2 (1- cl-end2)))
	  (and (or (< cl-start1 cl-end1) (< cl-start2 cl-end2))
	       (1- cl-end1)))
      (let ((cl-p1 (and (listp cl-seq1) (nthcdr cl-start1 cl-seq1)))
	    (cl-p2 (and (listp cl-seq2) (nthcdr cl-start2 cl-seq2))))
	(while (and (< cl-start1 cl-end1) (< cl-start2 cl-end2)
		    (cl-check-match (if cl-p1 (car cl-p1)
				      (aref cl-seq1 cl-start1))
				    (if cl-p2 (car cl-p2)
				      (aref cl-seq2 cl-start2))))
	  (setq cl-p1 (cdr cl-p1) cl-p2 (cdr cl-p2)
		cl-start1 (1+ cl-start1) cl-start2 (1+ cl-start2)))
	(and (or (< cl-start1 cl-end1) (< cl-start2 cl-end2))
	     cl-start1)))))

(defun search (cl-seq1 cl-seq2 &rest cl-keys)
  "Search for SEQ1 as a subsequence of SEQ2.
Return the index of the leftmost element of the first match found;
return nil if there are no matches.
Keywords supported:  :test :test-not :key :start1 :end1 :start2 :end2 :from-end"
  (cl-parsing-keywords (:test :test-not :key :from-end
			(:start1 0) :end1 (:start2 0) :end2) ()
    (or cl-end1 (setq cl-end1 (length cl-seq1)))
    (or cl-end2 (setq cl-end2 (length cl-seq2)))
    (if (>= cl-start1 cl-end1)
	(if cl-from-end cl-end2 cl-start2)
      (let* ((cl-len (- cl-end1 cl-start1))
	     (cl-first (cl-check-key (elt cl-seq1 cl-start1)))
	     (cl-if nil) cl-pos)
	(setq cl-end2 (- cl-end2 (1- cl-len)))
	(while (and (< cl-start2 cl-end2)
		    (setq cl-pos (cl-position cl-first cl-seq2
					      cl-start2 cl-end2 cl-from-end))
		    (apply 'mismatch cl-seq1 cl-seq2
			   ':start1 (1+ cl-start1) ':end1 cl-end1
			   ':start2 (1+ cl-pos) ':end2 (+ cl-pos cl-len)
			   ':from-end nil cl-keys))
	  (if cl-from-end (setq cl-end2 cl-pos) (setq cl-start2 (1+ cl-pos))))
	(and (< cl-start2 cl-end2) cl-pos)))))

(defun sort* (cl-seq cl-pred &rest cl-keys)
  "Sort the argument SEQUENCE according to PREDICATE.
This is a destructive function; it reuses the storage of SEQUENCE if possible.
Keywords supported:  :key"
  (if (nlistp cl-seq)
      (replace cl-seq (apply 'sort* (append cl-seq nil) cl-pred cl-keys))
    (cl-parsing-keywords (:key) ()
      (if (memq cl-key '(nil identity))
	  (sort cl-seq cl-pred)
	(sort cl-seq (function (lambda (cl-x cl-y)
				 (funcall cl-pred (funcall cl-key cl-x)
					  (funcall cl-key cl-y)))))))))

(defun stable-sort (cl-seq cl-pred &rest cl-keys)
  "Sort the argument SEQUENCE stably according to PREDICATE.
This is a destructive function; it reuses the storage of SEQUENCE if possible.
Keywords supported:  :key"
  (apply 'sort* cl-seq cl-pred cl-keys))

(defun merge (cl-type cl-seq1 cl-seq2 cl-pred &rest cl-keys)
  "Destructively merge the two sequences to produce a new sequence.
TYPE is the sequence type to return, SEQ1 and SEQ2 are the two
argument sequences, and PRED is a `less-than' predicate on the elements.
Keywords supported:  :key"
  (or (listp cl-seq1) (setq cl-seq1 (append cl-seq1 nil)))
  (or (listp cl-seq2) (setq cl-seq2 (append cl-seq2 nil)))
  (cl-parsing-keywords (:key) ()
    (let ((cl-res nil))
      (while (and cl-seq1 cl-seq2)
	(if (funcall cl-pred (cl-check-key (car cl-seq2))
		     (cl-check-key (car cl-seq1)))
	    (cl-push (cl-pop cl-seq2) cl-res)
	  (cl-push (cl-pop cl-seq1) cl-res)))
      (coerce (nconc (nreverse cl-res) cl-seq1 cl-seq2) cl-type))))

;;; See compiler macro in cl-macs.el
(defun member* (cl-item cl-list &rest cl-keys)
  "Find the first occurrence of ITEM in LIST.
Return the sublist of LIST whose car is ITEM.
Keywords supported:  :test :test-not :key"
  (if cl-keys
      (cl-parsing-keywords (:test :test-not :key :if :if-not) ()
	(while (and cl-list (not (cl-check-test cl-item (car cl-list))))
	  (setq cl-list (cdr cl-list)))
	cl-list)
    (if (and (numberp cl-item) (not (integerp cl-item)))
	(member cl-item cl-list)
      (memq cl-item cl-list))))

(defun member-if (cl-pred cl-list &rest cl-keys)
  "Find the first item satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches.
Keywords supported:  :key"
  (apply 'member* nil cl-list ':if cl-pred cl-keys))

(defun member-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item not satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches.
Keywords supported:  :key"
  (apply 'member* nil cl-list ':if-not cl-pred cl-keys))

(defun cl-adjoin (cl-item cl-list &rest cl-keys)
  (if (cl-parsing-keywords (:key) t
	(apply 'member* (cl-check-key cl-item) cl-list cl-keys))
      cl-list
    (cons cl-item cl-list)))

;;; See compiler macro in cl-macs.el
(defun assoc* (cl-item cl-alist &rest cl-keys)
  "Find the first item whose car matches ITEM in LIST.
Keywords supported:  :test :test-not :key"
  (if cl-keys
      (cl-parsing-keywords (:test :test-not :key :if :if-not) ()
	(while (and cl-alist
		    (or (not (consp (car cl-alist)))
			(not (cl-check-test cl-item (car (car cl-alist))))))
	  (setq cl-alist (cdr cl-alist)))
	(and cl-alist (car cl-alist)))
    (if (and (numberp cl-item) (not (integerp cl-item)))
	(assoc cl-item cl-alist)
      (assq cl-item cl-alist))))

(defun assoc-if (cl-pred cl-list &rest cl-keys)
  "Find the first item whose car satisfies PREDICATE in LIST.
Keywords supported:  :key"
  (apply 'assoc* nil cl-list ':if cl-pred cl-keys))

(defun assoc-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item whose car does not satisfy PREDICATE in LIST.
Keywords supported:  :key"
  (apply 'assoc* nil cl-list ':if-not cl-pred cl-keys))

(defun rassoc* (cl-item cl-alist &rest cl-keys)
  "Find the first item whose cdr matches ITEM in LIST.
Keywords supported:  :test :test-not :key"
  (if (or cl-keys (numberp cl-item))
      (cl-parsing-keywords (:test :test-not :key :if :if-not) ()
	(while (and cl-alist
		    (or (not (consp (car cl-alist)))
			(not (cl-check-test cl-item (cdr (car cl-alist))))))
	  (setq cl-alist (cdr cl-alist)))
	(and cl-alist (car cl-alist)))
    (rassq cl-item cl-alist)))

(defun rassoc (item alist) (rassoc* item alist ':test 'equal))

(defun rassoc-if (cl-pred cl-list &rest cl-keys)
  "Find the first item whose cdr satisfies PREDICATE in LIST.
Keywords supported:  :key"
  (apply 'rassoc* nil cl-list ':if cl-pred cl-keys))

(defun rassoc-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item whose cdr does not satisfy PREDICATE in LIST.
Keywords supported:  :key"
  (apply 'rassoc* nil cl-list ':if-not cl-pred cl-keys))

(defun union (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
Keywords supported:  :test :test-not :key"
  (cond ((null cl-list1) cl-list2) ((null cl-list2) cl-list1)
	((equal cl-list1 cl-list2) cl-list1)
	(t
	 (or (>= (length cl-list1) (length cl-list2))
	     (setq cl-list1 (prog1 cl-list2 (setq cl-list2 cl-list1))))
	 (while cl-list2
	   (if (or cl-keys (numberp (car cl-list2)))
	       (setq cl-list1 (apply 'adjoin (car cl-list2) cl-list1 cl-keys))
	     (or (memq (car cl-list2) cl-list1)
		 (cl-push (car cl-list2) cl-list1)))
	   (cl-pop cl-list2))
	 cl-list1)))

(defun nunion (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-union operation.
The result list contains all items that appear in either LIST1 or LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.
Keywords supported:  :test :test-not :key"
  (cond ((null cl-list1) cl-list2) ((null cl-list2) cl-list1)
	(t (apply 'union cl-list1 cl-list2 cl-keys))))

(defun intersection (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-intersection operation.
The result list contains all items that appear in both LIST1 and LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
Keywords supported:  :test :test-not :key"
  (and cl-list1 cl-list2
       (if (equal cl-list1 cl-list2) cl-list1
	 (cl-parsing-keywords (:key) (:test :test-not)
	   (let ((cl-res nil))
	     (or (>= (length cl-list1) (length cl-list2))
		 (setq cl-list1 (prog1 cl-list2 (setq cl-list2 cl-list1))))
	     (while cl-list2
	       (if (if (or cl-keys (numberp (car cl-list2)))
		       (apply 'member* (cl-check-key (car cl-list2))
			      cl-list1 cl-keys)
		     (memq (car cl-list2) cl-list1))
		   (cl-push (car cl-list2) cl-res))
	       (cl-pop cl-list2))
	     cl-res)))))

(defun nintersection (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-intersection operation.
The result list contains all items that appear in both LIST1 and LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.
Keywords supported:  :test :test-not :key"
  (and cl-list1 cl-list2 (apply 'intersection cl-list1 cl-list2 cl-keys)))

(defun set-difference (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-difference operation.
The result list contains all items that appear in LIST1 but not LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
Keywords supported:  :test :test-not :key"
  (if (or (null cl-list1) (null cl-list2)) cl-list1
    (cl-parsing-keywords (:key) (:test :test-not)
      (let ((cl-res nil))
	(while cl-list1
	  (or (if (or cl-keys (numberp (car cl-list1)))
		  (apply 'member* (cl-check-key (car cl-list1))
			 cl-list2 cl-keys)
		(memq (car cl-list1) cl-list2))
	      (cl-push (car cl-list1) cl-res))
	  (cl-pop cl-list1))
	cl-res))))

(defun nset-difference (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-difference operation.
The result list contains all items that appear in LIST1 but not LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.
Keywords supported:  :test :test-not :key"
  (if (or (null cl-list1) (null cl-list2)) cl-list1
    (apply 'set-difference cl-list1 cl-list2 cl-keys)))

(defun set-exclusive-or (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-exclusive-or operation.
The result list contains all items that appear in exactly one of LIST1, LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
Keywords supported:  :test :test-not :key"
  (cond ((null cl-list1) cl-list2) ((null cl-list2) cl-list1)
	((equal cl-list1 cl-list2) nil)
	(t (append (apply 'set-difference cl-list1 cl-list2 cl-keys)
		   (apply 'set-difference cl-list2 cl-list1 cl-keys)))))

(defun nset-exclusive-or (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-exclusive-or operation.
The result list contains all items that appear in exactly one of LIST1, LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.
Keywords supported:  :test :test-not :key"
  (cond ((null cl-list1) cl-list2) ((null cl-list2) cl-list1)
	((equal cl-list1 cl-list2) nil)
	(t (nconc (apply 'nset-difference cl-list1 cl-list2 cl-keys)
		  (apply 'nset-difference cl-list2 cl-list1 cl-keys)))))

(defun subsetp (cl-list1 cl-list2 &rest cl-keys)
  "True if LIST1 is a subset of LIST2.
I.e., if every element of LIST1 also appears in LIST2.
Keywords supported:  :test :test-not :key"
  (cond ((null cl-list1) t) ((null cl-list2) nil)
	((equal cl-list1 cl-list2) t)
	(t (cl-parsing-keywords (:key) (:test :test-not)
	     (while (and cl-list1
			 (apply 'member* (cl-check-key (car cl-list1))
				cl-list2 cl-keys))
	       (cl-pop cl-list1))
	     (null cl-list1)))))

(defun subst-if (cl-new cl-pred cl-tree &rest cl-keys)
  "Substitute NEW for elements matching PREDICATE in TREE (non-destructively).
Return a copy of TREE with all matching elements replaced by NEW.
Keywords supported:  :key"
  (apply 'sublis (list (cons nil cl-new)) cl-tree ':if cl-pred cl-keys))

(defun subst-if-not (cl-new cl-pred cl-tree &rest cl-keys)
  "Substitute NEW for elts not matching PREDICATE in TREE (non-destructively).
Return a copy of TREE with all non-matching elements replaced by NEW.
Keywords supported:  :key"
  (apply 'sublis (list (cons nil cl-new)) cl-tree ':if-not cl-pred cl-keys))

(defun nsubst (cl-new cl-old cl-tree &rest cl-keys)
  "Substitute NEW for OLD everywhere in TREE (destructively).
Any element of TREE which is `eql' to OLD is changed to NEW (via a call
to `setcar').
Keywords supported:  :test :test-not :key"
  (apply 'nsublis (list (cons cl-old cl-new)) cl-tree cl-keys))

(defun nsubst-if (cl-new cl-pred cl-tree &rest cl-keys)
  "Substitute NEW for elements matching PREDICATE in TREE (destructively).
Any element of TREE which matches is changed to NEW (via a call to `setcar').
Keywords supported:  :key"
  (apply 'nsublis (list (cons nil cl-new)) cl-tree ':if cl-pred cl-keys))

(defun nsubst-if-not (cl-new cl-pred cl-tree &rest cl-keys)
  "Substitute NEW for elements not matching PREDICATE in TREE (destructively).
Any element of TREE which matches is changed to NEW (via a call to `setcar').
Keywords supported:  :key"
  (apply 'nsublis (list (cons nil cl-new)) cl-tree ':if-not cl-pred cl-keys))

(defun sublis (cl-alist cl-tree &rest cl-keys)
  "Perform substitutions indicated by ALIST in TREE (non-destructively).
Return a copy of TREE with all matching elements replaced.
Keywords supported:  :test :test-not :key"
  (cl-parsing-keywords (:test :test-not :key :if :if-not) ()
    (cl-sublis-rec cl-tree)))

(defvar cl-alist)
(defun cl-sublis-rec (cl-tree)   ; uses cl-alist/key/test*/if*
  (let ((cl-temp (cl-check-key cl-tree)) (cl-p cl-alist))
    (while (and cl-p (not (cl-check-test-nokey (car (car cl-p)) cl-temp)))
      (setq cl-p (cdr cl-p)))
    (if cl-p (cdr (car cl-p))
      (if (consp cl-tree)
	  (let ((cl-a (cl-sublis-rec (car cl-tree)))
		(cl-d (cl-sublis-rec (cdr cl-tree))))
	    (if (and (eq cl-a (car cl-tree)) (eq cl-d (cdr cl-tree)))
		cl-tree
	      (cons cl-a cl-d)))
	cl-tree))))

(defun nsublis (cl-alist cl-tree &rest cl-keys)
  "Perform substitutions indicated by ALIST in TREE (destructively).
Any matching element of TREE is changed via a call to `setcar'.
Keywords supported:  :test :test-not :key"
  (cl-parsing-keywords (:test :test-not :key :if :if-not) ()
    (let ((cl-hold (list cl-tree)))
      (cl-nsublis-rec cl-hold)
      (car cl-hold))))

(defun cl-nsublis-rec (cl-tree)   ; uses cl-alist/temp/p/key/test*/if*
  (while (consp cl-tree)
    (let ((cl-temp (cl-check-key (car cl-tree))) (cl-p cl-alist))
      (while (and cl-p (not (cl-check-test-nokey (car (car cl-p)) cl-temp)))
	(setq cl-p (cdr cl-p)))
      (if cl-p (setcar cl-tree (cdr (car cl-p)))
	(if (consp (car cl-tree)) (cl-nsublis-rec (car cl-tree))))
      (setq cl-temp (cl-check-key (cdr cl-tree)) cl-p cl-alist)
      (while (and cl-p (not (cl-check-test-nokey (car (car cl-p)) cl-temp)))
	(setq cl-p (cdr cl-p)))
      (if cl-p
	  (progn (setcdr cl-tree (cdr (car cl-p))) (setq cl-tree nil))
	(setq cl-tree (cdr cl-tree))))))

(defun tree-equal (cl-x cl-y &rest cl-keys)
  "T if trees X and Y have `eql' leaves.
Atoms are compared by `eql'; cons cells are compared recursively.
Keywords supported:  :test :test-not :key"
  (cl-parsing-keywords (:test :test-not :key) ()
    (cl-tree-equal-rec cl-x cl-y)))

(defun cl-tree-equal-rec (cl-x cl-y)
  (while (and (consp cl-x) (consp cl-y)
	      (cl-tree-equal-rec (car cl-x) (car cl-y)))
    (setq cl-x (cdr cl-x) cl-y (cdr cl-y)))
  (and (not (consp cl-x)) (not (consp cl-y)) (cl-check-match cl-x cl-y)))


(run-hooks 'cl-seq-load-hook)

;;; cl-seq.el ends here
