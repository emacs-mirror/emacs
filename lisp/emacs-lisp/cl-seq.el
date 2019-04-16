;;; cl-seq.el --- Common Lisp features, part 3  -*- lexical-binding: t -*-

;; Copyright (C) 1993, 2001-2019 Free Software Foundation, Inc.

;; Author: Dave Gillespie <daveg@synaptics.com>
;; Old-Version: 2.02
;; Keywords: extensions
;; Package: emacs

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

;; These are extensions to Emacs Lisp that provide a degree of
;; Common Lisp compatibility, beyond what is already built-in
;; in Emacs Lisp.
;;
;; This package was written by Dave Gillespie; it is a complete
;; rewrite of Cesar Quiroz's original cl.el package of December 1986.
;;
;; Bug reports, comments, and suggestions are welcome!

;; This file contains the Common Lisp sequence and list functions
;; which take keyword arguments.

;; See cl.el for Change Log.


;;; Code:

(require 'cl-lib)

;; Keyword parsing.
;; This is special-cased here so that we can compile
;; this file independent from cl-macs.

(defmacro cl--parsing-keywords (kwords other-keys &rest body)
  (declare (indent 2) (debug (sexp sexp &rest form)))
  `(let* ,(mapcar
           (lambda (x)
             (let* ((var (if (consp x) (car x) x))
                    (mem `(car (cdr (memq ',var cl-keys)))))
               (if (eq var :test-not)
                   (setq mem `(and ,mem (setq cl-test ,mem) t)))
               (if (eq var :if-not)
                   (setq mem `(and ,mem (setq cl-if ,mem) t)))
               (list (intern
                      (format "cl-%s" (substring (symbol-name var) 1)))
                     (if (consp x) `(or ,mem ,(car (cdr x))) mem))))
           kwords)
     ,@(append
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
        body)))

(defmacro cl--check-key (x)     ;Expects `cl-key' in context of generated code.
  (declare (debug edebug-forms))
  `(if cl-key (funcall cl-key ,x) ,x))

(defmacro cl--check-test-nokey (item x) ;cl-test cl-if cl-test-not cl-if-not.
  (declare (debug edebug-forms))
  `(cond
    (cl-test (eq (not (funcall cl-test ,item ,x))
                 cl-test-not))
    (cl-if (eq (not (funcall cl-if ,x)) cl-if-not))
    (t (eql ,item ,x))))

(defmacro cl--check-test (item x)       ;all of the above.
  (declare (debug edebug-forms))
  `(cl--check-test-nokey ,item (cl--check-key ,x)))

(defmacro cl--check-match (x y)         ;cl-key cl-test cl-test-not
  (declare (debug edebug-forms))
  (setq x `(cl--check-key ,x) y `(cl--check-key ,y))
  `(if cl-test
       (eq (not (funcall cl-test ,x ,y)) cl-test-not)
     (eql ,x ,y)))

;; Yuck!  These vars are set/bound by cl--parsing-keywords to match :if :test
;; and :key keyword args, and they are also accessed (sometimes) via dynamic
;; scoping (and some of those accesses are from macro-expanded code).
(defvar cl-test) (defvar cl-test-not)
(defvar cl-if) (defvar cl-if-not)
(defvar cl-key)

;;;###autoload
(defun cl-endp (x)
  "Return true if X is the empty list; false if it is a cons.
Signal an error if X is not a list."
  (cl-check-type x list)
  (null x))

;;;###autoload
(defun cl-reduce (cl-func cl-seq &rest cl-keys)
  "Reduce two-argument FUNCTION across SEQ.
\nKeywords supported:  :start :end :from-end :initial-value :key

Return the result of calling FUNCTION with the first and the
second element of SEQ, then calling FUNCTION with that result and
the third element of SEQ, then with that result and the fourth
element of SEQ, etc.

If :INITIAL-VALUE is specified, it is added to the front of SEQ.
If SEQ is empty, return :INITIAL-VALUE and FUNCTION is not
called.

\n(fn FUNCTION SEQ [KEYWORD VALUE]...)"
  (cl--parsing-keywords (:from-end (:start 0) :end :initial-value :key) ()
    (or (listp cl-seq) (setq cl-seq (append cl-seq nil)))
    (setq cl-seq (cl-subseq cl-seq cl-start cl-end))
    (if cl-from-end (setq cl-seq (nreverse cl-seq)))
    (let ((cl-accum (cond ((memq :initial-value cl-keys) cl-initial-value)
			  (cl-seq (cl--check-key (pop cl-seq)))
			  (t (funcall cl-func)))))
      (if cl-from-end
	  (while cl-seq
	    (setq cl-accum (funcall cl-func (cl--check-key (pop cl-seq))
				    cl-accum)))
	(while cl-seq
	  (setq cl-accum (funcall cl-func cl-accum
				  (cl--check-key (pop cl-seq))))))
      cl-accum)))

;;;###autoload
(defun cl-fill (cl-seq cl-item &rest cl-keys)
  "Fill the elements of SEQ with ITEM.
\nKeywords supported:  :start :end
\n(fn SEQ ITEM [KEYWORD VALUE]...)"
  (cl--parsing-keywords ((:start 0) :end) ()
    (if (listp cl-seq)
	(let ((p (nthcdr cl-start cl-seq))
	      (n (and cl-end (- cl-end cl-start))))
	  (while (and p (or (null n) (>= (cl-decf n) 0)))
	    (setcar p cl-item)
	    (setq p (cdr p))))
      (or cl-end (setq cl-end (length cl-seq)))
      (if (and (= cl-start 0) (= cl-end (length cl-seq)))
	  (fillarray cl-seq cl-item)
	(while (< cl-start cl-end)
	  (aset cl-seq cl-start cl-item)
	  (setq cl-start (1+ cl-start)))))
    cl-seq))

;;;###autoload
(defun cl-replace (cl-seq1 cl-seq2 &rest cl-keys)
  "Replace the elements of SEQ1 with the elements of SEQ2.
SEQ1 is destructively modified, then returned.
\nKeywords supported:  :start1 :end1 :start2 :end2
\n(fn SEQ1 SEQ2 [KEYWORD VALUE]...)"
  (cl--parsing-keywords ((:start1 0) :end1 (:start2 0) :end2) ()
    (if (and (eq cl-seq1 cl-seq2) (<= cl-start2 cl-start1))
	(or (= cl-start1 cl-start2)
	    (let* ((cl-len (length cl-seq1))
		   (cl-n (min (- (or cl-end1 cl-len) cl-start1)
			      (- (or cl-end2 cl-len) cl-start2))))
	      (while (>= (setq cl-n (1- cl-n)) 0)
		(setf (elt cl-seq1 (+ cl-start1 cl-n))
			    (elt cl-seq2 (+ cl-start2 cl-n))))))
      (if (listp cl-seq1)
	  (let ((cl-p1 (nthcdr cl-start1 cl-seq1))
		(cl-n1 (and cl-end1 (- cl-end1 cl-start1))))
	    (if (listp cl-seq2)
		(let ((cl-p2 (nthcdr cl-start2 cl-seq2))
		      (cl-n (cond ((and cl-n1 cl-end2)
				   (min cl-n1 (- cl-end2 cl-start2)))
				  ((and cl-n1 (null cl-end2)) cl-n1)
				  ((and (null cl-n1) cl-end2) (- cl-end2 cl-start2)))))
		  (while (and cl-p1 cl-p2 (or (null cl-n) (>= (cl-decf cl-n) 0)))
		    (setcar cl-p1 (car cl-p2))
		    (setq cl-p1 (cdr cl-p1) cl-p2 (cdr cl-p2))))
	      (setq cl-end2 (if (null cl-n1)
				(or cl-end2 (length cl-seq2))
			      (min (or cl-end2 (length cl-seq2))
				   (+ cl-start2 cl-n1))))
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

;;;###autoload
(defun cl-remove (cl-item cl-seq &rest cl-keys)
  "Remove all occurrences of ITEM in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :test :test-not :key :count :start :end :from-end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (cl--parsing-keywords (:test :test-not :key :if :if-not :count :from-end
			(:start 0) :end) ()
    (let ((len (length cl-seq)))
      (if (<= (or cl-count (setq cl-count len)) 0)
	cl-seq
        (if (or (nlistp cl-seq) (and cl-from-end (< cl-count (/ len 2))))
	  (let ((cl-i (cl--position cl-item cl-seq cl-start cl-end
                                    cl-from-end)))
	    (if cl-i
		(let ((cl-res (apply 'cl-delete cl-item (append cl-seq nil)
				     (append (if cl-from-end
						 (list :end (1+ cl-i))
					       (list :start cl-i))
					     cl-keys))))
		  (if (listp cl-seq) cl-res
		    (if (stringp cl-seq) (concat cl-res) (vconcat cl-res))))
	      cl-seq))
	  (setq cl-end (- (or cl-end len) cl-start))
	(if (= cl-start 0)
	    (while (and cl-seq (> cl-end 0)
			(cl--check-test cl-item (car cl-seq))
			(setq cl-end (1- cl-end) cl-seq (cdr cl-seq))
			(> (setq cl-count (1- cl-count)) 0))))
	(if (and (> cl-count 0) (> cl-end 0))
	    (let ((cl-p (if (> cl-start 0) (nthcdr cl-start cl-seq)
			  (setq cl-end (1- cl-end)) (cdr cl-seq))))
	      (while (and cl-p (> cl-end 0)
			  (not (cl--check-test cl-item (car cl-p))))
		(setq cl-p (cdr cl-p) cl-end (1- cl-end)))
	      (if (and cl-p (> cl-end 0))
		  (nconc (cl-ldiff cl-seq cl-p)
			 (if (= cl-count 1) (cdr cl-p)
			   (and (cdr cl-p)
				(apply 'cl-delete cl-item
				       (copy-sequence (cdr cl-p))
				       :start 0 :end (1- cl-end)
				       :count (1- cl-count) cl-keys))))
		cl-seq))
	  cl-seq))))))

;;;###autoload
(defun cl-remove-if (cl-pred cl-list &rest cl-keys)
  "Remove all items satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl-remove nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl-remove-if-not (cl-pred cl-list &rest cl-keys)
  "Remove all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl-remove nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl-delete (cl-item cl-seq &rest cl-keys)
  "Remove all occurrences of ITEM in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :test :test-not :key :count :start :end :from-end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (cl--parsing-keywords (:test :test-not :key :if :if-not :count :from-end
			(:start 0) :end) ()
    (let ((len (length cl-seq)))
      (if (<= (or cl-count (setq cl-count len)) 0)
	cl-seq
      (if (listp cl-seq)
	  (if (and cl-from-end (< cl-count (/ len 2)))
	      (let (cl-i)
		(while (and (>= (setq cl-count (1- cl-count)) 0)
			    (setq cl-i (cl--position cl-item cl-seq cl-start
						     cl-end cl-from-end)))
		  (if (= cl-i 0) (setq cl-seq (cdr cl-seq))
		    (let ((cl-tail (nthcdr (1- cl-i) cl-seq)))
		      (setcdr cl-tail (cdr (cdr cl-tail)))))
		  (setq cl-end cl-i))
		cl-seq)
	    (setq cl-end (- (or cl-end len) cl-start))
	    (if (= cl-start 0)
		(progn
		  (while (and cl-seq
			      (> cl-end 0)
			      (cl--check-test cl-item (car cl-seq))
			      (setq cl-end (1- cl-end) cl-seq (cdr cl-seq))
			      (> (setq cl-count (1- cl-count)) 0)))
		  (setq cl-end (1- cl-end)))
	      (setq cl-start (1- cl-start)))
	    (if (and (> cl-count 0) (> cl-end 0))
		(let ((cl-p (nthcdr cl-start cl-seq)))
		  (while (and (cdr cl-p) (> cl-end 0))
		    (if (cl--check-test cl-item (car (cdr cl-p)))
			(progn
			  (setcdr cl-p (cdr (cdr cl-p)))
			  (if (= (setq cl-count (1- cl-count)) 0)
			      (setq cl-end 1)))
		      (setq cl-p (cdr cl-p)))
		    (setq cl-end (1- cl-end)))))
	    cl-seq)
	(apply 'cl-remove cl-item cl-seq cl-keys))))))

;;;###autoload
(defun cl-delete-if (cl-pred cl-list &rest cl-keys)
  "Remove all items satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl-delete nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl-delete-if-not (cl-pred cl-list &rest cl-keys)
  "Remove all items not satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl-delete nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl-remove-duplicates (cl-seq &rest cl-keys)
  "Return a copy of SEQ with all duplicate elements removed.
\nKeywords supported:  :test :test-not :key :start :end :from-end
\n(fn SEQ [KEYWORD VALUE]...)"
  (cl--delete-duplicates cl-seq cl-keys t))

;;;###autoload
(defun cl-delete-duplicates (cl-seq &rest cl-keys)
  "Remove all duplicate elements from SEQ (destructively).
\nKeywords supported:  :test :test-not :key :start :end :from-end
\n(fn SEQ [KEYWORD VALUE]...)"
  (cl--delete-duplicates cl-seq cl-keys nil))

(defun cl--delete-duplicates (cl-seq cl-keys cl-copy)
  (if (listp cl-seq)
      (cl--parsing-keywords
          ;; We need to parse :if, otherwise `cl-if' is unbound.
          (:test :test-not :key (:start 0) :end :from-end :if)
	  ()
	(if cl-from-end
	    (let ((cl-p (nthcdr cl-start cl-seq)) cl-i)
	      (setq cl-end (- (or cl-end (length cl-seq)) cl-start))
	      (while (> cl-end 1)
		(setq cl-i 0)
		(while (setq cl-i (cl--position (cl--check-key (car cl-p))
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
		      (cl--position (cl--check-key (car cl-seq))
                                    (cdr cl-seq) 0 (1- cl-end)))
	    (setq cl-seq (cdr cl-seq) cl-end (1- cl-end)))
	  (let ((cl-p (if (> cl-start 0) (nthcdr (1- cl-start) cl-seq)
			(setq cl-end (1- cl-end) cl-start 1) cl-seq)))
	    (while (and (cdr (cdr cl-p)) (> cl-end 1))
	      (if (cl--position (cl--check-key (car (cdr cl-p)))
                                (cdr (cdr cl-p)) 0 (1- cl-end))
		  (progn
		    (if cl-copy (setq cl-seq (copy-sequence cl-seq)
				      cl-p (nthcdr (1- cl-start) cl-seq)
				      cl-copy nil))
		    (setcdr cl-p (cdr (cdr cl-p))))
		(setq cl-p (cdr cl-p)))
	      (setq cl-end (1- cl-end) cl-start (1+ cl-start)))
	    cl-seq)))
    (let ((cl-res (cl--delete-duplicates (append cl-seq nil) cl-keys nil)))
      (if (stringp cl-seq) (concat cl-res) (vconcat cl-res)))))

;;;###autoload
(defun cl-substitute (cl-new cl-old cl-seq &rest cl-keys)
  "Substitute NEW for OLD in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :test :test-not :key :count :start :end :from-end
\n(fn NEW OLD SEQ [KEYWORD VALUE]...)"
  (cl--parsing-keywords (:test :test-not :key :if :if-not :count
			(:start 0) :end :from-end) ()
    (if (or (eq cl-old cl-new)
	    (<= (or cl-count (setq cl-from-end nil
				   cl-count (length cl-seq))) 0))
	cl-seq
      (let ((cl-i (cl--position cl-old cl-seq cl-start cl-end)))
	(if (not cl-i)
	    cl-seq
	  (setq cl-seq (copy-sequence cl-seq))
	  (unless cl-from-end
	    (setf (elt cl-seq cl-i) cl-new)
	    (cl-incf cl-i)
	    (cl-decf cl-count))
	  (apply 'cl-nsubstitute cl-new cl-old cl-seq :count cl-count
		 :start cl-i cl-keys))))))

;;;###autoload
(defun cl-substitute-if (cl-new cl-pred cl-list &rest cl-keys)
  "Substitute NEW for all items satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl-substitute cl-new nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl-substitute-if-not (cl-new cl-pred cl-list &rest cl-keys)
  "Substitute NEW for all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl-substitute cl-new nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl-nsubstitute (cl-new cl-old cl-seq &rest cl-keys)
  "Substitute NEW for OLD in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :test :test-not :key :count :start :end :from-end
\n(fn NEW OLD SEQ [KEYWORD VALUE]...)"
  (cl--parsing-keywords (:test :test-not :key :if :if-not :count
			(:start 0) :end :from-end) ()
    (let ((len (length cl-seq)))
      (or (eq cl-old cl-new) (<= (or cl-count (setq cl-count len)) 0)
	  (if (and (listp cl-seq) (or (not cl-from-end) (> cl-count (/ len 2))))
	    (let ((cl-p (nthcdr cl-start cl-seq)))
	      (setq cl-end (- (or cl-end len) cl-start))
	      (while (and cl-p (> cl-end 0) (> cl-count 0))
		(if (cl--check-test cl-old (car cl-p))
		    (progn
		      (setcar cl-p cl-new)
		      (setq cl-count (1- cl-count))))
		(setq cl-p (cdr cl-p) cl-end (1- cl-end))))
	    (or cl-end (setq cl-end len))
	  (if cl-from-end
	      (while (and (< cl-start cl-end) (> cl-count 0))
		(setq cl-end (1- cl-end))
		(if (cl--check-test cl-old (elt cl-seq cl-end))
		    (progn
		      (setf (elt cl-seq cl-end) cl-new)
		      (setq cl-count (1- cl-count)))))
	    (while (and (< cl-start cl-end) (> cl-count 0))
	      (if (cl--check-test cl-old (aref cl-seq cl-start))
		  (progn
		    (aset cl-seq cl-start cl-new)
		    (setq cl-count (1- cl-count))))
	      (setq cl-start (1+ cl-start)))))))
    cl-seq))

;;;###autoload
(defun cl-nsubstitute-if (cl-new cl-pred cl-list &rest cl-keys)
  "Substitute NEW for all items satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl-nsubstitute cl-new nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl-nsubstitute-if-not (cl-new cl-pred cl-list &rest cl-keys)
  "Substitute NEW for all items not satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl-nsubstitute cl-new nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl-find (cl-item cl-seq &rest cl-keys)
  "Find the first occurrence of ITEM in SEQ.
Return the matching ITEM, or nil if not found.
\nKeywords supported:  :test :test-not :key :start :end :from-end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (let ((cl-pos (apply 'cl-position cl-item cl-seq cl-keys)))
    (and cl-pos (elt cl-seq cl-pos))))

;;;###autoload
(defun cl-find-if (cl-pred cl-list &rest cl-keys)
  "Find the first item satisfying PREDICATE in SEQ.
Return the matching item, or nil if not found.
\nKeywords supported:  :key :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl-find nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl-find-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item not satisfying PREDICATE in SEQ.
Return the matching item, or nil if not found.
\nKeywords supported:  :key :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl-find nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl-position (cl-item cl-seq &rest cl-keys)
  "Find the first occurrence of ITEM in SEQ.
Return the index of the matching item, or nil if not found.
\nKeywords supported:  :test :test-not :key :start :end :from-end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (cl--parsing-keywords (:test :test-not :key :if :if-not
			(:start 0) :end :from-end) ()
    (cl--position cl-item cl-seq cl-start cl-end cl-from-end)))

(defun cl--position (cl-item cl-seq cl-start &optional cl-end cl-from-end)
  (if (listp cl-seq)
      (let ((cl-p (nthcdr cl-start cl-seq))
	    cl-res)
	(while (and cl-p (or (null cl-end) (< cl-start cl-end)) (or (null cl-res) cl-from-end))
	    (if (cl--check-test cl-item (car cl-p))
		(setq cl-res cl-start))
	    (setq cl-p (cdr cl-p) cl-start (1+ cl-start)))
	cl-res)
    (or cl-end (setq cl-end (length cl-seq)))
    (if cl-from-end
	(progn
	  (while (and (>= (setq cl-end (1- cl-end)) cl-start)
		      (not (cl--check-test cl-item (aref cl-seq cl-end)))))
	  (and (>= cl-end cl-start) cl-end))
      (while (and (< cl-start cl-end)
		  (not (cl--check-test cl-item (aref cl-seq cl-start))))
	(setq cl-start (1+ cl-start)))
      (and (< cl-start cl-end) cl-start))))

;;;###autoload
(defun cl-position-if (cl-pred cl-list &rest cl-keys)
  "Find the first item satisfying PREDICATE in SEQ.
Return the index of the matching item, or nil if not found.
\nKeywords supported:  :key :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl-position nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl-position-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item not satisfying PREDICATE in SEQ.
Return the index of the matching item, or nil if not found.
\nKeywords supported:  :key :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl-position nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl-count (cl-item cl-seq &rest cl-keys)
  "Count the number of occurrences of ITEM in SEQ.
\nKeywords supported:  :test :test-not :key :start :end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (cl--parsing-keywords (:test :test-not :key :if :if-not (:start 0) :end) ()
    (let ((cl-count 0) cl-x)
      (or cl-end (setq cl-end (length cl-seq)))
      (if (consp cl-seq) (setq cl-seq (nthcdr cl-start cl-seq)))
      (while (< cl-start cl-end)
	(setq cl-x (if (consp cl-seq) (pop cl-seq) (aref cl-seq cl-start)))
	(if (cl--check-test cl-item cl-x) (setq cl-count (1+ cl-count)))
	(setq cl-start (1+ cl-start)))
      cl-count)))

;;;###autoload
(defun cl-count-if (cl-pred cl-list &rest cl-keys)
  "Count the number of items satisfying PREDICATE in SEQ.
\nKeywords supported:  :key :start :end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl-count nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl-count-if-not (cl-pred cl-list &rest cl-keys)
  "Count the number of items not satisfying PREDICATE in SEQ.
\nKeywords supported:  :key :start :end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (apply 'cl-count nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl-mismatch (cl-seq1 cl-seq2 &rest cl-keys)
  "Compare SEQ1 with SEQ2, return index of first mismatching element.
Return nil if the sequences match.  If one sequence is a prefix of the
other, the return value indicates the end of the shorter sequence.
\nKeywords supported:  :test :test-not :key :start1 :end1 :start2 :end2 :from-end
\n(fn SEQ1 SEQ2 [KEYWORD VALUE]...)"
  (cl--parsing-keywords (:test :test-not :key :from-end
			(:start1 0) :end1 (:start2 0) :end2) ()
    (or cl-end1 (setq cl-end1 (length cl-seq1)))
    (or cl-end2 (setq cl-end2 (length cl-seq2)))
    (if cl-from-end
	(progn
	  (while (and (< cl-start1 cl-end1) (< cl-start2 cl-end2)
		      (cl--check-match (elt cl-seq1 (1- cl-end1))
				      (elt cl-seq2 (1- cl-end2))))
	    (setq cl-end1 (1- cl-end1) cl-end2 (1- cl-end2)))
	  (and (or (< cl-start1 cl-end1) (< cl-start2 cl-end2))
	       (1- cl-end1)))
      (let ((cl-p1 (and (listp cl-seq1) (nthcdr cl-start1 cl-seq1)))
	    (cl-p2 (and (listp cl-seq2) (nthcdr cl-start2 cl-seq2))))
	(while (and (< cl-start1 cl-end1) (< cl-start2 cl-end2)
		    (cl--check-match (if cl-p1 (car cl-p1)
				      (aref cl-seq1 cl-start1))
				    (if cl-p2 (car cl-p2)
				      (aref cl-seq2 cl-start2))))
	  (setq cl-p1 (cdr cl-p1) cl-p2 (cdr cl-p2)
		cl-start1 (1+ cl-start1) cl-start2 (1+ cl-start2)))
	(and (or (< cl-start1 cl-end1) (< cl-start2 cl-end2))
	     cl-start1)))))

;;;###autoload
(defun cl-search (cl-seq1 cl-seq2 &rest cl-keys)
  "Search for SEQ1 as a subsequence of SEQ2.
Return the index of the leftmost element of the first match found;
return nil if there are no matches.
\nKeywords supported:  :test :test-not :key :start1 :end1 :start2 :end2 :from-end
\n(fn SEQ1 SEQ2 [KEYWORD VALUE]...)"
  (cl--parsing-keywords (:test :test-not :key :from-end
			(:start1 0) :end1 (:start2 0) :end2) ()
    (or cl-end1 (setq cl-end1 (length cl-seq1)))
    (or cl-end2 (setq cl-end2 (length cl-seq2)))
    (if (>= cl-start1 cl-end1)
	(if cl-from-end cl-end2 cl-start2)
      (let* ((cl-len (- cl-end1 cl-start1))
	     (cl-first (cl--check-key (elt cl-seq1 cl-start1)))
	     (cl-if nil) cl-pos)
	(setq cl-end2 (- cl-end2 (1- cl-len)))
	(while (and (< cl-start2 cl-end2)
		    (setq cl-pos (cl--position cl-first cl-seq2
                                               cl-start2 cl-end2 cl-from-end))
		    (apply 'cl-mismatch cl-seq1 cl-seq2
			   :start1 (1+ cl-start1) :end1 cl-end1
			   :start2 (1+ cl-pos) :end2 (+ cl-pos cl-len)
			   :from-end nil cl-keys))
	  (if cl-from-end (setq cl-end2 cl-pos) (setq cl-start2 (1+ cl-pos))))
	(and (< cl-start2 cl-end2) cl-pos)))))

;;;###autoload
(defun cl-sort (cl-seq cl-pred &rest cl-keys)
  "Sort the argument SEQ according to PREDICATE.
This is a destructive function; it reuses the storage of SEQ if possible.
\nKeywords supported:  :key
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  (if (nlistp cl-seq)
      (cl-replace cl-seq (apply 'cl-sort (append cl-seq nil) cl-pred cl-keys))
    (cl--parsing-keywords (:key) ()
      (if (memq cl-key '(nil identity))
	  (sort cl-seq cl-pred)
	(sort cl-seq (function (lambda (cl-x cl-y)
				 (funcall cl-pred (funcall cl-key cl-x)
					  (funcall cl-key cl-y)))))))))

;;;###autoload
(defun cl-stable-sort (cl-seq cl-pred &rest cl-keys)
  "Sort the argument SEQ stably according to PREDICATE.
This is a destructive function; it reuses the storage of SEQ if possible.
\nKeywords supported:  :key
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  (apply 'cl-sort cl-seq cl-pred cl-keys))

;;;###autoload
(defun cl-merge (cl-type cl-seq1 cl-seq2 cl-pred &rest cl-keys)
  "Destructively merge the two sequences to produce a new sequence.
TYPE is the sequence type to return, SEQ1 and SEQ2 are the two argument
sequences, and PREDICATE is a `less-than' predicate on the elements.
\nKeywords supported:  :key
\n(fn TYPE SEQ1 SEQ2 PREDICATE [KEYWORD VALUE]...)"
  (or (listp cl-seq1) (setq cl-seq1 (append cl-seq1 nil)))
  (or (listp cl-seq2) (setq cl-seq2 (append cl-seq2 nil)))
  (cl--parsing-keywords (:key) ()
    (let ((cl-res nil))
      (while (and cl-seq1 cl-seq2)
	(if (funcall cl-pred (cl--check-key (car cl-seq2))
		     (cl--check-key (car cl-seq1)))
	    (push (pop cl-seq2) cl-res)
	  (push (pop cl-seq1) cl-res)))
      (cl-coerce (nconc (nreverse cl-res) cl-seq1 cl-seq2) cl-type))))

;;;###autoload
(defun cl-member (cl-item cl-list &rest cl-keys)
  "Find the first occurrence of ITEM in LIST.
Return the sublist of LIST whose car is ITEM.
\nKeywords supported:  :test :test-not :key
\n(fn ITEM LIST [KEYWORD VALUE]...)"
  (declare (compiler-macro cl--compiler-macro-member))
  (if cl-keys
      (cl--parsing-keywords (:test :test-not :key :if :if-not) ()
	(while (and cl-list (not (cl--check-test cl-item (car cl-list))))
	  (setq cl-list (cdr cl-list)))
	cl-list)
    (if (and (numberp cl-item) (not (integerp cl-item)))
	(member cl-item cl-list)
      (memq cl-item cl-list))))
(autoload 'cl--compiler-macro-member "cl-macs")

;;;###autoload
(defun cl-member-if (cl-pred cl-list &rest cl-keys)
  "Find the first item satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (apply 'cl-member nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl-member-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item not satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (apply 'cl-member nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl--adjoin (cl-item cl-list &rest cl-keys)
  (if (cl--parsing-keywords (:key) t
	(apply 'cl-member (cl--check-key cl-item) cl-list cl-keys))
      cl-list
    (cons cl-item cl-list)))

;;;###autoload
(defun cl-assoc (cl-item cl-alist &rest cl-keys)
  "Find the first item whose car matches ITEM in LIST.
\nKeywords supported:  :test :test-not :key
\n(fn ITEM LIST [KEYWORD VALUE]...)"
  (declare (compiler-macro cl--compiler-macro-assoc))
  (if cl-keys
      (cl--parsing-keywords (:test :test-not :key :if :if-not) ()
	(while (and cl-alist
		    (or (not (consp (car cl-alist)))
			(not (cl--check-test cl-item (car (car cl-alist))))))
	  (setq cl-alist (cdr cl-alist)))
	(and cl-alist (car cl-alist)))
    (if (and (numberp cl-item) (not (integerp cl-item)))
	(assoc cl-item cl-alist)
      (assq cl-item cl-alist))))
(autoload 'cl--compiler-macro-assoc "cl-macs")

;;;###autoload
(defun cl-assoc-if (cl-pred cl-list &rest cl-keys)
  "Find the first item whose car satisfies PREDICATE in LIST.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (apply 'cl-assoc nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl-assoc-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item whose car does not satisfy PREDICATE in LIST.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (apply 'cl-assoc nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl-rassoc (cl-item cl-alist &rest cl-keys)
  "Find the first item whose cdr matches ITEM in LIST.
\nKeywords supported:  :test :test-not :key
\n(fn ITEM LIST [KEYWORD VALUE]...)"
  (if (or cl-keys (numberp cl-item))
      (cl--parsing-keywords (:test :test-not :key :if :if-not) ()
	(while (and cl-alist
		    (or (not (consp (car cl-alist)))
			(not (cl--check-test cl-item (cdr (car cl-alist))))))
	  (setq cl-alist (cdr cl-alist)))
	(and cl-alist (car cl-alist)))
    (rassq cl-item cl-alist)))

;;;###autoload
(defun cl-rassoc-if (cl-pred cl-list &rest cl-keys)
  "Find the first item whose cdr satisfies PREDICATE in LIST.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (apply 'cl-rassoc nil cl-list :if cl-pred cl-keys))

;;;###autoload
(defun cl-rassoc-if-not (cl-pred cl-list &rest cl-keys)
  "Find the first item whose cdr does not satisfy PREDICATE in LIST.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (apply 'cl-rassoc nil cl-list :if-not cl-pred cl-keys))

;;;###autoload
(defun cl-union (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-union operation.
The resulting list contains all items that appear in either LIST1 or LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (cond ((null cl-list1) cl-list2) ((null cl-list2) cl-list1)
	((and (not cl-keys) (equal cl-list1 cl-list2)) cl-list1)
	(t
	 (or (>= (length cl-list1) (length cl-list2))
	     (setq cl-list1 (prog1 cl-list2 (setq cl-list2 cl-list1))))
	 (while cl-list2
	   (if (or cl-keys (numberp (car cl-list2)))
	       (setq cl-list1
                     (apply 'cl-adjoin (car cl-list2) cl-list1 cl-keys))
	     (or (memq (car cl-list2) cl-list1)
		 (push (car cl-list2) cl-list1)))
	   (pop cl-list2))
	 cl-list1)))

;;;###autoload
(defun cl-nunion (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-union operation.
The resulting list contains all items that appear in either LIST1 or LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (cond ((null cl-list1) cl-list2) ((null cl-list2) cl-list1)
	(t (apply 'cl-union cl-list1 cl-list2 cl-keys))))

;;;###autoload
(defun cl-intersection (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-intersection operation.
The resulting list contains all items that appear in both LIST1 and LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (and cl-list1 cl-list2
       (if (equal cl-list1 cl-list2) cl-list1
	 (cl--parsing-keywords (:key) (:test :test-not)
	   (let ((cl-res nil))
	     (or (>= (length cl-list1) (length cl-list2))
		 (setq cl-list1 (prog1 cl-list2 (setq cl-list2 cl-list1))))
	     (while cl-list2
	       (if (if (or cl-keys (numberp (car cl-list2)))
		       (apply 'cl-member (cl--check-key (car cl-list2))
			      cl-list1 cl-keys)
		     (memq (car cl-list2) cl-list1))
		   (push (car cl-list2) cl-res))
	       (pop cl-list2))
	     cl-res)))))

;;;###autoload
(defun cl-nintersection (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-intersection operation.
The resulting list contains all items that appear in both LIST1 and LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (and cl-list1 cl-list2 (apply 'cl-intersection cl-list1 cl-list2 cl-keys)))

;;;###autoload
(defun cl-set-difference (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-difference operation.
The resulting list contains all items that appear in LIST1 but not LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (if (or (null cl-list1) (null cl-list2)) cl-list1
    (cl--parsing-keywords (:key) (:test :test-not)
      (let ((cl-res nil))
	(while cl-list1
	  (or (if (or cl-keys (numberp (car cl-list1)))
		  (apply 'cl-member (cl--check-key (car cl-list1))
			 cl-list2 cl-keys)
		(memq (car cl-list1) cl-list2))
	      (push (car cl-list1) cl-res))
	  (pop cl-list1))
        (nreverse cl-res)))))

;;;###autoload
(defun cl-nset-difference (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-difference operation.
The resulting list contains all items that appear in LIST1 but not LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (if (or (null cl-list1) (null cl-list2)) cl-list1
    (apply 'cl-set-difference cl-list1 cl-list2 cl-keys)))

;;;###autoload
(defun cl-set-exclusive-or (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-exclusive-or operation.
The resulting list contains all items appearing in exactly one of LIST1, LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (cond ((null cl-list1) cl-list2) ((null cl-list2) cl-list1)
	((equal cl-list1 cl-list2) nil)
	(t (append (apply 'cl-set-difference cl-list1 cl-list2 cl-keys)
		   (apply 'cl-set-difference cl-list2 cl-list1 cl-keys)))))

;;;###autoload
(defun cl-nset-exclusive-or (cl-list1 cl-list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-exclusive-or operation.
The resulting list contains all items appearing in exactly one of LIST1, LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (cond ((null cl-list1) cl-list2) ((null cl-list2) cl-list1)
	((equal cl-list1 cl-list2) nil)
	(t (nconc (apply 'cl-nset-difference cl-list1 cl-list2 cl-keys)
		  (apply 'cl-nset-difference cl-list2 cl-list1 cl-keys)))))

;;;###autoload
(defun cl-subsetp (cl-list1 cl-list2 &rest cl-keys)
  "Return true if LIST1 is a subset of LIST2.
I.e., if every element of LIST1 also appears in LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (cond ((null cl-list1) t) ((null cl-list2) nil)
	((equal cl-list1 cl-list2) t)
	(t (cl--parsing-keywords (:key) (:test :test-not)
	     (while (and cl-list1
			 (apply 'cl-member (cl--check-key (car cl-list1))
				cl-list2 cl-keys))
	       (pop cl-list1))
	     (null cl-list1)))))

;;;###autoload
(defun cl-subst-if (cl-new cl-pred cl-tree &rest cl-keys)
  "Substitute NEW for elements matching PREDICATE in TREE (non-destructively).
Return a copy of TREE with all matching elements replaced by NEW.
\nKeywords supported:  :key
\n(fn NEW PREDICATE TREE [KEYWORD VALUE]...)"
  (apply 'cl-sublis (list (cons nil cl-new)) cl-tree :if cl-pred cl-keys))

;;;###autoload
(defun cl-subst-if-not (cl-new cl-pred cl-tree &rest cl-keys)
  "Substitute NEW for elts not matching PREDICATE in TREE (non-destructively).
Return a copy of TREE with all non-matching elements replaced by NEW.
\nKeywords supported:  :key
\n(fn NEW PREDICATE TREE [KEYWORD VALUE]...)"
  (apply 'cl-sublis (list (cons nil cl-new)) cl-tree :if-not cl-pred cl-keys))

;;;###autoload
(defun cl-nsubst (cl-new cl-old cl-tree &rest cl-keys)
  "Substitute NEW for OLD everywhere in TREE (destructively).
Any element of TREE which is `eql' to OLD is changed to NEW (via a call
to `setcar').
\nKeywords supported:  :test :test-not :key
\n(fn NEW OLD TREE [KEYWORD VALUE]...)"
  (apply 'cl-nsublis (list (cons cl-old cl-new)) cl-tree cl-keys))

;;;###autoload
(defun cl-nsubst-if (cl-new cl-pred cl-tree &rest cl-keys)
  "Substitute NEW for elements matching PREDICATE in TREE (destructively).
Any element of TREE which matches is changed to NEW (via a call to `setcar').
\nKeywords supported:  :key
\n(fn NEW PREDICATE TREE [KEYWORD VALUE]...)"
  (apply 'cl-nsublis (list (cons nil cl-new)) cl-tree :if cl-pred cl-keys))

;;;###autoload
(defun cl-nsubst-if-not (cl-new cl-pred cl-tree &rest cl-keys)
  "Substitute NEW for elements not matching PREDICATE in TREE (destructively).
Any element of TREE which matches is changed to NEW (via a call to `setcar').
\nKeywords supported:  :key
\n(fn NEW PREDICATE TREE [KEYWORD VALUE]...)"
  (apply 'cl-nsublis (list (cons nil cl-new)) cl-tree :if-not cl-pred cl-keys))

(defvar cl--alist)

;;;###autoload
(defun cl-sublis (cl-alist cl-tree &rest cl-keys)
  "Perform substitutions indicated by ALIST in TREE (non-destructively).
Return a copy of TREE with all matching elements replaced.
\nKeywords supported:  :test :test-not :key
\n(fn ALIST TREE [KEYWORD VALUE]...)"
  (cl--parsing-keywords (:test :test-not :key :if :if-not) ()
    (let ((cl--alist cl-alist))
      (cl--sublis-rec cl-tree))))

(defun cl--sublis-rec (cl-tree)   ;Uses cl--alist cl-key/test*/if*.
  (let ((cl-temp (cl--check-key cl-tree)) (cl-p cl--alist))
    (while (and cl-p (not (cl--check-test-nokey (car (car cl-p)) cl-temp)))
      (setq cl-p (cdr cl-p)))
    (if cl-p (cdr (car cl-p))
      (if (consp cl-tree)
	  (let ((cl-a (cl--sublis-rec (car cl-tree)))
		(cl-d (cl--sublis-rec (cdr cl-tree))))
	    (if (and (eq cl-a (car cl-tree)) (eq cl-d (cdr cl-tree)))
		cl-tree
	      (cons cl-a cl-d)))
	cl-tree))))

;;;###autoload
(defun cl-nsublis (cl-alist cl-tree &rest cl-keys)
  "Perform substitutions indicated by ALIST in TREE (destructively).
Any matching element of TREE is changed via a call to `setcar'.
\nKeywords supported:  :test :test-not :key
\n(fn ALIST TREE [KEYWORD VALUE]...)"
  (cl--parsing-keywords (:test :test-not :key :if :if-not) ()
    (let ((cl-hold (list cl-tree))
          (cl--alist cl-alist))
      (cl--nsublis-rec cl-hold)
      (car cl-hold))))

(defun cl--nsublis-rec (cl-tree)   ;Uses cl--alist cl-key/test*/if*.
  (while (consp cl-tree)
    (let ((cl-temp (cl--check-key (car cl-tree))) (cl-p cl--alist))
      (while (and cl-p (not (cl--check-test-nokey (car (car cl-p)) cl-temp)))
	(setq cl-p (cdr cl-p)))
      (if cl-p (setcar cl-tree (cdr (car cl-p)))
	(if (consp (car cl-tree)) (cl--nsublis-rec (car cl-tree))))
      (setq cl-temp (cl--check-key (cdr cl-tree)) cl-p cl--alist)
      (while (and cl-p (not (cl--check-test-nokey (car (car cl-p)) cl-temp)))
	(setq cl-p (cdr cl-p)))
      (if cl-p
	  (progn (setcdr cl-tree (cdr (car cl-p))) (setq cl-tree nil))
	(setq cl-tree (cdr cl-tree))))))

;;;###autoload
(defun cl-tree-equal (cl-x cl-y &rest cl-keys)
  "Return t if trees TREE1 and TREE2 have `eql' leaves.
Atoms are compared by `eql'; cons cells are compared recursively.
\nKeywords supported:  :test :test-not :key
\n(fn TREE1 TREE2 [KEYWORD VALUE]...)"
  (cl--parsing-keywords (:test :test-not :key) ()
    (cl--tree-equal-rec cl-x cl-y)))

(defun cl--tree-equal-rec (cl-x cl-y)   ;Uses cl-key/test*.
  (while (and (consp cl-x) (consp cl-y)
	      (cl--tree-equal-rec (car cl-x) (car cl-y)))
    (setq cl-x (cdr cl-x) cl-y (cdr cl-y)))
  (and (not (consp cl-x)) (not (consp cl-y)) (cl--check-match cl-x cl-y)))


(run-hooks 'cl-seq-load-hook)

;; Local variables:
;; byte-compile-dynamic: t
;; generated-autoload-file: "cl-loaddefs.el"
;; End:

(provide 'cl-seq)

;;; cl-seq.el ends here
