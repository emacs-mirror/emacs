;;; cl-seq.el --- Common Lisp features, part 3  -*- lexical-binding: t -*-

;; Copyright (C) 1993, 2001-2026 Free Software Foundation, Inc.

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

(defmacro cl--parsing-keywords (keywords other-keys &rest body)
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
                     (if (consp x) `(or ,mem ,(cadr x)) mem))))
           keywords)
     ,@(append
        (and (not (eq other-keys t))
             `((let ((cl-keys-temp cl-keys))
                 (while cl-keys-temp
                   (or (memq (car cl-keys-temp)
                             (quote ,(mapcar
                                      (lambda (x)
                                        (if (consp x)
                                            (car x) x))
                                      (append keywords other-keys))))
                       (cadr (memq :allow-other-keys cl-keys))
                       (error "Bad keyword argument %s"
                              (car cl-keys-temp)))
                   (setq cl-keys-temp (cddr cl-keys-temp))))))
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
  (declare (side-effect-free t))
  (cl-check-type x list)
  (null x))

;;;###autoload
(defun cl-reduce (func seq &rest cl-keys)
  "Reduce two-argument FUNCTION across SEQ.
\nKeywords supported:  :start :end :from-end :initial-value :key

Return the result of calling FUNCTION with the first and the
second element of SEQ, then calling FUNCTION with that result and
the third element of SEQ, then with that result and the fourth
element of SEQ, etc.

If :INITIAL-VALUE is specified, it is logically added to the
front of SEQ (or the back if :FROM-END is non-nil).  If SEQ is
empty, return :INITIAL-VALUE and FUNCTION is not called.

If SEQ is empty and no :INITIAL-VALUE is specified, then return
the result of calling FUNCTION with zero arguments.  This is the
only case where FUNCTION is called with fewer than two arguments.

If SEQ contains exactly one element and no :INITIAL-VALUE is
specified, then just return that element without calling FUNCTION.

If :FROM-END is non-nil, the reduction occurs from the back of
the SEQ moving forward, and the order of arguments to the
FUNCTION is also reversed.

\n(fn FUNCTION SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cl--parsing-keywords (:from-end (:start 0) :end :initial-value :key) ()
    (or (listp seq) (setq seq (append seq nil)))
    (setq seq (cl-subseq seq cl-start cl-end))
    (if cl-from-end (setq seq (nreverse seq)))
    (let ((accum (cond ((memq :initial-value cl-keys) cl-initial-value)
                       (seq (cl--check-key (pop seq)))
                       (t (funcall func)))))
      (if cl-from-end
          (while seq
            (setq accum (funcall func (cl--check-key (pop seq))
                                 accum)))
        (while seq
          (setq accum (funcall func accum
                               (cl--check-key (pop seq))))))
      accum)))

;;;###autoload
(defun cl-fill (seq item &rest cl-keys)
  "Fill the elements of SEQ with ITEM.
\nKeywords supported:  :start :end
\n(fn SEQ ITEM [KEYWORD VALUE]...)"
  (cl--parsing-keywords ((:start 0) :end) ()
    (if (listp seq)
        (let ((p (nthcdr cl-start seq))
	      (n (and cl-end (- cl-end cl-start))))
          (while (and p (or (null n) (>= (decf n) 0)))
            (setcar p item)
	    (setq p (cdr p))))
      (or cl-end (setq cl-end (length seq)))
      (if (and (= cl-start 0) (= cl-end (length seq)))
          (fillarray seq item)
	(while (< cl-start cl-end)
          (aset seq cl-start item)
	  (setq cl-start (1+ cl-start)))))
    seq))

;;;###autoload
(defun cl-replace (seq1 seq2 &rest cl-keys)
  "Replace the elements of SEQ1 with the elements of SEQ2.
SEQ1 is destructively modified, then returned.
\nKeywords supported:  :start1 :end1 :start2 :end2
\n(fn SEQ1 SEQ2 [KEYWORD VALUE]...)"
  (cl--parsing-keywords ((:start1 0) :end1 (:start2 0) :end2) ()
    (if (and (eq seq1 seq2) (<= cl-start2 cl-start1))
	(or (= cl-start1 cl-start2)
            (let* ((len (length seq1))
                   (n (min (- (or cl-end1 len) cl-start1)
                           (- (or cl-end2 len) cl-start2))))
              (while (>= (setq n (1- n)) 0)
                (setf (elt seq1 (+ cl-start1 n))
                      (elt seq2 (+ cl-start2 n))))))
      (if (listp seq1)
          (let ((p1 (nthcdr cl-start1 seq1))
                (n1 (and cl-end1 (- cl-end1 cl-start1))))
            (if (listp seq2)
                (let ((p2 (nthcdr cl-start2 seq2))
                      (n (cond ((and n1 cl-end2)
                                (min n1 (- cl-end2 cl-start2)))
                               ((and n1 (null cl-end2)) n1)
                               ((and (null n1) cl-end2) (- cl-end2 cl-start2)))))
                  (while (and p1 p2 (or (null n) (>= (decf n) 0)))
                    (setcar p1 (car p2))
                    (setq p1 (cdr p1) p2 (cdr p2))))
              (setq cl-end2 (if (null n1)
                                (or cl-end2 (length seq2))
                              (min (or cl-end2 (length seq2))
                                   (+ cl-start2 n1))))
              (while (and p1 (< cl-start2 cl-end2))
                (setcar p1 (aref seq2 cl-start2))
                (setq p1 (cdr p1) cl-start2 (1+ cl-start2)))))
        (setq cl-end1 (min (or cl-end1 (length seq1))
                           (+ cl-start1 (- (or cl-end2 (length seq2))
					   cl-start2))))
        (if (listp seq2)
            (let ((p2 (nthcdr cl-start2 seq2)))
	      (while (< cl-start1 cl-end1)
                (aset seq1 cl-start1 (car p2))
                (setq p2 (cdr p2) cl-start1 (1+ cl-start1))))
	  (while (< cl-start1 cl-end1)
            (aset seq1 cl-start1 (aref seq2 cl-start2))
	    (setq cl-start2 (1+ cl-start2) cl-start1 (1+ cl-start1))))))
    seq1))

;;;###autoload
(defun cl-remove (item seq &rest cl-keys)
  "Remove all occurrences of ITEM in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :test :test-not :key :count :start :end :from-end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cl--parsing-keywords ( :test :test-not :key :if :if-not :count :from-end
                          (:start 0) :end) ()
    (let ((len (length seq)))
      (if (<= (or cl-count (setq cl-count len)) 0)
          seq
        (if (or (nlistp seq) (and cl-from-end (< cl-count (/ len 2))))
            (let ((i (cl--position item seq cl-start cl-end
                                   cl-from-end)))
              (if i
                  (let ((res (apply #'cl-delete item (append seq nil)
                                    (append (if cl-from-end
                                                (list :end (1+ i))
                                              (list :start i))
                                            cl-keys))))
                    (if (listp seq) res
                      (if (stringp seq) (concat res) (vconcat res))))
                seq))
	  (setq cl-end (- (or cl-end len) cl-start))
          (if (= cl-start 0)
              (while (and seq (> cl-end 0)
                          (cl--check-test item (car seq))
                          (setq cl-end (1- cl-end) seq (cdr seq))
                          (> (setq cl-count (1- cl-count)) 0))))
          (if (and (> cl-count 0) (> cl-end 0))
              (let ((p (if (> cl-start 0) (nthcdr cl-start seq)
                         (setq cl-end (1- cl-end)) (cdr seq))))
                (while (and p (> cl-end 0)
                            (not (cl--check-test item (car p))))
                  (setq p (cdr p) cl-end (1- cl-end)))
                (if (and p (> cl-end 0))
                    (nconc (cl-ldiff seq p)
                           (if (= cl-count 1) (cdr p)
                             (and (cdr p)
                                  (apply #'cl-delete item
                                         (copy-sequence (cdr p))
                                         :start 0 :end (1- cl-end)
                                         :count (1- cl-count) cl-keys))))
                  seq))
            seq))))))

;;;###autoload
(defun cl-remove-if (pred list &rest cl-keys)
  "Remove all items satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-remove nil list :if pred cl-keys))

;;;###autoload
(defun cl-remove-if-not (pred list &rest cl-keys)
  "Remove all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-remove nil list :if-not pred cl-keys))

;;;###autoload
(defun cl-delete (item seq &rest cl-keys)
  "Remove all occurrences of ITEM in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :test :test-not :key :count :start :end :from-end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cl--parsing-keywords ( :test :test-not :key :if :if-not :count :from-end
                          (:start 0) :end) ()
    (let ((len (length seq)))
      (if (<= (or cl-count (setq cl-count len)) 0)
          seq
        (if (listp seq)
            (if (and cl-from-end (< cl-count (/ len 2)))
                (let (i)
                  (while (and (>= (setq cl-count (1- cl-count)) 0)
                              (setq i (cl--position item seq cl-start
                                                    cl-end cl-from-end)))
                    (if (= i 0) (setq seq (cdr seq))
                      (let ((tail (nthcdr (1- i) seq)))
                        (setcdr tail (cdr (cdr tail)))))
                    (setq cl-end i))
                  seq)
              (setq cl-end (- (or cl-end len) cl-start))
              (if (= cl-start 0)
                  (progn
                    (while (and seq
                                (> cl-end 0)
                                (cl--check-test item (car seq))
                                (setq cl-end (1- cl-end) seq (cdr seq))
                                (> (setq cl-count (1- cl-count)) 0)))
                    (setq cl-end (1- cl-end)))
                (setq cl-start (1- cl-start)))
              (if (and (> cl-count 0) (> cl-end 0))
                  (let ((p (nthcdr cl-start seq)))
                    (while (and (cdr p) (> cl-end 0))
                      (if (cl--check-test item (car (cdr p)))
                          (progn
                            (setcdr p (cdr (cdr p)))
                            (if (= (setq cl-count (1- cl-count)) 0)
                                (setq cl-end 1)))
                        (setq p (cdr p)))
                      (setq cl-end (1- cl-end)))))
              seq)
          (apply #'cl-remove item seq cl-keys))))))

;;;###autoload
(defun cl-delete-if (pred list &rest cl-keys)
  "Remove all items satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-delete nil list :if pred cl-keys))

;;;###autoload
(defun cl-delete-if-not (pred list &rest cl-keys)
  "Remove all items not satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-delete nil list :if-not pred cl-keys))

;;;###autoload
(defun cl-remove-duplicates (seq &rest cl-keys)
  "Return a copy of SEQ with all duplicate elements removed.
\nKeywords supported:  :test :test-not :key :start :end :from-end
\n(fn SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cl--delete-duplicates seq cl-keys t))

;;;###autoload
(defun cl-delete-duplicates (seq &rest cl-keys)
  "Remove all duplicate elements from SEQ (destructively).
\nKeywords supported:  :test :test-not :key :start :end :from-end
\n(fn SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cl--delete-duplicates seq cl-keys nil))

(defun cl--delete-duplicates (seq cl-keys copy)
  (if (listp seq)
      (cl--parsing-keywords
          ;; We need to parse :if, otherwise `cl-if' is unbound.
          (:test :test-not :key (:start 0) :end :from-end :if)
	  ()
	(if cl-from-end
            (let ((p (nthcdr cl-start seq)) i)
              (setq cl-end (- (or cl-end (length seq)) cl-start))
	      (while (> cl-end 1)
                (setq i 0)
                (while (setq i (cl--position (cl--check-key (car p))
                                             (cdr p) i (1- cl-end)))
                  (if copy (setq seq (copy-sequence seq)
                                 p (nthcdr cl-start seq) copy nil))
                  (let ((tail (nthcdr i p)))
                    (setcdr tail (cdr (cdr tail))))
		  (setq cl-end (1- cl-end)))
                (setq p (cdr p) cl-end (1- cl-end)
		      cl-start (1+ cl-start)))
              seq)
          (setq cl-end (- (or cl-end (length seq)) cl-start))
          (while (and (cdr seq) (= cl-start 0) (> cl-end 1)
                      (cl--position (cl--check-key (car seq))
                                    (cdr seq) 0 (1- cl-end)))
            (setq seq (cdr seq) cl-end (1- cl-end)))
          (let ((p (if (> cl-start 0) (nthcdr (1- cl-start) seq)
                     (setq cl-end (1- cl-end) cl-start 1) seq)))
            (while (and (cdr (cdr p)) (> cl-end 1))
              (if (cl--position (cl--check-key (car (cdr p)))
                                (cdr (cdr p)) 0 (1- cl-end))
		  (progn
                    (if copy (setq seq (copy-sequence seq)
                                   p (nthcdr (1- cl-start) seq)
                                   copy nil))
                    (setcdr p (cdr (cdr p))))
                (setq p (cdr p)))
	      (setq cl-end (1- cl-end) cl-start (1+ cl-start)))
            seq)))
    (let ((res (cl--delete-duplicates (append seq nil) cl-keys nil)))
      (if (stringp seq) (concat res) (vconcat res)))))

;;;###autoload
(defun cl-substitute (new old seq &rest cl-keys)
  "Substitute NEW for OLD in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :test :test-not :key :count :start :end :from-end
\n(fn NEW OLD SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cl--parsing-keywords ( :test :test-not :key :if :if-not :count
                          (:start 0) :end :from-end) ()
    (if (or (eq old new)
	    (<= (or cl-count (setq cl-from-end nil
                                   cl-count (length seq))) 0))
        seq
      (let ((i (cl--position old seq cl-start cl-end)))
        (if (not i)
            seq
          (setq seq (copy-sequence seq))
	  (unless cl-from-end
            (setf (elt seq i) new)
            (incf i)
	    (decf cl-count))
          (apply #'cl-nsubstitute new old seq :count cl-count
                 :start i cl-keys))))))

;;;###autoload
(defun cl-substitute-if (new pred seq &rest cl-keys)
  "Substitute NEW for all items satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-substitute new nil seq :if pred cl-keys))

;;;###autoload
(defun cl-substitute-if-not (new pred seq &rest cl-keys)
  "Substitute NEW for all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-substitute new nil seq :if-not pred cl-keys))

;;;###autoload
(defun cl-nsubstitute (new old seq &rest cl-keys)
  "Substitute NEW for OLD in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :test :test-not :key :count :start :end :from-end
\n(fn NEW OLD SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cl--parsing-keywords ( :test :test-not :key :if :if-not :count
                          (:start 0) :end :from-end) ()
    (let* ((stringp (stringp seq))
           (seq (if (stringp seq) (string-to-vector seq) seq))
           (len (length seq)))
      (or (eq old new) (<= (or cl-count (setq cl-count len)) 0)
          (if (and (listp seq) (or (not cl-from-end) (> cl-count (/ len 2))))
              (let ((p (nthcdr cl-start seq)))
                (setq cl-end (- (or cl-end len) cl-start))
                (while (and p (> cl-end 0) (> cl-count 0))
                  (if (cl--check-test old (car p))
                      (progn
                        (setcar p new)
                        (setq cl-count (1- cl-count))))
                  (setq p (cdr p) cl-end (1- cl-end))))
	    (or cl-end (setq cl-end len))
            (if cl-from-end
                (while (and (< cl-start cl-end) (> cl-count 0))
                  (setq cl-end (1- cl-end))
                  (if (cl--check-test old (elt seq cl-end))
                      (progn
                        (setf (elt seq cl-end) new)
                        (setq cl-count (1- cl-count)))))
              (while (and (< cl-start cl-end) (> cl-count 0))
                (if (cl--check-test old (aref seq cl-start))
                    (progn
                      (aset seq cl-start new)
                      (setq cl-count (1- cl-count))))
                (setq cl-start (1+ cl-start))))))
      (if stringp (concat seq) seq))))

;;;###autoload
(defun cl-nsubstitute-if (new pred list &rest cl-keys)
  "Substitute NEW for all items satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-nsubstitute new nil list :if pred cl-keys))

;;;###autoload
(defun cl-nsubstitute-if-not (new pred list &rest cl-keys)
  "Substitute NEW for all items not satisfying PREDICATE in SEQ.
This is a destructive function; it reuses the storage of SEQ whenever possible.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn NEW PREDICATE SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-nsubstitute new nil list :if-not pred cl-keys))

;;;###autoload
(defun cl-find (item seq &rest cl-keys)
  "Find the first occurrence of ITEM in SEQ.
Return the matching ITEM, or nil if not found.
\nKeywords supported:  :test :test-not :key :start :end :from-end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (let ((pos (apply #'cl-position item seq cl-keys)))
    (and pos (elt seq pos))))

;;;###autoload
(defun cl-find-if (pred list &rest cl-keys)
  "Find the first item satisfying PREDICATE in SEQ.
Return the matching item, or nil if not found.
\nKeywords supported:  :key :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-find nil list :if pred cl-keys))

;;;###autoload
(defun cl-find-if-not (pred list &rest cl-keys)
  "Find the first item not satisfying PREDICATE in SEQ.
Return the matching item, or nil if not found.
\nKeywords supported:  :key :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-find nil list :if-not pred cl-keys))

;;;###autoload
(defun cl-position (item seq &rest cl-keys)
  "Find the first occurrence of ITEM in SEQ.
Return the index of the matching item, or nil if not found.
\nKeywords supported:  :test :test-not :key :start :end :from-end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cl--parsing-keywords ( :test :test-not :key :if :if-not
                          (:start 0) :end :from-end) ()
    (cl--position item seq cl-start cl-end cl-from-end)))

(defun cl--position (item seq start &optional end from-end)
  (if (listp seq)
      (let ((p (nthcdr start seq))
            res)
        (while (and p (or (null end) (< start end)) (or (null res) from-end))
          (if (cl--check-test item (car p))
              (setq res start))
          (setq p (cdr p) start (1+ start)))
        res)
    (or end (setq end (length seq)))
    (if from-end
	(progn
          (while (and (>= (setq end (1- end)) start)
                      (not (cl--check-test item (aref seq end)))))
          (and (>= end start) end))
      (while (and (< start end)
                  (not (cl--check-test item (aref seq start))))
        (setq start (1+ start)))
      (and (< start end) start))))

;;;###autoload
(defun cl-position-if (pred list &rest cl-keys)
  "Find the first item satisfying PREDICATE in SEQ.
Return the index of the matching item, or nil if not found.
\nKeywords supported:  :key :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-position nil list :if pred cl-keys))

;;;###autoload
(defun cl-position-if-not (pred list &rest cl-keys)
  "Find the first item not satisfying PREDICATE in SEQ.
Return the index of the matching item, or nil if not found.
\nKeywords supported:  :key :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-position nil list :if-not pred cl-keys))

;;;###autoload
(defun cl-count (item seq &rest cl-keys)
  "Count the number of occurrences of ITEM in SEQ.
\nKeywords supported:  :test :test-not :key :start :end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cl--parsing-keywords (:test :test-not :key :if :if-not (:start 0) :end) ()
    (let ((count 0) x)
      (or cl-end (setq cl-end (length seq)))
      (if (consp seq) (setq seq (nthcdr cl-start seq)))
      (while (< cl-start cl-end)
        (setq x (if (consp seq) (pop seq) (aref seq cl-start)))
        (if (cl--check-test item x) (incf count))
	(setq cl-start (1+ cl-start)))
      count)))

;;;###autoload
(defun cl-count-if (pred list &rest cl-keys)
  "Count the number of items satisfying PREDICATE in SEQ.
\nKeywords supported:  :key :start :end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-count nil list :if pred cl-keys))

;;;###autoload
(defun cl-count-if-not (pred list &rest cl-keys)
  "Count the number of items not satisfying PREDICATE in SEQ.
\nKeywords supported:  :key :start :end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-count nil list :if-not pred cl-keys))

;;;###autoload
(defun cl-mismatch (seq1 seq2 &rest cl-keys)
  "Compare SEQ1 with SEQ2, return index of first mismatching element.
Return nil if the sequences match.  If one sequence is a prefix of the
other, the return value indicates the end of the shorter sequence.
\nKeywords supported:  :test :test-not :key :start1 :end1 :start2 :end2 :from-end
\n(fn SEQ1 SEQ2 [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cl--parsing-keywords ( :test :test-not :key :from-end
                          (:start1 0) :end1 (:start2 0) :end2) ()
    (or cl-end1 (setq cl-end1 (length seq1)))
    (or cl-end2 (setq cl-end2 (length seq2)))
    (if cl-from-end
	(progn
	  (while (and (< cl-start1 cl-end1) (< cl-start2 cl-end2)
                      (cl--check-match (elt seq1 (1- cl-end1))
                                       (elt seq2 (1- cl-end2))))
	    (setq cl-end1 (1- cl-end1) cl-end2 (1- cl-end2)))
	  (and (or (< cl-start1 cl-end1) (< cl-start2 cl-end2))
	       (1- cl-end1)))
      (let ((p1 (and (listp seq1) (nthcdr cl-start1 seq1)))
            (p2 (and (listp seq2) (nthcdr cl-start2 seq2))))
	(while (and (< cl-start1 cl-end1) (< cl-start2 cl-end2)
                    (cl--check-match (if p1 (car p1)
                                       (aref seq1 cl-start1))
                                     (if p2 (car p2)
                                       (aref seq2 cl-start2))))
          (setq p1 (cdr p1) p2 (cdr p2)
		cl-start1 (1+ cl-start1) cl-start2 (1+ cl-start2)))
	(and (or (< cl-start1 cl-end1) (< cl-start2 cl-end2))
	     cl-start1)))))

;;;###autoload
(defun cl-search (seq1 seq2 &rest cl-keys)
  "Search for SEQ1 as a subsequence of SEQ2.
Return the index of the leftmost element of the first match found;
return nil if there are no matches.
\nKeywords supported:  :test :test-not :key :start1 :end1 :start2 :end2 :from-end
\n(fn SEQ1 SEQ2 [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cl--parsing-keywords ( :test :test-not :key :from-end
                          (:start1 0) :end1 (:start2 0) :end2) ()
    (or cl-end1 (setq cl-end1 (length seq1)))
    (or cl-end2 (setq cl-end2 (length seq2)))
    (if (>= cl-start1 cl-end1)
	(if cl-from-end cl-end2 cl-start2)
      (let* ((len (- cl-end1 cl-start1))
             (first (cl--check-key (elt seq1 cl-start1)))
             (cl-if nil) pos)
        (setq cl-end2 (- cl-end2 (1- len)))
	(while (and (< cl-start2 cl-end2)
                    (setq pos (cl--position first seq2
                                            cl-start2 cl-end2 cl-from-end))
                    (apply #'cl-mismatch seq1 seq2
			   :start1 (1+ cl-start1) :end1 cl-end1
                           :start2 (1+ pos) :end2 (+ pos len)
			   :from-end nil cl-keys))
          (if cl-from-end (setq cl-end2 pos) (setq cl-start2 (1+ pos))))
        (and (< cl-start2 cl-end2) pos)))))

;;;###autoload
(defun cl-sort (seq pred &rest cl-keys)
  "Sort the argument SEQ according to PREDICATE.
This is a destructive function; it reuses the storage of SEQ if possible.
\nKeywords supported:  :key
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  ;; It's safe to ignore the return value when used on arrays,
  ;; but most calls pass lists.
  (declare (important-return-value t))
  (if (nlistp seq)
      (if (stringp seq)
          (concat (apply #'cl-sort (vconcat seq) pred cl-keys))
        (cl-replace seq
                    (apply #'cl-sort (append seq nil) pred cl-keys)))
    (cl--parsing-keywords (:key) ()
      (if (memq cl-key '(nil identity))
          (sort seq pred)
        (sort seq (lambda (x y)
                    (funcall pred (funcall cl-key x)
                             (funcall cl-key y))))))))

;;;###autoload
(defun cl-stable-sort (seq pred &rest cl-keys)
  "Sort the argument SEQ stably according to PREDICATE.
This is a destructive function; it reuses the storage of SEQ if possible.
\nKeywords supported:  :key
\n(fn SEQ PREDICATE [KEYWORD VALUE]...)"
  ;; It's safe to ignore the return value when used on arrays,
  ;; but most calls pass lists.
  (declare (important-return-value t))
  (apply #'cl-sort seq pred cl-keys))

;;;###autoload
(defun cl-merge (type seq1 seq2 pred &rest cl-keys)
  "Destructively merge the two sequences to produce a new sequence.
TYPE is the sequence type to return, SEQ1 and SEQ2 are the two argument
sequences, and PREDICATE is a `less-than' predicate on the elements.
\nKeywords supported:  :key
\n(fn TYPE SEQ1 SEQ2 PREDICATE [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (or (listp seq1) (setq seq1 (append seq1 nil)))
  (or (listp seq2) (setq seq2 (append seq2 nil)))
  (cl--parsing-keywords (:key) ()
    (let ((res nil))
      (while (and seq1 seq2)
        (if (funcall pred (cl--check-key (car seq2))
                     (cl--check-key (car seq1)))
            (push (pop seq2) res)
          (push (pop seq1) res)))
      (cl-coerce (nconc (nreverse res) seq1 seq2) type))))

;;;###autoload
(defun cl-member (item list &rest cl-keys)
  "Find the first occurrence of ITEM in LIST.
Return the sublist of LIST whose car is ITEM.
\nKeywords supported:  :test :test-not :key
\n(fn ITEM LIST [KEYWORD VALUE]...)"
  (declare (important-return-value t)
           (compiler-macro cl--compiler-macro-member))
  (if cl-keys
      (cl--parsing-keywords (:test :test-not :key :if :if-not) ()
        (while (and list (not (cl--check-test item (car list))))
          (setq list (cdr list)))
        list)
    (memql item list)))
(autoload 'cl--compiler-macro-member "cl-macs")

;;;###autoload
(defun cl-member-if (pred list &rest cl-keys)
  "Find the first item satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-member nil list :if pred cl-keys))

;;;###autoload
(defun cl-member-if-not (pred list &rest cl-keys)
  "Find the first item not satisfying PREDICATE in LIST.
Return the sublist of LIST whose car matches.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-member nil list :if-not pred cl-keys))

;;;###autoload
(defun cl--adjoin (item list &rest cl-keys)
  (if (cl--parsing-keywords (:key) t
        (apply #'cl-member (cl--check-key item) list cl-keys))
      list
    (cons item list)))

;;;###autoload
(defun cl-assoc (item alist &rest cl-keys)
  "Find the first item whose car matches ITEM in LIST.
\nKeywords supported:  :test :test-not :key
\n(fn ITEM LIST [KEYWORD VALUE]...)"
  (declare (important-return-value t)
           (compiler-macro cl--compiler-macro-assoc))
  (if cl-keys
      (cl--parsing-keywords (:test :test-not :key :if :if-not) ()
        (while (and alist
                    (or (not (consp (car alist)))
                        (not (cl--check-test item (car (car alist))))))
          (setq alist (cdr alist)))
        (and alist (car alist)))
    (if (and (numberp item) (not (fixnump item)))
        (assoc item alist)
      (assq item alist))))
(autoload 'cl--compiler-macro-assoc "cl-macs")

;;;###autoload
(defun cl-assoc-if (pred list &rest cl-keys)
  "Find the first item whose car satisfies PREDICATE in LIST.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-assoc nil list :if pred cl-keys))

;;;###autoload
(defun cl-assoc-if-not (pred list &rest cl-keys)
  "Find the first item whose car does not satisfy PREDICATE in LIST.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-assoc nil list :if-not pred cl-keys))

;;;###autoload
(defun cl-rassoc (item alist &rest cl-keys)
  "Find the first item whose cdr matches ITEM in LIST.
\nKeywords supported:  :test :test-not :key
\n(fn ITEM LIST [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (if (or cl-keys (numberp item))
      (cl--parsing-keywords (:test :test-not :key :if :if-not) ()
        (while (and alist
                    (or (not (consp (car alist)))
                        (not (cl--check-test item (cdr (car alist))))))
          (setq alist (cdr alist)))
        (and alist (car alist)))
    (rassq item alist)))

;;;###autoload
(defun cl-rassoc-if (pred list &rest cl-keys)
  "Find the first item whose cdr satisfies PREDICATE in LIST.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-rassoc nil list :if pred cl-keys))

;;;###autoload
(defun cl-rassoc-if-not (pred list &rest cl-keys)
  "Find the first item whose cdr does not satisfy PREDICATE in LIST.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-rassoc nil list :if-not pred cl-keys))

;;;###autoload
(defun cl-union (list1 list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-union operation.
The resulting list contains all items that appear in either LIST1 or LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cond ((null list1) list2) ((null list2) list1)
        ((and (not cl-keys) (equal list1 list2)) list1)
	(t
         (or (>= (length list1) (length list2))
             (setq list1 (prog1 list2 (setq list2 list1))))
         (while list2
           (if (or cl-keys (numberp (car list2)))
               (setq list1
                     (apply #'cl-adjoin (car list2) list1 cl-keys))
             (or (memq (car list2) list1)
                 (push (car list2) list1)))
           (pop list2))
         list1)))

;;;###autoload
(defun cl-nunion (list1 list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-union operation.
The resulting list contains all items that appear in either LIST1 or LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cond ((null list1) list2) ((null list2) list1)
        (t (apply #'cl-union list1 list2 cl-keys))))

;;;###autoload
(defun cl-intersection (list1 list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-intersection operation.
The resulting list contains all items that appear in both LIST1 and LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (and list1 list2
       (if (equal list1 list2) list1
	 (cl--parsing-keywords (:key) (:test :test-not)
           (let ((res nil))
             (or (>= (length list1) (length list2))
                 (setq list1 (prog1 list2 (setq list2 list1))))
             (while list2
               (if (if (or cl-keys (numberp (car list2)))
                       (apply #'cl-member (cl--check-key (car list2))
                              list1 cl-keys)
                     (memq (car list2) list1))
                   (push (car list2) res))
               (pop list2))
             res)))))

;;;###autoload
(defun cl-nintersection (list1 list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-intersection operation.
The resulting list contains all items that appear in both LIST1 and LIST2.
This is a destructive function; it reuses the storage of LIST1 (but not
LIST2) whenever possible.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (and list1 list2 (apply #'cl-intersection list1 list2 cl-keys)))

;;;###autoload
(defun cl-set-difference (list1 list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-difference operation.
The resulting list contains all items that appear in LIST1 but not LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (if (or (null list1) (null list2)) list1
    (cl--parsing-keywords (:key) (:test :test-not)
      (let ((res nil))
        (while list1
          (or (if (or cl-keys (numberp (car list1)))
                  (apply #'cl-member (cl--check-key (car list1))
                         list2 cl-keys)
                (memq (car list1) list2))
              (push (car list1) res))
          (pop list1))
        (nreverse res)))))

;;;###autoload
(defun cl-nset-difference (list1 list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-difference operation.
The resulting list contains all items that appear in LIST1 but not LIST2.
This is a destructive function; it reuses the storage of LIST1 (but not
LIST2) whenever possible.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (if (or (null list1) (null list2)) list1
    (apply #'cl-set-difference list1 list2 cl-keys)))

;;;###autoload
(defun cl-set-exclusive-or (list1 list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-exclusive-or operation.
The resulting list contains all items appearing in exactly one of LIST1, LIST2.
This is a non-destructive function; it makes a copy of the data if necessary
to avoid corrupting the original LIST1 and LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cond ((null list1) list2) ((null list2) list1)
        ((equal list1 list2) nil)
        (t (append (apply #'cl-set-difference list1 list2 cl-keys)
                   (apply #'cl-set-difference list2 list1 cl-keys)))))

;;;###autoload
(defun cl-nset-exclusive-or (list1 list2 &rest cl-keys)
  "Combine LIST1 and LIST2 using a set-exclusive-or operation.
The resulting list contains all items appearing in exactly one of LIST1, LIST2.
This is a destructive function; it reuses the storage of LIST1 and LIST2
whenever possible.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cond ((null list1) list2) ((null list2) list1)
        ((equal list1 list2) nil)
        (t (nconc (apply #'cl-nset-difference list1 list2 cl-keys)
                  (apply #'cl-nset-difference list2 list1 cl-keys)))))

;;;###autoload
(defun cl-subsetp (list1 list2 &rest cl-keys)
  "Return true if LIST1 is a subset of LIST2.
I.e., if every element of LIST1 also appears in LIST2.
\nKeywords supported:  :test :test-not :key
\n(fn LIST1 LIST2 [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cond ((null list1) t) ((null list2) nil)
        ((equal list1 list2) t)
	(t (cl--parsing-keywords (:key) (:test :test-not)
             (while (and list1
                         (apply #'cl-member (cl--check-key (car list1))
                                list2 cl-keys))
               (pop list1))
             (null list1)))))

;;;###autoload
(defun cl-subst-if (new pred tree &rest cl-keys)
  "Substitute NEW for elements matching PREDICATE in TREE (non-destructively).
Return a copy of TREE with all matching elements replaced by NEW.
\nKeywords supported:  :key
\n(fn NEW PREDICATE TREE [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-sublis (list (cons nil new)) tree :if pred cl-keys))

;;;###autoload
(defun cl-subst-if-not (new pred tree &rest cl-keys)
  "Substitute NEW for elts not matching PREDICATE in TREE (non-destructively).
Return a copy of TREE with all non-matching elements replaced by NEW.
\nKeywords supported:  :key
\n(fn NEW PREDICATE TREE [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-sublis (list (cons nil new)) tree :if-not pred cl-keys))

;;;###autoload
(defun cl-nsubst (new old tree &rest cl-keys)
  "Substitute NEW for OLD everywhere in TREE (destructively).
Any element of TREE which is `eql' to OLD is changed to NEW (via a call
to `setcar').
\nKeywords supported:  :test :test-not :key
\n(fn NEW OLD TREE [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-nsublis (list (cons old new)) tree cl-keys))

;;;###autoload
(defun cl-nsubst-if (new pred tree &rest cl-keys)
  "Substitute NEW for elements matching PREDICATE in TREE (destructively).
Any element of TREE which matches is changed to NEW (via a call to `setcar').
\nKeywords supported:  :key
\n(fn NEW PREDICATE TREE [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-nsublis (list (cons nil new)) tree :if pred cl-keys))

;;;###autoload
(defun cl-nsubst-if-not (new pred tree &rest cl-keys)
  "Substitute NEW for elements not matching PREDICATE in TREE (destructively).
Any element of TREE which matches is changed to NEW (via a call to `setcar').
\nKeywords supported:  :key
\n(fn NEW PREDICATE TREE [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (apply #'cl-nsublis (list (cons nil new)) tree :if-not pred cl-keys))

(defvar cl--alist)

;;;###autoload
(defun cl-sublis (alist tree &rest cl-keys)
  "Perform substitutions indicated by ALIST in TREE (non-destructively).
Return a copy of TREE with all matching elements replaced.
\nKeywords supported:  :test :test-not :key
\n(fn ALIST TREE [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cl--parsing-keywords (:test :test-not :key :if :if-not) ()
    (let ((cl--alist alist))
      (cl--sublis-rec tree))))

(defun cl--sublis-rec (tree)   ;Uses cl--alist cl-key/test*/if*.
  (let ((temp (cl--check-key tree))
        (p cl--alist))
    (while (and p (not (cl--check-test-nokey (car (car p)) temp)))
      (setq p (cdr p)))
    (if p (cdr (car p))
      (if (consp tree)
          (let ((a (cl--sublis-rec (car tree)))
                (d (cl--sublis-rec (cdr tree))))
            (if (and (eq a (car tree)) (eq d (cdr tree)))
                tree
              (cons a d)))
        tree))))

;;;###autoload
(defun cl-nsublis (alist tree &rest cl-keys)
  "Perform substitutions indicated by ALIST in TREE (destructively).
Any matching element of TREE is changed via a call to `setcar'.
\nKeywords supported:  :test :test-not :key
\n(fn ALIST TREE [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cl--parsing-keywords (:test :test-not :key :if :if-not) ()
    (let ((hold (list tree))
          (cl--alist alist))
      (cl--nsublis-rec hold)
      (car hold))))

(defun cl--nsublis-rec (tree)   ;Uses cl--alist cl-key/test*/if*.
  (while (consp tree)
    (let ((temp (cl--check-key (car tree)))
          (p cl--alist))
      (while (and p (not (cl--check-test-nokey (car (car p)) temp)))
        (setq p (cdr p)))
      (if p (setcar tree (cdr (car p)))
        (if (consp (car tree)) (cl--nsublis-rec (car tree))))
      (setq temp (cl--check-key (cdr tree)) p cl--alist)
      (while (and p (not (cl--check-test-nokey (car (car p)) temp)))
        (setq p (cdr p)))
      (if p
          (progn (setcdr tree (cdr (car p))) (setq tree nil))
        (setq tree (cdr tree))))))

;;;###autoload
(defun cl-tree-equal (x y &rest cl-keys)
  "Return t if trees TREE1 and TREE2 have `eql' leaves.
Atoms are compared by `eql'; cons cells are compared recursively.
\nKeywords supported:  :test :test-not :key
\n(fn TREE1 TREE2 [KEYWORD VALUE]...)"
  (declare (important-return-value t))
  (cl--parsing-keywords (:test :test-not :key) ()
    (cl--tree-equal-rec x y)))

(defun cl--tree-equal-rec (x y)   ;Uses cl-key/test*.
  (while (and (consp x) (consp y)
              (cl--tree-equal-rec (car x) (car y)))
    (setq x (cdr x) y (cdr y)))
  (and (not (consp x)) (not (consp y)) (cl--check-match x y)))


(make-obsolete-variable 'cl-seq-load-hook
                        "use `with-eval-after-load' instead." "28.1")
(run-hooks 'cl-seq-load-hook)

;; Local variables:
;; generated-autoload-file: "cl-loaddefs.el"
;; End:

(provide 'cl-seq)

;;; cl-seq.el ends here
