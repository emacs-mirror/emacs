;;; seq.el --- Sequence manipulation functions  -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: sequences
;; Version: 2.0
;; Package: seq

;; Maintainer: emacs-devel@gnu.org

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

;; Sequence-manipulation functions that complement basic functions
;; provided by subr.el.
;;
;; All functions are prefixed with "seq-".
;;
;; All provided functions work on lists, strings and vectors.
;;
;; Functions taking a predicate or iterating over a sequence using a
;; function as argument take the function as their first argument and
;; the sequence as their second argument.  All other functions take
;; the sequence as their first argument.
;;
;; While seq.el version 1.8 is in GNU ELPA for convenience, seq.el
;; version 2.0 requires Emacs>=25.1.
;;
;; seq.el can be extended to support new type of sequences.  Here are
;; the generic functions that must be implemented by new seq types:
;; - `seq-elt'
;; - `seq-length'
;; - `seq-do'
;; - `seq-p'
;; - `seq-subseq'
;; - `seq-into-sequence'
;; - `seq-copy'
;; - `seq-into'
;;
;; All functions are tested in test/automated/seq-tests.el

;;; Code:

(eval-when-compile (require 'cl-generic))
(require 'cl-extra) ;; for cl-subseq

(defmacro seq-doseq (spec &rest body)
  "Loop over a sequence.
Similar to `dolist' but can be applied to lists, strings, and vectors.

Evaluate BODY with VAR bound to each element of SEQ, in turn.

\(fn (VAR SEQ) BODY...)"
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  `(seq-do (lambda (,(car spec))
             ,@body)
           ,(cadr spec)))

(pcase-defmacro seq (&rest args)
  "pcase pattern matching sequence elements.
Matches if the object is a sequence (list, string or vector), and
binds each element of ARGS to the corresponding element of the
sequence."
  `(and (pred seq-p)
        ,@(seq--make-pcase-bindings args)))

(defmacro seq-let (args seq &rest body)
  "Bind the variables in ARGS to the elements of SEQ then evaluate BODY.

ARGS can also include the `&rest' marker followed by a variable
name to be bound to the rest of SEQ."
  (declare (indent 2) (debug t))
  `(pcase-let ((,(seq--make-pcase-patterns args) ,seq))
     ,@body))


;;; Basic seq functions that have to be implemented by new seq types
(cl-defgeneric seq-elt (seq n)
  "Return the element of SEQ at index N."
  (elt seq n))

;; Default gv setters for `seq-elt'.
;; It can be a good idea for new sequence implementations to provide a
;; "gv-setter" for `seq-elt'.
(cl-defmethod (setf seq-elt) (store (seq array) n)
  (aset seq n store))

(cl-defmethod (setf seq-elt) (store (seq cons) n)
  (setcar (nthcdr n seq) store))

(cl-defgeneric seq-length (seq)
  "Return the length of the sequence SEQ."
  (length seq))

(cl-defgeneric seq-do (function seq)
  "Apply FUNCTION to each element of SEQ, presumably for side effects.
Return SEQ."
  (mapc function seq))

(defalias 'seq-each #'seq-do)

(cl-defgeneric seq-p (seq)
  "Return non-nil if SEQ is a sequence, nil otherwise."
  (sequencep seq))

(cl-defgeneric seq-copy (seq)
  "Return a shallow copy of SEQ."
  (copy-sequence seq))

(cl-defgeneric seq-subseq (seq start &optional end)
  "Return the subsequence of SEQ from START to END.
If END is omitted, it defaults to the length of the sequence.
If START or END is negative, it counts from the end.
Signal an error if START or END are outside of the sequence (i.e
too large if positive or too small if negative)."
  (cl-subseq seq start end))


(cl-defgeneric seq-map (function seq)
  "Return the result of applying FUNCTION to each element of SEQ."
  (let (result)
    (seq-do (lambda (elt)
              (push (funcall function elt) result))
            seq)
    (nreverse result)))

;; faster implementation for sequences (sequencep)
(cl-defmethod seq-map (function (seq sequence))
  (mapcar function seq))

(cl-defgeneric seq-drop (seq n)
  "Return a subsequence of SEQ without its first N elements.
The result is a sequence of the same type as SEQ.

If N is a negative integer or zero, SEQ is returned."
  (if (<= n 0)
      seq
    (let ((length (seq-length seq)))
      (seq-subseq seq (min n length) length))))

(cl-defgeneric seq-take (seq n)
  "Return a subsequence of SEQ with its first N elements.
The result is a sequence of the same type as SEQ.

If N is a negative integer or zero, an empty sequence is
returned."
  (seq-subseq seq 0 (min (max n 0) (seq-length seq))))

(cl-defgeneric seq-drop-while (pred seq)
  "Return a sequence from the first element for which (PRED element) is nil in SEQ.
The result is a sequence of the same type as SEQ."
  (seq-drop seq (seq--count-successive pred seq)))

(cl-defgeneric seq-take-while (pred seq)
  "Return the successive elements for which (PRED element) is non-nil in SEQ.
The result is a sequence of the same type as SEQ."
  (seq-take seq (seq--count-successive pred seq)))

(cl-defgeneric seq-empty-p (seq)
  "Return non-nil if the sequence SEQ is empty, nil otherwise."
  (= 0 (seq-length seq)))

(cl-defgeneric seq-sort (pred seq)
  "Return a sorted sequence comparing using PRED the elements of SEQ.
The result is a sequence of the same type as SEQ."
  (let ((result (seq-sort pred (append seq nil))))
    (seq-into result (type-of seq))))

(cl-defmethod seq-sort (pred (list list))
  (sort (seq-copy list) pred))

(cl-defgeneric seq-reverse (seq)
  "Return the reversed shallow copy of SEQ."
  (let ((result '()))
    (seq-map (lambda (elt)
               (push elt result))
             seq)
    (seq-into result (type-of seq))))

;; faster implementation for sequences (sequencep)
(cl-defmethod seq-reverse ((seq sequence))
  (reverse seq))

(cl-defgeneric seq-concatenate (type &rest seqs)
  "Concatenate, into a sequence of type TYPE, the sequences SEQS.
TYPE must be one of following symbols: vector, string or list.

\n(fn TYPE SEQUENCE...)"
  (apply #'cl-concatenate type (seq-map #'seq-into-sequence seqs)))

(cl-defgeneric seq-into-sequence (seq)
  "Convert SEQ into a sequence.

The default implementation is to signal an error if SEQ is not a
sequence, specific functions should be implemented for new types
of seq."
  (unless (sequencep seq)
    (error "Cannot convert %S into a sequence" seq))
  seq)

(cl-defgeneric seq-into (seq type)
  "Convert the sequence SEQ into a sequence of type TYPE.
TYPE can be one of the following symbols: vector, string or list."
  (pcase type
    (`vector (vconcat seq))
    (`string (concat seq))
    (`list (append seq nil))
    (_ (error "Not a sequence type name: %S" type))))

(cl-defgeneric seq-filter (pred seq)
  "Return a list of all the elements for which (PRED element) is non-nil in SEQ."
  (let ((exclude (make-symbol "exclude")))
    (delq exclude (seq-map (lambda (elt)
                             (if (funcall pred elt)
                                 elt
                               exclude))
                           seq))))

(cl-defgeneric seq-remove (pred seq)
  "Return a list of all the elements for which (PRED element) is nil in SEQ."
  (seq-filter (lambda (elt) (not (funcall pred elt)))
              seq))

(cl-defgeneric seq-reduce (function seq initial-value)
  "Reduce the function FUNCTION across SEQ, starting with INITIAL-VALUE.

Return the result of calling FUNCTION with INITIAL-VALUE and the
first element of SEQ, then calling FUNCTION with that result and
the second element of SEQ, then with that result and the third
element of SEQ, etc.

If SEQ is empty, return INITIAL-VALUE and FUNCTION is not called."
  (if (seq-empty-p seq)
      initial-value
    (let ((acc initial-value))
      (seq-doseq (elt seq)
        (setq acc (funcall function acc elt)))
      acc)))

(cl-defgeneric seq-some-p (pred seq)
  "Return any element for which (PRED element) is non-nil in SEQ, nil otherwise."
  (catch 'seq--break
    (seq-doseq (elt seq)
      (when (funcall pred elt)
        (throw 'seq--break elt)))
    nil))

(cl-defgeneric seq-every-p (pred seq)
  "Return non-nil if (PRED element) is non-nil for all elements of the sequence SEQ."
  (catch 'seq--break
    (seq-doseq (elt seq)
      (or (funcall pred elt)
          (throw 'seq--break nil)))
    t))

(cl-defgeneric seq-count (pred seq)
  "Return the number of elements for which (PRED element) is non-nil in SEQ."
  (let ((count 0))
    (seq-doseq (elt seq)
      (when (funcall pred elt)
        (setq count (+ 1 count))))
    count))

(cl-defgeneric seq-contains-p (seq elt &optional testfn)
  "Return the first element in SEQ that equals to ELT.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-some-p (lambda (e)
                (funcall (or testfn #'equal) elt e))
              seq))

(cl-defgeneric seq-uniq (seq &optional testfn)
  "Return a list of the elements of SEQ with duplicates removed.
TESTFN is used to compare elements, or `equal' if TESTFN is nil."
  (let ((result '()))
    (seq-doseq (elt seq)
      (unless (seq-contains-p result elt testfn)
        (setq result (cons elt result))))
    (nreverse result)))

(cl-defgeneric seq-mapcat (function seq &optional type)
  "Concatenate the result of applying FUNCTION to each element of SEQ.
The result is a sequence of type TYPE, or a list if TYPE is nil."
  (apply #'seq-concatenate (or type 'list)
         (seq-map function seq)))

(cl-defgeneric seq-partition (seq n)
  "Return a list of the elements of SEQ grouped into sub-sequences of length N.
The last sequence may contain less than N elements.  If N is a
negative integer or 0, nil is returned."
  (unless (< n 1)
    (let ((result '()))
      (while (not (seq-empty-p seq))
        (push (seq-take seq n) result)
        (setq seq (seq-drop seq n)))
      (nreverse result))))

(cl-defgeneric seq-intersection (seq1 seq2 &optional testfn)
  "Return a list of the elements that appear in both SEQ1 and SEQ2.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-reduce (lambda (acc elt)
                (if (seq-contains-p seq2 elt testfn)
                    (cons elt acc)
                  acc))
              (seq-reverse seq1)
              '()))

(cl-defgeneric seq-difference (seq1 seq2 &optional testfn)
  "Return a list of the elements that appear in SEQ1 but not in SEQ2.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (seq-reduce (lambda (acc elt)
                (if (not (seq-contains-p seq2 elt testfn))
                    (cons elt acc)
                  acc))
              (seq-reverse seq1)
              '()))

(cl-defgeneric seq-group-by (function seq)
  "Apply FUNCTION to each element of SEQ.
Separate the elements of SEQ into an alist using the results as
keys.  Keys are compared using `equal'."
  (seq-reduce
   (lambda (acc elt)
     (let* ((key (funcall function elt))
            (cell (assoc key acc)))
       (if cell
           (setcdr cell (push elt (cdr cell)))
         (push (list key elt) acc))
       acc))
   (seq-reverse seq)
   nil))

(cl-defgeneric seq-min (seq)
  "Return the smallest element of SEQ.
SEQ must be a sequence of numbers or markers."
  (apply #'min (seq-into seq 'list)))

(cl-defgeneric seq-max (seq)
  "Return the largest element of SEQ.
SEQ must be a sequence of numbers or markers."
  (apply #'max (seq-into seq 'list)))

(defun seq--count-successive (pred seq)
  "Return the number of successive elements for which (PRED element) is non-nil in SEQ."
  (let ((n 0)
        (len (seq-length seq)))
    (while (and (< n len)
                (funcall pred (seq-elt seq n)))
      (setq n (+ 1 n)))
    n))

(defun seq--make-pcase-bindings (args)
  "Return a list of bindings of the variables in ARGS to the elements of a sequence."
  (let ((bindings '())
        (index 0)
        (rest-marker nil))
    (seq-doseq (name args)
      (unless rest-marker
        (pcase name
          (`&rest
           (progn (push `(app (pcase--flip seq-drop ,index)
                              ,(seq--elt-safe args (1+ index)))
                        bindings)
                  (setq rest-marker t)))
          (_
           (push `(app (pcase--flip seq--elt-safe ,index) ,name) bindings))))
      (setq index (1+ index)))
    bindings))

(defun seq--make-pcase-patterns (args)
  "Return a list of `(seq ...)' pcase patterns from the argument list ARGS."
  (cons 'seq
        (seq-map (lambda (elt)
                   (if (seq-p elt)
                       (seq--make-pcase-patterns elt)
                     elt))
                 args)))

;; TODO: make public?
(defun seq--elt-safe (seq n)
  "Return element of SEQ at the index N.
If no element is found, return nil."
  (ignore-errors (seq-elt seq n)))


;;; Optimized implementations for lists

(cl-defmethod seq-drop ((list list) n)
  "Optimized implementation of `seq-drop' for lists."
  (while (and list (> n 0))
    (setq list (cdr list)
          n (1- n)))
  list)

(cl-defmethod seq-take ((list list) n)
  "Optimized implementation of `seq-take' for lists."
  (let ((result '()))
    (while (and list (> n 0))
      (setq n (1- n))
      (push (pop list) result))
    (nreverse result)))

(cl-defmethod seq-drop-while (pred (list list))
  "Optimized implementation of `seq-drop-while' for lists"
  (while (and list (funcall pred (car list)))
    (setq list (cdr list)))
  list)

(cl-defmethod seq-drop-while (pred (list list))
  "Optimized implementation of `seq-drop-while' for lists"
  (while (and list (funcall pred (car list)))
    (setq list (cdr list)))
  list)

(cl-defmethod seq-empty-p ((list list))
  "Optimized implementation of `seq-empty-p' for lists."
  (null list))


(defun seq--activate-font-lock-keywords ()
  "Activate font-lock keywords for some symbols defined in seq."
  (font-lock-add-keywords 'emacs-lisp-mode
                          '("\\<seq-doseq\\>" "\\<seq-let\\>")))

(unless (fboundp 'elisp--font-lock-flush-elisp-buffers)
  ;; In Emacs≥25, (via elisp--font-lock-flush-elisp-buffers and a few others)
  ;; we automatically highlight macros.
  (add-hook 'emacs-lisp-mode-hook #'seq--activate-font-lock-keywords))

(provide 'seq)
;;; seq.el ends here
