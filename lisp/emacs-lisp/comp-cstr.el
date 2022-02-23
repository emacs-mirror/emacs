;;; comp-cstr.el --- native compiler constraint library -*- lexical-binding: t -*-

;; Copyright (C) 2020-2022 Free Software Foundation, Inc.

;; Author: Andrea Corallo <akrl@sdf.com>
;; Keywords: lisp
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

;; Constraint library in use by the native compiler.

;; In LIMPLE each non immediate value is represented by a `comp-mvar'.
;; The part concerning the set of all values the `comp-mvar' can
;; assume is described into its constraint `comp-cstr'.  Each
;; constraint consists in a triplet: type-set, value-set, range-set.
;; This file provide set operations between constraints (union
;; intersection and negation) plus routines to convert from and to a
;; CL like type specifier.

;;; Code:

(require 'cl-lib)

(defconst comp--typeof-types (mapcar (lambda (x)
                                       (append x '(t)))
                                     cl--typeof-types)
  ;; TODO can we just add t in `cl--typeof-types'?
  "Like `cl--typeof-types' but with t as common supertype.")

(defconst comp--all-builtin-types
  (append cl--all-builtin-types '(t))
  "Likewise like `cl--all-builtin-types' but with t as common supertype.")

(cl-defstruct (comp-cstr (:constructor comp-type-to-cstr
                                       (type &aux
					     (null (eq type 'null))
                                             (integer (eq type 'integer))
					     (typeset (if (or null integer)
							  nil
							(list type)))
					     (valset (when null
						       '(nil)))
                                             (range (when integer
                                                      '((- . +))))))
                         (:constructor comp-value-to-cstr
                                       (value &aux
                                              (integer (integerp value))
                                              (valset (unless integer
                                                        (list value)))
                                              (range (when integer
                                                       `((,value . ,value))))
                                              (typeset ())))
                         (:constructor comp-irange-to-cstr
                                       (irange &aux
                                               (range (list irange))
                                               (typeset ())))
                         (:copier comp-cstr-shallow-copy))
  "Internal representation of a type/value constraint."
  (typeset '(t) :type list
           :documentation "List of possible types the mvar can assume.
Each element cannot be a subtype of any other element of this slot.")
  (valset () :type list
          :documentation "List of possible values the mvar can assume.
Integer values are handled in the `range' slot.")
  (range () :type list
         :documentation "Integer interval.")
  (neg nil :type boolean
       :documentation "Non-nil if the constraint is negated"))

(cl-defstruct comp-cstr-f
  "Internal constraint representation for a function."
  (args () :type list
        :documentation "List of `comp-cstr' for its arguments.")
  (ret nil :type (or comp-cstr comp-cstr-f)
       :documentation "Returned value."))

(cl-defstruct comp-cstr-ctxt
  (union-typesets-mem (make-hash-table :test #'equal) :type hash-table
                      :documentation "Serve memoization for
`comp-union-typesets'.")
  ;; TODO we should be able to just cons hash this.
  (common-supertype-mem (make-hash-table :test #'equal) :type hash-table
                        :documentation "Serve memoization for
`comp-common-supertype'.")
  (subtype-p-mem (make-hash-table :test #'equal) :type hash-table
                 :documentation "Serve memoization for
`comp-subtype-p-mem'.")
  (union-1-mem-no-range (make-hash-table :test #'equal) :type hash-table
                        :documentation "Serve memoization for
`comp-cstr-union-1'.")
  (union-1-mem-range (make-hash-table :test #'equal) :type hash-table
                     :documentation "Serve memoization for
`comp-cstr-union-1'.")
  (intersection-mem (make-hash-table :test #'equal) :type hash-table
                    :documentation "Serve memoization for
`intersection-mem'."))

(defmacro with-comp-cstr-accessors (&rest body)
  "Define some quick accessor to reduce code vergosity in BODY."
  (declare (debug (form body))
           (indent defun))
  `(cl-macrolet ((typeset (x)
                          `(comp-cstr-typeset ,x))
                 (valset (x)
                         `(comp-cstr-valset ,x))
                 (range (x)
                        `(comp-cstr-range ,x))
                 (neg (x)
                      `(comp-cstr-neg ,x)))
     ,@body))

(defun comp-cstr-copy (cstr)
  "Return a deep copy of CSTR."
  (with-comp-cstr-accessors
    (make-comp-cstr :typeset (copy-sequence (typeset cstr))
                    :valset (copy-sequence (valset cstr))
                    :range (copy-tree (range cstr))
                    :neg (neg cstr))))

(defsubst comp-cstr-empty-p (cstr)
  "Return t if CSTR is equivalent to the nil type specifier or nil otherwise."
  (with-comp-cstr-accessors
    (and (null (typeset cstr))
         (null (valset cstr))
         (null (range cstr))
         (null (neg cstr)))))

(defsubst comp-cstr-null-p (cstr)
  "Return t if CSTR is equivalent to the `null' type specifier, nil otherwise."
  (with-comp-cstr-accessors
    (and (null (typeset cstr))
         (null (range cstr))
         (null (neg cstr))
         (equal (valset cstr) '(nil)))))

(defun comp-cstrs-homogeneous (cstrs)
  "Check if constraints CSTRS are all homogeneously negated or non-negated.
Return `pos' if they are all positive, `neg' if they are all
negated or nil otherwise."
  (cl-loop
   for cstr in cstrs
   unless (comp-cstr-neg cstr)
     count t into n-pos
   else
     count t into n-neg
   finally
   (cond
    ((zerop n-neg) (cl-return 'pos))
    ((zerop n-pos) (cl-return 'neg)))))

(defun comp-split-pos-neg (cstrs)
  "Split constraints CSTRS into non-negated and negated.
Return them as multiple value."
  (cl-loop
   for cstr in cstrs
   if (comp-cstr-neg cstr)
     collect cstr into negatives
   else
     collect cstr into positives
   finally return (cl-values positives negatives)))

;; So we can load comp-cstr.el and comp.el in non native compiled
;; builds.
(defvar comp-ctxt nil)

(defvar comp-cstr-one (comp-value-to-cstr 1)
  "Represent the integer immediate one.")

(defvar comp-cstr-t (comp-type-to-cstr t)
  "Represent the superclass t.")


;;; Value handling.

(defun comp-normalize-valset (valset)
  "Sort and remove duplicates from VALSET then return it."
  (cl-sort (cl-remove-duplicates valset :test #'eq)
           (lambda (x y)
             (cond
              ((and (symbolp x) (symbolp y))
               (string< x y))
              ((and (symbolp x) (not (symbolp y)))
               t)
              ((and (not (symbolp x)) (symbolp y))
               nil)
              (t
               (< (sxhash-equal x)
                  (sxhash-equal y)))))))

(defun comp-union-valsets (&rest valsets)
  "Union values present into VALSETS."
  (comp-normalize-valset (cl-reduce #'cl-union valsets)))

(defun comp-intersection-valsets (&rest valsets)
  "Union values present into VALSETS."
  (comp-normalize-valset (cl-reduce #'cl-intersection valsets)))


;;; Type handling.

(defun comp-normalize-typeset (typeset)
  "Sort TYPESET and return it."
  (cl-sort (cl-remove-duplicates typeset)
           (lambda (x y)
             (string-lessp (symbol-name x)
                           (symbol-name y)))))

(defun comp-supertypes (type)
  "Return a list of pairs (supertype . hierarchy-level) for TYPE."
  (cl-loop
   named outer
   with found = nil
   for l in comp--typeof-types
   do (cl-loop
       for x in l
       for i from (length l) downto 0
       when (eq type x)
         do (setf found t)
       when found
         collect `(,x . ,i) into res
       finally (when found
                 (cl-return-from outer res)))))

(defun comp-common-supertype-2 (type1 type2)
  "Return the first common supertype of TYPE1 TYPE2."
  (when-let ((types (cl-intersection
                     (comp-supertypes type1)
                     (comp-supertypes type2)
                     :key #'car)))
    (car (cl-reduce (lambda (x y)
                      (if (> (cdr x) (cdr y)) x y))
                    types))))

(defun comp-common-supertype (&rest types)
  "Return the first common supertype of TYPES."
  (or (gethash types (comp-cstr-ctxt-common-supertype-mem comp-ctxt))
      (puthash types
               (cl-reduce #'comp-common-supertype-2 types)
               (comp-cstr-ctxt-common-supertype-mem comp-ctxt))))

(defsubst comp-subtype-p (type1 type2)
  "Return t if TYPE1 is a subtype of TYPE2 or nil otherwise."
  (let ((types (cons type1 type2)))
    (or (gethash types (comp-cstr-ctxt-subtype-p-mem comp-ctxt))
        (puthash types
                 (eq (comp-common-supertype-2 type1 type2) type2)
                 (comp-cstr-ctxt-subtype-p-mem comp-ctxt)))))

(defun comp-union-typesets (&rest typesets)
  "Union types present into TYPESETS."
  (or (gethash typesets (comp-cstr-ctxt-union-typesets-mem comp-ctxt))
      (puthash typesets
               (cl-loop
                with types = (apply #'append typesets)
                with res = '()
                for lane in comp--typeof-types
                do (cl-loop
                    with last = nil
                    for x in lane
                    when (memq x types)
                      do (setf last x)
                    finally (when last
                              (push last res)))
                finally return (comp-normalize-typeset res))
               (comp-cstr-ctxt-union-typesets-mem comp-ctxt))))

(defun comp-intersect-two-typesets (t1 t2)
  "Intersect typesets T1 and T2."
  (with-comp-cstr-accessors
    (cl-loop
     for types in (list t1 t2)
     for other-types in (list t2 t1)
     append
     (cl-loop
      for type in types
      when (cl-some (lambda (x)
                      (comp-subtype-p type x))
                    other-types)
      collect type))))

(defun comp-intersect-typesets (&rest typesets)
  "Intersect types present into TYPESETS."
  (unless (cl-some #'null typesets)
    (if (length= typesets 1)
        (car typesets)
      (comp-normalize-typeset
       (cl-reduce #'comp-intersect-two-typesets typesets)))))


;;; Integer range handling

(defsubst comp-star-or-num-p (x)
  (or (numberp x) (eq '* x)))

(defsubst comp-range-1+ (x)
  (if (symbolp x)
      x
    (1+ x)))

(defsubst comp-range-1- (x)
  (if (symbolp x)
      x
    (1- x)))

(defsubst comp-range-+ (x y)
  (pcase (cons x y)
    ((or '(+ . -) '(- . +)) '??)
    ((or `(- . ,_) `(,_ . -)) '-)
    ((or `(+ . ,_) `(,_ . +)) '+)
    (_ (+ x y))))

(defsubst comp-range-- (x y)
  (pcase (cons x y)
    ((or '(+ . +) '(- . -)) '??)
    ('(+ . -) '+)
    ('(- . +) '-)
    ((or `(+ . ,_) `(,_ . -)) '+)
    ((or `(- . ,_) `(,_ . +)) '-)
    (_ (- x y))))

(defsubst comp-range-< (x y)
  (cond
   ((eq x '+) nil)
   ((eq x '-) t)
   ((eq y '+) t)
   ((eq y '-) nil)
   (t (< x y))))

(defsubst comp-cstr-smallest-in-range (range)
  "Smallest entry in RANGE."
  (caar range))

(defsubst comp-cstr-greatest-in-range (range)
  "Greater entry in RANGE."
  (cdar (last range)))

(defun comp-range-union (&rest ranges)
  "Combine integer intervals RANGES by union set operation."
  (cl-loop
   with all-ranges = (apply #'append ranges)
   with lows = (mapcar (lambda (x)
                         (cons (comp-range-1- (car x)) 'l))
                       all-ranges)
   with highs = (mapcar (lambda (x)
                          (cons (cdr x) 'h))
                        all-ranges)
   with nest = 0
   with low = nil
   with res = ()
   for (i . x) in (cl-sort (nconc lows highs) #'comp-range-< :key #'car)
   if (eq x 'l)
   do
   (when (zerop nest)
     (setf low i))
   (cl-incf nest)
   else
   do
   (when (= nest 1)
     (push `(,(comp-range-1+ low) . ,i) res))
   (cl-decf nest)
   finally return (reverse res)))

(defun comp-range-intersection (&rest ranges)
  "Combine integer intervals RANGES by intersecting."
  (cl-loop
   with all-ranges = (apply #'append ranges)
   with n-ranges = (length ranges)
   with lows = (mapcar (lambda (x)
                         (cons (car x) 'l))
                       all-ranges)
   with highs = (mapcar (lambda (x)
                          (cons (cdr x) 'h))
                        all-ranges)
   with nest = 0
   with low = nil
   with res = ()
   for (i . x) in (cl-sort (nconc lows highs) #'comp-range-< :key #'car)
   initially (when (cl-some #'null ranges)
               ;; Intersecting with a null range always results in a
               ;; null range.
               (cl-return '()))
   if (eq x 'l)
   do
   (cl-incf nest)
   (when (= nest n-ranges)
     (setf low i))
   else
   do
   (when (= nest n-ranges)
     (push `(,low . ,i)
           res))
   (cl-decf nest)
   finally return (reverse res)))

(defun comp-range-negation (range)
  "Negate range RANGE."
  (if (null range)
      '((- . +))
    (cl-loop
     with res = ()
     with last-h = '-
     for (l . h) in range
     unless (eq l '-)
     do (push `(,(comp-range-1+ last-h) . ,(1- l)) res)
     do (setf last-h h)
     finally
     (unless (eq '+ last-h)
       (push `(,(1+ last-h) . +) res))
     (cl-return (reverse res)))))

(defsubst comp-cstr-set-cmp-range (dst old-dst ext-range)
  "Support range comparison functions."
  (with-comp-cstr-accessors
    (if ext-range
        (setf (typeset dst) (when (cl-some (lambda (x)
                                             (comp-subtype-p 'float x))
                                           (typeset old-dst))
                                '(float))
              (valset dst) ()
              (range dst) (if (range old-dst)
                              (comp-range-intersection (range old-dst)
                                                       ext-range)
                            ext-range)
              (neg dst) nil)
      (setf (typeset dst) (typeset old-dst)
            (valset dst) (valset old-dst)
            (range dst) (range old-dst)
            (neg dst) (neg old-dst)))))

(defmacro comp-cstr-set-range-for-arithm (dst src1 src2 &rest range-body)
  ;; Prevent some code duplication for `comp-cstr-add-2'
  ;; `comp-cstr-sub-2'.
  (declare (debug (range-body))
           (indent defun))
  `(with-comp-cstr-accessors
     (if (or (neg src1) (neg src2))
         (setf (typeset ,dst) '(number))
       (when-let ((r1 (range ,src1))
                  (r2 (range ,src2)))
         (let* ((l1 (comp-cstr-smallest-in-range r1))
                (l2 (comp-cstr-smallest-in-range r2))
                (h1 (comp-cstr-greatest-in-range r1))
                (h2 (comp-cstr-greatest-in-range r2)))
           (setf (typeset ,dst) (when (cl-some (lambda (x)
                                                 (comp-subtype-p 'float x))
                                               (append (typeset src1)
                                                       (typeset src2)))
                                  '(float))
                 (range ,dst) ,@range-body))))))

(defun comp-cstr-add-2 (dst src1 src2)
  "Sum SRC1 and SRC2 into DST."
  (comp-cstr-set-range-for-arithm dst src1 src2
    `((,(comp-range-+ l1 l2) . ,(comp-range-+ h1 h2)))))

(defun comp-cstr-sub-2 (dst src1 src2)
  "Subtract SRC1 and SRC2 into DST."
  (comp-cstr-set-range-for-arithm dst src1 src2
    (let ((l (comp-range-- l1 h2))
          (h (comp-range-- h1 l2)))
      (if (or (eq l '??) (eq h '??))
          '((- . +))
        `((,l . ,h))))))


;;; Union specific code.

(defun comp-cstr-union-homogeneous-no-range (dst &rest srcs)
  "As `comp-cstr-union' but escluding the irange component.
All SRCS constraints must be homogeneously negated or non-negated."

  ;; Type propagation.
  (setf (comp-cstr-typeset dst)
        (apply #'comp-union-typesets (mapcar #'comp-cstr-typeset srcs)))

  ;; Value propagation.
  (setf (comp-cstr-valset dst)
        (comp-normalize-valset
         (cl-loop
          with values = (mapcar #'comp-cstr-valset srcs)
          ;; TODO sort.
          for v in (cl-remove-duplicates (apply #'append values)
                                         :test #'equal)
          ;; We propagate only values those types are not already
          ;; into typeset.
          when (cl-notany (lambda (x)
                            (comp-subtype-p (type-of v) x))
                          (comp-cstr-typeset dst))
          collect v)))

  dst)

(defun comp-cstr-union-homogeneous (range dst &rest srcs)
  "Combine SRCS by union set operation setting the result in DST.
Do range propagation when RANGE is non-nil.
All SRCS constraints must be homogeneously negated or non-negated.
DST is returned."
  (apply #'comp-cstr-union-homogeneous-no-range dst srcs)
  ;; Range propagation.
  (setf (comp-cstr-neg dst)
        (when srcs
          (comp-cstr-neg (car srcs)))

        (comp-cstr-range dst)
        (when (cl-notany (lambda (x)
                           (comp-subtype-p 'integer x))
                         (comp-cstr-typeset dst))
          (if range
              (apply #'comp-range-union
                     (mapcar #'comp-cstr-range srcs))
            '((- . +)))))
  dst)

(cl-defun comp-cstr-union-1-no-mem (range &rest srcs)
  "Combine SRCS by union set operation setting the result in DST.
Do range propagation when RANGE is non-nil.
Non memoized version of `comp-cstr-union-1'.
DST is returned."
  (with-comp-cstr-accessors
    (let ((dst (make-comp-cstr)))
      (cl-flet ((give-up ()
                         (setf (typeset dst) '(t)
                               (valset dst) ()
                               (range dst) ()
                               (neg dst) nil)
                         (cl-return-from comp-cstr-union-1-no-mem dst)))

        ;; Check first if we are in the simple case of all input non-negate
        ;; or negated so we don't have to cons.
        (when-let ((res (comp-cstrs-homogeneous srcs)))
          (apply #'comp-cstr-union-homogeneous range dst srcs)
          (cl-return-from comp-cstr-union-1-no-mem dst))

        ;; Some are negated and some are not
        (cl-multiple-value-bind (positives negatives) (comp-split-pos-neg srcs)
          (let* ((pos (apply #'comp-cstr-union-homogeneous range
                             (make-comp-cstr) positives))
                 ;; We'll always use neg as result as this is almost
                 ;; always necessary for describing open intervals
                 ;; resulting from negated constraints.
                 (neg (apply #'comp-cstr-union-homogeneous range
                             (make-comp-cstr :neg t) negatives)))
            ;; Type propagation.
            (when (and (typeset pos)
                       ;; When every pos type is a subtype of some neg ones.
                       (cl-every (lambda (x)
                                   (cl-some (lambda (y)
                                              (comp-subtype-p x y))
                                            (append (typeset neg)
                                                    (when (range neg)
                                                      '(integer)))))
                                 (typeset pos)))
              ;; This is a conservative choice, ATM we can't represent such
              ;; a disjoint set of types unless we decide to add a new slot
              ;; into `comp-cstr' or adopt something like
              ;; `intersection-type' `union-type' in SBCL.  Keep it
              ;; "simple" for now.
              (give-up))

            ;; When every neg type is a subtype of some pos one.
            ;; In case return pos.
            (when (and (typeset neg)
                       (cl-every (lambda (x)
                                   (cl-some (lambda (y)
                                              (comp-subtype-p x y))
                                            (append (typeset pos)
                                                    (when (range pos)
                                                      '(integer)))))
                                 (typeset neg)))
              (setf (typeset dst) (typeset pos)
                    (valset dst) (valset pos)
                    (range dst) (range pos)
                    (neg dst) nil)
              (cl-return-from comp-cstr-union-1-no-mem dst))

            ;; Verify disjoint condition between positive types and
            ;; negative types coming from values, in case give-up.
            (let ((neg-value-types (nconc (mapcar #'type-of (valset neg))
                                          (when (range neg)
                                            '(integer)))))
              (when (cl-some (lambda (x)
                               (cl-some (lambda (y)
                                          (and (not (eq y x))
                                               (comp-subtype-p y x)))
                                        neg-value-types))
                             (typeset pos))
                (give-up)))

            ;; Value propagation.
            (cond
             ((and (valset pos) (valset neg)
                   (equal (comp-union-valsets (valset pos) (valset neg))
                          (valset pos)))
              ;; Pos is a superset of neg.
              (give-up))
             ((cl-some (lambda (x)
                         (cl-some (lambda (y)
                                    (comp-subtype-p y x))
                                  (mapcar #'type-of (valset pos))))
                       (typeset neg))
              (give-up))
             (t
              ;; pos is a subset or eq to neg
              (setf (valset neg)
                    (cl-nset-difference (valset neg) (valset pos)))))

            ;; Range propagation
            (when range
              ;; Handle apart (or (integer 1 1) (not (integer 1 1)))
              ;; like cases.
              (if (and (range pos) (range neg)
                       (equal (range pos) (range neg)))
                  (give-up)
                (setf (range neg)
                      (comp-range-negation
                       (comp-range-union
                        (comp-range-negation (range neg))
                        (range pos))))))

            (if (comp-cstr-empty-p neg)
                (setf (typeset dst) (typeset pos)
                      (valset dst) (valset pos)
                      (range dst) (range pos)
                      (neg dst) nil)
              (setf (typeset dst) (typeset neg)
                    (valset dst) (valset neg)
                    (range dst) (range neg)
                    (neg dst) (neg neg)))))

        ;; (not null) => t
        (when (and (neg dst)
                   (null (typeset dst))
                   (null (valset dst))
                   (null (range dst)))
          (give-up)))

      dst)))

(defun comp-cstr-union-1 (range dst &rest srcs)
  "Combine SRCS by union set operation setting the result in DST.
Do range propagation when RANGE is non-nil.
DST is returned."
  (with-comp-cstr-accessors
    (let* ((mem-h (if range
                      (comp-cstr-ctxt-union-1-mem-range comp-ctxt)
                    (comp-cstr-ctxt-union-1-mem-no-range comp-ctxt)))
           (res (or (gethash srcs mem-h)
                    (puthash
                     (mapcar #'comp-cstr-copy srcs)
                     (apply #'comp-cstr-union-1-no-mem range srcs)
                     mem-h))))
      (setf (typeset dst) (typeset res)
            (valset dst) (valset res)
            (range dst) (range res)
            (neg dst) (neg res))
      res)))

(cl-defun comp-cstr-intersection-homogeneous (dst &rest srcs)
  "Combine SRCS by intersection set operation setting the result in DST.
All SRCS constraints must be homogeneously negated or non-negated.
DST is returned."

  (with-comp-cstr-accessors
    (when (cl-some #'comp-cstr-empty-p srcs)
      (setf (valset dst) nil
            (range dst) nil
            (typeset dst) nil)
      (cl-return-from comp-cstr-intersection-homogeneous dst))

    (setf (neg dst) (when srcs
                      (neg (car srcs))))

    ;; Type propagation.
    (setf (typeset dst)
          (apply #'comp-intersect-typesets
                 (mapcar #'comp-cstr-typeset srcs)))

    ;; Value propagation.
    (setf (valset dst)
          (comp-normalize-valset
           (cl-loop
            for src in srcs
            append
            (cl-loop
             for val in (valset src)
             ;; If (member value) is subtypep of all other sources then
             ;; is good to be colleted.
             when (cl-every (lambda (s)
                              (or (memql val (valset s))
                                  (cl-some (lambda (type)
                                             (cl-typep val type))
                                           (typeset s))))
                            (remq src srcs))
             collect val))))

    ;; Range propagation.
    (setf (range dst)
          ;; Do range propagation only if the destination typeset
          ;; doesn't cover it already.
          (unless (cl-some (lambda (type)
                             (comp-subtype-p 'integer type))
                           (typeset dst))
            (apply #'comp-range-intersection
                   (cl-loop
                    for src in srcs
                    ;; Collect effective ranges.
                    collect (or (range src)
                                (when (cl-some (lambda (s)
                                                 (comp-subtype-p 'integer s))
                                               (typeset src))
                                  '((- . +))))))))

    dst))

(cl-defun comp-cstr-intersection-no-mem (&rest srcs)
  "Combine SRCS by intersection set operation.
Non memoized version of `comp-cstr-intersection-no-mem'."
  (let ((dst (make-comp-cstr)))
    (with-comp-cstr-accessors
      (cl-flet ((return-empty ()
                              (setf (typeset dst) ()
                                    (valset dst) ()
                                    (range dst) ()
                                    (neg dst) nil)
                              (cl-return-from comp-cstr-intersection-no-mem dst)))
        (when-let ((res (comp-cstrs-homogeneous srcs)))
          (if (eq res 'neg)
              (apply #'comp-cstr-union-homogeneous t dst srcs)
            (apply #'comp-cstr-intersection-homogeneous dst srcs))
          (cl-return-from comp-cstr-intersection-no-mem dst))

        ;; Some are negated and some are not
        (cl-multiple-value-bind (positives negatives) (comp-split-pos-neg srcs)
          (let* ((pos (apply #'comp-cstr-intersection-homogeneous
                             (make-comp-cstr) positives))
                 (neg (apply #'comp-cstr-intersection-homogeneous
                             (make-comp-cstr) negatives)))

            ;; In case pos is not relevant return directly the content
            ;; of neg.
            (when (equal (typeset pos) '(t))
              (setf (typeset dst) (typeset neg)
                    (valset dst) (valset neg)
                    (range dst) (range neg)
                    (neg dst) t)

              ;; (not t) => nil
              (when (and (null (valset dst))
                         (null (range dst))
                         (neg dst)
                         (equal '(t) (typeset dst)))
                (setf (typeset dst) ()
                      (neg dst) nil))

              (cl-return-from comp-cstr-intersection-no-mem dst))

            (when (cl-some
                   (lambda (ty)
                     (memq ty (typeset neg)))
                   (typeset pos))
              (return-empty))

            ;; Some negated types are subtypes of some non-negated one.
            ;; Transform the corresponding set of types from neg to pos.
            (cl-loop
             for neg-type in (typeset neg)
             do (cl-loop
                 for pos-type in (copy-sequence (typeset pos))
                 when (and (not (eq neg-type pos-type))
                           (comp-subtype-p neg-type pos-type))
                   do (cl-loop
                       with found
                       for (type . _) in (comp-supertypes neg-type)
                       when found
                         collect type into res
                       when (eq type pos-type)
                         do (setf (typeset pos) (cl-union (typeset pos) res))
                            (cl-return)
                       when (eq type neg-type)
                         do (setf found t))))

            (setf (range pos)
                  (comp-range-intersection (range pos)
                                           (comp-range-negation (range neg)))
                  (valset pos)
                  (cl-set-difference (valset pos) (valset neg)))

            ;; Return a non negated form.
            (setf (typeset dst) (typeset pos)
                  (valset dst) (valset pos)
                  (range dst) (range pos)
                  (neg dst) nil)))
        dst))))


;;; Entry points.

(defun comp-cstr-imm-vld-p (cstr)
  "Return t if one and only one immediate value can be extracted from CSTR."
  (with-comp-cstr-accessors
    (when (and (null (typeset cstr))
               (null (neg cstr)))
      (let* ((v (valset cstr))
             (r (range cstr))
             (valset-len (length v))
             (range-len (length r)))
        (if (and (= valset-len 1)
                 (= range-len 0))
            t
          (when (and (= valset-len 0)
                     (= range-len 1))
            (let* ((low (caar r))
                   (high (cdar r)))
              (and (integerp low)
                   (integerp high)
                   (= low high)))))))))

(defun comp-cstr-imm (cstr)
  "Return the immediate value of CSTR.
`comp-cstr-imm-vld-p' *must* be satisfied before calling
`comp-cstr-imm'."
  (declare (gv-setter
            (lambda (val)
              `(with-comp-cstr-accessors
                 (if (integerp ,val)
                     (setf (typeset ,cstr) nil
                           (range ,cstr) (list (cons ,val ,val)))
                   (setf (typeset ,cstr) nil
                         (valset ,cstr) (list ,val)))))))
  (with-comp-cstr-accessors
    (let ((v (valset cstr)))
      (if (length= v 1)
          (car v)
        (caar (range cstr))))))

(defun comp-cstr-fixnum-p (cstr)
  "Return t if CSTR is certainly a fixnum."
  (with-comp-cstr-accessors
    (when (null (neg cstr))
      (when-let (range (range cstr))
        (let* ((low (caar range))
               (high (cdar (last range))))
          (unless (or (eq low '-)
                      (< low most-negative-fixnum)
                      (eq high '+)
                      (> high most-positive-fixnum))
            t))))))

(defun comp-cstr-symbol-p (cstr)
  "Return t if CSTR is certainly a symbol."
  (with-comp-cstr-accessors
    (and (null (range cstr))
         (null (neg cstr))
         (or (and (null (valset cstr))
                  (equal (typeset cstr) '(symbol)))
             (and (or (null (typeset cstr))
                      (equal (typeset cstr) '(symbol)))
                  (cl-every #'symbolp (valset cstr)))))))

(defsubst comp-cstr-cons-p (cstr)
  "Return t if CSTR is certainly a cons."
  (with-comp-cstr-accessors
    (and (null (valset cstr))
         (null (range cstr))
         (null (neg cstr))
         (equal (typeset cstr) '(cons)))))

(defun comp-cstr-= (dst op1 op2)
  "Constraint OP1 being = OP2 setting the result into DST."
  (with-comp-cstr-accessors
    (cl-flet ((relax-cstr (cstr)
                (setf cstr (comp-cstr-shallow-copy cstr))
                ;; If can be any float extend it to all integers.
                (when (memq 'float (typeset cstr))
                  (setf (range cstr) '((- . +))))
                ;; For each float value that can be represented
                ;; precisely as an integer add the integer as well.
                (cl-loop
                 for v in (valset cstr)
                 do
                 (when-let* ((ok (floatp v))
                             (truncated (ignore-error overflow-error
                                          (truncate v)))
                             (ok (= v truncated)))
                   (push (cons truncated truncated) (range cstr))))
                (cl-loop
                 with vals-to-add
                 for (l . h) in (range cstr)
                 ;; If an integer range reduces to single value add
                 ;; its float value too.
                 if (eql l h)
                   do (push (float l) vals-to-add)
                 ;; Otherwise can be any float.
                 else
                   do (cl-pushnew 'float (typeset cstr))
                      (cl-return cstr)
                 finally (setf (valset cstr)
                               (append vals-to-add (valset cstr))))
                (when (memql 0.0 (valset cstr))
                  (cl-pushnew -0.0 (valset cstr)))
                (when (memql -0.0 (valset cstr))
                  (cl-pushnew 0.0 (valset cstr)))
                cstr))
      (comp-cstr-intersection dst (relax-cstr op1) (relax-cstr op2)))))

(defun comp-cstr-> (dst old-dst src)
  "Constraint DST being > than SRC.
SRC can be either a comp-cstr or an integer."
  (with-comp-cstr-accessors
    (let ((ext-range
           (if (integerp src)
               `((,(1+ src) . +))
             (when-let* ((range (range src))
                         (low (comp-cstr-smallest-in-range range))
                         (okay (integerp low)))
               `((,(1+ low) . +))))))
      (comp-cstr-set-cmp-range dst old-dst ext-range))))

(defun comp-cstr->= (dst old-dst src)
  "Constraint DST being >= than SRC.
SRC can be either a comp-cstr or an integer."
  (with-comp-cstr-accessors
    (let ((ext-range
           (if (integerp src)
               `((,src . +))
             (when-let* ((range (range src))
                         (low (comp-cstr-smallest-in-range range))
                         (okay (integerp low)))
               `((,low . +))))))
      (comp-cstr-set-cmp-range dst old-dst ext-range))))

(defun comp-cstr-< (dst old-dst src)
  "Constraint DST being < than SRC.
SRC can be either a comp-cstr or an integer."
  (with-comp-cstr-accessors
    (let ((ext-range
           (if (integerp src)
               `((- . ,(1- src)))
             (when-let* ((range (range src))
                         (low (comp-cstr-greatest-in-range range))
                         (okay (integerp low)))
               `((- . ,(1- low)))))))
      (comp-cstr-set-cmp-range dst old-dst ext-range))))

(defun comp-cstr-<= (dst old-dst src)
  "Constraint DST being > than SRC.
SRC can be either a comp-cstr or an integer."
  (with-comp-cstr-accessors
    (let ((ext-range
           (if (integerp src)
               `((- . ,src))
             (when-let* ((range (range src))
                         (low (comp-cstr-greatest-in-range range))
                         (okay (integerp low)))
               `((- . ,low))))))
      (comp-cstr-set-cmp-range dst old-dst ext-range))))

(defun comp-cstr-add (dst srcs)
  "Sum SRCS into DST."
  (comp-cstr-add-2 dst (cl-first srcs) (cl-second srcs))
  (cl-loop
   for src in (nthcdr 2 srcs)
   do (comp-cstr-add-2 dst dst src)))

(defun comp-cstr-sub (dst srcs)
  "Subtract SRCS into DST."
  (comp-cstr-sub-2 dst (cl-first srcs) (cl-second srcs))
  (cl-loop
   for src in (nthcdr 2 srcs)
   do (comp-cstr-sub-2 dst dst src)))

(defun comp-cstr-union-no-range (dst &rest srcs)
  "Combine SRCS by union set operation setting the result in DST.
Do not propagate the range component.
DST is returned."
  (apply #'comp-cstr-union-1 nil dst srcs))

(defun comp-cstr-union (dst &rest srcs)
  "Combine SRCS by union set operation setting the result in DST.
DST is returned."
  (apply #'comp-cstr-union-1 t dst srcs))

(defun comp-cstr-union-make (&rest srcs)
  "Combine SRCS by union set operation and return a new constraint."
  (apply #'comp-cstr-union (make-comp-cstr) srcs))

(defun comp-cstr-intersection (dst &rest srcs)
  "Combine SRCS by intersection set operation setting the result in DST.
DST is returned."
  (with-comp-cstr-accessors
    (let* ((mem-h (comp-cstr-ctxt-intersection-mem comp-ctxt))
           (res (or (gethash srcs mem-h)
                    (puthash
                     (mapcar #'comp-cstr-copy srcs)
                     (apply #'comp-cstr-intersection-no-mem srcs)
                     mem-h))))
      (setf (typeset dst) (typeset res)
            (valset dst) (valset res)
            (range dst) (range res)
            (neg dst) (neg res))
      res)))

(defun comp-cstr-intersection-no-hashcons (dst &rest srcs)
  "Combine SRCS by intersection set operation setting the result in DST.
Non hash consed values are not propagated as values but rather
promoted to their types.
DST is returned."
  (with-comp-cstr-accessors
    (apply #'comp-cstr-intersection dst srcs)
    (if (and (neg dst)
             (valset dst)
             (cl-notevery #'symbolp (valset dst)))
        (setf (valset dst) ()
              (typeset dst) '(t)
              (range dst) ()
              (neg dst) nil)
      (let (strip-values strip-types)
        (cl-loop for v in (valset dst)
                 unless (symbolp v)
                   do (push v strip-values)
                      (push (type-of v) strip-types))
        (when strip-values
          (setf (typeset dst) (comp-union-typesets (typeset dst) strip-types)
                (valset dst) (cl-set-difference (valset dst) strip-values)))
        (cl-loop for (l . h) in (range dst)
                 when (or (bignump l) (bignump h))
                 do (setf (range dst) '((- . +)))
                    (cl-return))))
    dst))

(defun comp-cstr-intersection-make (&rest srcs)
  "Combine SRCS by intersection set operation and return a new constraint."
  (apply #'comp-cstr-intersection (make-comp-cstr) srcs))

(defun comp-cstr-negation (dst src)
  "Negate SRC setting the result in DST.
DST is returned."
  (with-comp-cstr-accessors
    (cond
     ((and (null (valset src))
           (null (range src))
           (null (neg src))
           (equal (typeset src) '(t)))
      (setf (typeset dst) ()
            (valset dst) ()
            (range dst) nil
            (neg dst) nil))
     ((and (null (valset src))
           (null (range src))
           (null (neg src))
           (null (typeset src)))
      (setf (typeset dst) '(t)
            (valset dst) ()
            (range dst) nil
            (neg dst) nil))
     (t (setf (typeset dst) (typeset src)
              (valset dst) (valset src)
              (range dst) (range src)
              (neg dst) (not (neg src)))))
    dst))

(defun comp-cstr-value-negation (dst src)
  "Negate values in SRC setting the result in DST.
DST is returned."
  (with-comp-cstr-accessors
    (if (or (valset src) (range src))
        (setf (typeset dst) ()
              (valset dst) (valset src)
              (range dst) (range src)
              (neg dst) (not (neg src)))
      (setf (typeset dst) (typeset src)
            (valset dst) ()
            (range dst) ()))
    dst))

(defun comp-cstr-negation-make (src)
  "Negate SRC and return a new constraint."
  (comp-cstr-negation (make-comp-cstr) src))

(defun comp-type-spec-to-cstr (type-spec &optional fn)
  "Convert a type specifier TYPE-SPEC into a `comp-cstr'.
FN non-nil indicates we are parsing a function lambda list."
  (pcase type-spec
    ((and (or '&optional '&rest) x)
     (if fn
         x
       (error "Invalid `%s` in type specifier" x)))
    ('nil
     (make-comp-cstr :typeset ()))
    ('fixnum
     (comp-irange-to-cstr `(,most-negative-fixnum . ,most-positive-fixnum)))
    ('boolean
     (comp-type-spec-to-cstr '(member t nil)))
    ('integer
     (comp-irange-to-cstr '(- . +)))
    ('null (comp-value-to-cstr nil))
    ((pred atom)
     (comp-type-to-cstr type-spec))
    (`(or . ,rest)
     (apply #'comp-cstr-union-make
            (mapcar #'comp-type-spec-to-cstr rest)))
    (`(and . ,rest)
     (apply #'comp-cstr-intersection-make
            (mapcar #'comp-type-spec-to-cstr rest)))
    (`(not  ,cstr)
     (comp-cstr-negation-make (comp-type-spec-to-cstr cstr)))
    (`(integer ,(and (pred integerp) l) ,(and (pred integerp) h))
     (comp-irange-to-cstr `(,l . ,h)))
    (`(integer * ,(and (pred integerp) h))
     (comp-irange-to-cstr `(- . ,h)))
    (`(integer ,(and (pred integerp) l) *)
     (comp-irange-to-cstr `(,l . +)))
    (`(float ,(pred comp-star-or-num-p) ,(pred comp-star-or-num-p))
     ;; No float range support :/
     (comp-type-to-cstr 'float))
    (`(member . ,rest)
     (apply #'comp-cstr-union-make (mapcar #'comp-value-to-cstr rest)))
    (`(function ,args ,ret)
     (make-comp-cstr-f
      :args (mapcar (lambda (x)
                      (comp-type-spec-to-cstr x t))
                    args)
      :ret (comp-type-spec-to-cstr ret)))
    (_ (error "Invalid type specifier"))))

(defun comp-cstr-to-type-spec (cstr)
  "Given CSTR return its type specifier."
  (let ((valset (comp-cstr-valset cstr))
        (typeset (comp-cstr-typeset cstr))
        (range (comp-cstr-range cstr))
        (negated (comp-cstr-neg cstr)))

    (when valset
      (when (memq nil valset)
        (if (memq t valset)
            (progn
              ;; t and nil are values, convert into `boolean'.
              (push 'boolean typeset)
              (setf valset (remove t (remove nil valset))))
          ;; Only nil is a value, convert it into a `null' type specifier.
          (setf valset (remove nil valset))
          (push 'null typeset))))

    ;; Form proper integer type specifiers.
    (setf range (cl-loop for (l . h) in range
                         for low = (if (integerp l) l '*)
                         for high = (if (integerp h) h '*)
                         if (and (eq low '*) (eq high '*))
                           collect 'integer
                         else
                           collect `(integer ,low , high))
          valset (cl-remove-duplicates valset))

    ;; Form the final type specifier.
    (let* ((types-ints (append typeset range))
           (res (cond
                 ((and types-ints valset)
                  `((member ,@valset) ,@types-ints))
                 (types-ints types-ints)
                 (valset `(member ,@valset))
                 (t
                  ;; Empty type specifier
                  nil)))
           (final
            (pcase res
              ((or `(member . ,rest)
                   `(integer ,(pred comp-star-or-num-p)
                             ,(pred comp-star-or-num-p)))
               (if rest
                   res
                 (car res)))
              ((pred atom) res)
              (`(,_first . ,rest)
               (if rest
                   `(or ,@res)
                 (car res))))))
      (if negated
          `(not ,final)
        final))))

(provide 'comp-cstr)

;;; comp-cstr.el ends here
