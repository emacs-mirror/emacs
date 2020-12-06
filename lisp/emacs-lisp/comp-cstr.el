;;; comp-cstr.el --- native compiler constraint library -*- lexical-binding: t -*-

;; Author: Andrea Corallo <akrl@sdf.com>

;; Copyright (C) 2020 Free Software Foundation, Inc.

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
                                       (type &aux (typeset (list type))))
                         (:constructor comp-value-to-cstr
                                       (value &aux
                                              (valset (list value))
                                              (typeset ())))
                         (:constructor comp-irange-to-cstr
                                       (irange &aux
                                               (range (list irange))
                                               (typeset ())))
                         (:copier nil))
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
  (union-1-mem-no-range (make-hash-table :test #'equal) :type hash-table
                        :documentation "Serve memoization for
`comp-cstr-union-1'.")
  (union-1-mem-range (make-hash-table :test #'equal) :type hash-table
                     :documentation "Serve memoization for
`comp-cstr-union-1'."))

(defmacro with-comp-cstr-accessors (&rest body)
  "Define some quick accessor to reduce code vergosity in BODY."
  (declare (debug (form body))
           (indent defun))
  `(cl-macrolet ((typeset (&rest x)
                          `(comp-cstr-typeset ,@x))
                 (valset (&rest x)
                         `(comp-cstr-valset ,@x))
                 (range (&rest x)
                        `(comp-cstr-range ,@x))
                 (neg (&rest x)
                      `(comp-cstr-neg ,@x)))
     ,@body))

(defun comp-cstr-copy (cstr)
  "Return a deep copy of CSTR."
  (with-comp-cstr-accessors
    (make-comp-cstr :typeset (copy-tree (typeset cstr))
                    :valset (copy-tree (valset cstr))
                    :range (copy-tree (range cstr))
                    :neg (copy-tree (neg cstr)))))


;;; Type handling.

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
  (eq (comp-common-supertype-2 type1 type2) type2))

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
                ;; TODO sort.
                finally (cl-return (cl-remove-duplicates res)))
               (comp-cstr-ctxt-union-typesets-mem comp-ctxt))))

(defun comp-intersect-typesets (&rest typesets)
  "Intersect types present into TYPESETS."
  (when-let ((ty (apply #'append typesets)))
    (if (> (length ty) 1)
        (cl-reduce
         (lambda (x y)
           (let ((st (comp-common-supertype-2 x y)))
             (cond
              ((eq st x) (list y))
              ((eq st y) (list x)))))
         ty)
      ty)))


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

(defsubst comp-range-< (x y)
  (cond
   ((eq x '+) nil)
   ((eq x '-) t)
   ((eq y '+) t)
   ((eq y '-) nil)
   (t (< x y))))

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
   finally (cl-return (reverse res))))

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
   initially (when (cl-some #'null ranges)
               ;; Intersecting with a null range always results in a
               ;; null range.
               (cl-return '()))
   for (i . x) in (cl-sort (nconc lows highs) #'comp-range-< :key #'car)
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
   finally (cl-return (reverse res))))

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


;;; Union specific code.

(defun comp-cstr-union-homogeneous-no-range (dst &rest srcs)
  "As `comp-cstr-union' but escluding the irange component.
All SRCS constraints must be homogeneously negated or non-negated."

  ;; Type propagation.
  (setf (comp-cstr-typeset dst)
        (apply #'comp-union-typesets (mapcar #'comp-cstr-typeset srcs)))

  ;; Value propagation.
  (setf (comp-cstr-valset dst)
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
         collect v))

  dst)

(defun comp-cstr-union-homogeneous (dst &rest srcs)
  "Combine SRCS by union set operation setting the result in DST.
All SRCS constraints must be homogeneously negated or non-negated.
DST is returned."
  (apply #'comp-cstr-union-homogeneous-no-range dst srcs)
  ;; Range propagation.
  (setf (comp-cstr-range dst)
        (when (cl-notany (lambda (x)
                           (comp-subtype-p 'integer x))
                         (comp-cstr-typeset dst))
          ;; TODO memoize?
          (apply #'comp-range-union
                 (mapcar #'comp-cstr-range srcs))))
  dst)

(cl-defun comp-cstr-union-1-no-mem (range dst &rest srcs)
  "Combine SRCS by union set operation setting the result in DST.
Do range propagation when RANGE is non-nil.
Non memoized version of `comp-cstr-union-1'.
DST is returned."
  (with-comp-cstr-accessors
    (cl-flet ((give-up ()
                (setf (typeset dst) '(t)
                      (valset dst) ()
                      (range dst) ()
                      (neg dst) nil)
                (cl-return-from comp-cstr-union-1-no-mem dst)))

      ;; Check first if we are in the simple case of all input non-negate
      ;; or negated so we don't have to cons.
      (cl-loop
       for cstr in srcs
       unless (neg cstr)
         count t into n-pos
       else
         count t into n-neg
       finally
       (when (or (zerop n-pos) (zerop n-neg))
         (apply #'comp-cstr-union-homogeneous dst srcs)
         (when (zerop n-pos)
           (setf (neg dst) t))
         (cl-return-from comp-cstr-union-1-no-mem dst)))

      ;; Some are negated and some are not
      (cl-loop
       for cstr in srcs
       if (neg cstr)
         collect cstr into negatives
       else
         collect cstr into positives
       finally
       (let* ((pos (apply #'comp-cstr-union-homogeneous
                          (make-comp-cstr) positives))
              ;; We use neg as result as *most* of times this will be
              ;; negated.
              (neg (apply #'comp-cstr-union-homogeneous
                          (make-comp-cstr :neg t) negatives)))
         ;; Type propagation.
         (when (and (typeset pos)
                    ;; When every pos type is not a subtype of some neg ones.
                    (cl-every (lambda (x)
                                (cl-some (lambda (y)
                                           (not (and (not (eq x y))
                                                     (comp-subtype-p x y))))
                                         (typeset neg)))
                              (typeset pos)))
           ;; This is a conservative choice, ATM we can't represent such
           ;; a disjoint set of types unless we decide to add a new slot
           ;; into `comp-cstr' or adopt something like
           ;; `intersection-type' `union-type' in SBCL.  Keep it
           ;; "simple" for now.
           (give-up))

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
                (equal (cl-union (valset pos) (valset neg)) (valset pos)))
           ;; Pos is a superset of neg.
           (give-up))
          (t
           ;; pos is a subset or eq to neg
           (setf (valset neg)
                 (cl-nset-difference (valset neg) (valset pos)))))

         ;; Range propagation
         (if (and range
                  (or (range pos)
                      (range neg)))
             (if (or (valset neg) (typeset neg))
                 (setf (range neg)
                       (if (memq 'integer (typeset neg))
                           (comp-range-negation (range pos))
                         (comp-range-negation
                          (comp-range-union (range pos)
                                            (comp-range-negation (range neg))))))
               ;; When possibile do not return a negated cstr.
               (setf (typeset dst) (typeset pos)
                     (valset dst) (valset pos)
                     (range dst) (unless (memq 'integer (typeset dst))
                                   (comp-range-union
                                    (comp-range-negation (range neg))
                                    (range pos)))
                     (neg dst) nil)
               (cl-return-from comp-cstr-union-1-no-mem dst))
           (setf (range neg) ()))

         (if (and (null (typeset neg))
                  (null (valset neg))
                  (null (range neg)))
             (setf (typeset dst) (typeset pos)
                   (valset dst) (valset pos)
                   (range dst) (range pos)
                   (neg dst) nil)
           (setf (typeset dst) (typeset neg)
                 (valset dst) (valset neg)
                 (range dst) (range neg)
                 (neg dst) (neg neg))))))
    dst))

(defun comp-cstr-union-1 (range dst &rest srcs)
  "Combine SRCS by union set operation setting the result in DST.
Do range propagation when RANGE is non-nil.
DST is returned."
  (let ((mem-h (if range
                   (comp-cstr-ctxt-union-1-mem-range comp-ctxt)
                 (comp-cstr-ctxt-union-1-mem-no-range comp-ctxt))))
    (with-comp-cstr-accessors
      (if-let ((mem-res (gethash srcs mem-h)))
          (progn
            (setf (typeset dst) (typeset mem-res)
                  (valset dst) (valset mem-res)
                  (range dst) (range mem-res)
                  (neg dst) (neg mem-res))
            mem-res)
        (let ((res (apply #'comp-cstr-union-1-no-mem range dst srcs)))
          (puthash srcs (comp-cstr-copy res) mem-h)
         res)))))


;;; Entry points.

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

;; TODO memoize
(cl-defun comp-cstr-intersection (dst &rest srcs)
  "Combine SRCS by intersection set operation setting the result in DST.
DST is returned."

  ;; Value propagation.
  (setf (comp-cstr-valset dst)
        ;; TODO sort.
        (let ((values (cl-loop for src in srcs
                               for v = (comp-cstr-valset src)
                               when v
                               collect v)))
          (when values
            (cl-reduce (lambda (x y)
                         (cl-intersection x y :test #'equal))
                       values))))

  ;; Range propagation.
  (when (cl-some #'identity (mapcar #'comp-cstr-range srcs))
    (if (comp-cstr-valset dst)
        (progn
          (setf (comp-cstr-valset dst) nil
                (comp-cstr-range dst) nil
                (comp-cstr-typeset dst) nil)
          (cl-return-from comp-cstr-intersection dst))
      ;; TODO memoize?
      (setf  (comp-cstr-range dst)
             (apply #'comp-range-intersection
                    (mapcar #'comp-cstr-range srcs)))))

  ;; Type propagation.
  (setf (comp-cstr-typeset dst)
        (if (or (comp-cstr-range dst) (comp-cstr-valset dst))
            (cl-loop
             with type-val = (cl-remove-duplicates
                              (append (mapcar #'type-of
                                              (comp-cstr-valset dst))
                                      (when (comp-cstr-range dst)
                                        '(integer))))
             for type in (apply #'comp-intersect-typesets
                                (mapcar #'comp-cstr-typeset srcs))
             when (and type (not (member type type-val)))
               do (setf (comp-cstr-valset dst) nil
                        (comp-cstr-range dst) nil)
                  (cl-return nil))
          (apply #'comp-intersect-typesets
                 (mapcar #'comp-cstr-typeset srcs))))
  dst)

(defun comp-cstr-intersection-make (&rest srcs)
  "Combine SRCS by intersection set operation and return a new constraint."
  (apply #'comp-cstr-intersection (make-comp-cstr) srcs))

(defun comp-cstr-negation (dst src)
  "Negate SRC setting the result in DST.
DST is returned."
  (setf (comp-cstr-typeset dst) (comp-cstr-typeset src)
        (comp-cstr-valset dst) (comp-cstr-valset src)
        (comp-cstr-range dst) (comp-cstr-range src)
        (comp-cstr-neg dst) (not (comp-cstr-neg src)))
  dst)

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
    ('fixnum
     (comp-irange-to-cstr `(,most-negative-fixnum . ,most-positive-fixnum)))
    ('boolean
     (comp-type-spec-to-cstr '(member t nil)))
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
