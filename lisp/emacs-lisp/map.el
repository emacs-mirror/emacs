;;; map.el --- Map manipulation functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2020 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: convenience, map, hash-table, alist, array
;; Version: 2.0
;; Package-Requires: ((emacs "25"))
;; Package: map

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; map.el provides map-manipulation functions that work on alists,
;; hash-table and arrays.  All functions are prefixed with "map-".
;;
;; Functions taking a predicate or iterating over a map using a
;; function take the function as their first argument.  All other
;; functions take the map as their first argument.

;; TODO:
;; - Add support for char-tables
;; - Maybe add support for gv?
;; - See if we can integrate text-properties
;; - A macro similar to let-alist but working on any type of map could
;;   be really useful

;;; Code:

(require 'seq)
(eval-when-compile (require 'cl-lib))

(pcase-defmacro map (&rest args)
  "Build a `pcase' pattern matching map elements.

ARGS is a list of elements to be matched in the map.

Each element of ARGS can be of the form (KEY PAT), in which case KEY is
evaluated and searched for in the map.  The match fails if for any KEY
found in the map, the corresponding PAT doesn't match the value
associated to the KEY.

Each element can also be a SYMBOL, which is an abbreviation of a (KEY
PAT) tuple of the form (\\='SYMBOL SYMBOL).

Keys in ARGS not found in the map are ignored, and the match doesn't
fail."
  `(and (pred mapp)
        ,@(map--make-pcase-bindings args)))

(defmacro map-let (keys map &rest body)
  "Bind the variables in KEYS to the elements of MAP then evaluate BODY.

KEYS can be a list of symbols, in which case each element will be
bound to the looked up value in MAP.

KEYS can also be a list of (KEY VARNAME) pairs, in which case
KEY is an unquoted form.

MAP can be a list, hash-table or array."
  (declare (indent 2)
           (debug ((&rest &or symbolp ([form symbolp])) form body)))
  `(pcase-let ((,(map--make-pcase-patterns keys) ,map))
     ,@body))

(eval-when-compile
  (defmacro map--dispatch (map-var &rest args)
    "Evaluate one of the forms specified by ARGS based on the type of MAP-VAR.

The following keyword types are meaningful: `:list',
`:hash-table' and `:array'.

An error is thrown if MAP-VAR is neither a list, hash-table nor array.

Returns the result of evaluating the form associated with MAP-VAR's type."
    (declare (debug t) (indent 1))
    `(cond ((listp ,map-var) ,(plist-get args :list))
           ((hash-table-p ,map-var) ,(plist-get args :hash-table))
           ((arrayp ,map-var) ,(plist-get args :array))
           (t (error "Unsupported map type `%S': %S"
                     (type-of ,map-var) ,map-var)))))

(define-error 'map-not-inplace "Cannot modify map in-place")

(defsubst map--plist-p (list)
  (and (consp list) (not (listp (car list)))))

(cl-defgeneric map-elt (map key &optional default testfn)
  "Lookup KEY in MAP and return its associated value.
If KEY is not found, return DEFAULT which defaults to nil.

TESTFN is deprecated.  Its default depends on the MAP argument.

In the base definition, MAP can be an alist, hash-table, or array."
  (declare
   (gv-expander
    (lambda (do)
      (gv-letplace (mgetter msetter) `(gv-delay-error ,map)
        (macroexp-let2* nil
            ;; Eval them once and for all in the right order.
            ((key key) (default default) (testfn testfn))
          (funcall do `(map-elt ,mgetter ,key ,default)
                   (lambda (v)
                     `(condition-case nil
                          ;; Silence warnings about the hidden 4th arg.
                          (with-no-warnings (map-put! ,mgetter ,key ,v ,testfn))
                        (map-not-inplace
                         ,(funcall msetter
                                   `(map-insert ,mgetter ,key ,v))))))))))
   ;; `testfn' is deprecated.
   (advertised-calling-convention (map key &optional default) "27.1"))
  (map--dispatch map
    :list (if (map--plist-p map)
              (let ((res (plist-get map key)))
                (if (and default (null res) (not (plist-member map key)))
                    default
                  res))
            (alist-get key map default nil testfn))
    :hash-table (gethash key map default)
    :array (if (and (>= key 0) (< key (seq-length map)))
               (seq-elt map key)
             default)))

(defmacro map-put (map key value &optional testfn)
  "Associate KEY with VALUE in MAP and return VALUE.
If KEY is already present in MAP, replace the associated value
with VALUE.
When MAP is a list, test equality with TESTFN if non-nil,
otherwise use `eql'.

MAP can be a list, hash-table or array."
  (declare (obsolete "use map-put! or (setf (map-elt ...) ...) instead" "27.1"))
  `(setf (map-elt ,map ,key nil ,testfn) ,value))

(defun map--plist-delete (map key)
  (let ((tail map) last)
    (while (consp tail)
      (cond
       ((not (equal key (car tail)))
        (setq last tail)
        (setq tail (cddr last)))
       (last
        (setq tail (cddr tail))
        (setf (cddr last) tail))
       (t
        (cl-assert (eq tail map))
        (setq map (cddr map))
        (setq tail map))))
    map))

(cl-defgeneric map-delete (map key)
  "Delete KEY in-place from MAP and return MAP.
No error is signaled if KEY is not a key of MAP.
If MAP is an array, store nil at the index KEY."
  (map--dispatch map
    ;; FIXME: Signal map-not-inplace i.s.o returning a different list?
    :list (if (map--plist-p map)
              (setq map (map--plist-delete map key))
            (setf (alist-get key map nil t) nil))
    :hash-table (remhash key map)
    :array (and (>= key 0)
                (<= key (seq-length map))
                (aset map key nil)))
  map)

(defun map-nested-elt (map keys &optional default)
  "Traverse MAP using KEYS and return the looked up value or DEFAULT if nil.

Map can be a nested map composed of alists, hash-tables and arrays."
  (or (seq-reduce (lambda (acc key)
                    (when (mapp acc)
                      (map-elt acc key)))
                  keys
                  map)
      default))

(cl-defgeneric map-keys (map)
  "Return the list of keys in MAP.
The default implementation delegates to `map-apply'."
  (map-apply (lambda (key _) key) map))

(cl-defgeneric map-values (map)
  "Return the list of values in MAP.
The default implementation delegates to `map-apply'."
  (map-apply (lambda (_ value) value) map))

(cl-defgeneric map-pairs (map)
  "Return the elements of MAP as key/value association lists.
The default implementation delegates to `map-apply'."
  (map-apply #'cons map))

(cl-defgeneric map-length (map)
  ;; FIXME: Should we rename this to `map-size'?
  "Return the number of elements in the map.
The default implementation counts `map-keys'."
  (cond
   ((hash-table-p map) (hash-table-count map))
   ((listp map)
    ;; FIXME: What about repeated/shadowed keys?
    (if (map--plist-p map) (/ (length map) 2) (length map)))
   ((arrayp map) (length map))
   (t (length (map-keys map)))))

(cl-defgeneric map-copy (map)
  "Return a copy of MAP."
  ;; FIXME: Clarify how deep is the copy!
  (map--dispatch map
    :list (seq-copy map)           ;FIXME: Probably not deep enough for alists!
    :hash-table (copy-hash-table map)
    :array (seq-copy map)))

(cl-defgeneric map-apply (function map)
  "Apply FUNCTION to each element of MAP and return the result as a list.
FUNCTION is called with two arguments, the key and the value.
The default implementation delegates to `map-do'."
  (let ((res '()))
    (map-do (lambda (k v) (push (funcall function k v) res)) map)
    (nreverse res)))

(cl-defgeneric map-do (function map)
  "Apply FUNCTION to each element of MAP and return nil.
FUNCTION is called with two arguments, the key and the value.")

;; FIXME: I wish there was a way to avoid this η-redex!
(cl-defmethod map-do (function (map hash-table)) (maphash function map))

(cl-defgeneric map-keys-apply (function map)
  "Return the result of applying FUNCTION to each key of MAP.
The default implementation delegates to `map-apply'."
  (map-apply (lambda (key _)
               (funcall function key))
             map))

(cl-defgeneric map-values-apply (function map)
  "Return the result of applying FUNCTION to each value of MAP.
The default implementation delegates to `map-apply'."
  (map-apply (lambda (_ val)
               (funcall function val))
             map))

(cl-defgeneric map-filter (pred map)
  "Return an alist of key/val pairs for which (PRED key val) is non-nil in MAP.
The default implementation delegates to `map-apply'."
  (delq nil (map-apply (lambda (key val)
                         (if (funcall pred key val)
                             (cons key val)
                           nil))
                       map)))

(cl-defgeneric map-remove (pred map)
  "Return an alist of the key/val pairs for which (PRED key val) is nil in MAP.
The default implementation delegates to `map-filter'."
  (map-filter (lambda (key val) (not (funcall pred key val)))
              map))

(cl-defgeneric mapp (map)
  "Return non-nil if MAP is a map (alist, hash-table, array, ...)."
  (or (listp map)
      (hash-table-p map)
      (arrayp map)))

(cl-defgeneric map-empty-p (map)
  "Return non-nil if MAP is empty.
The default implementation delegates to `map-length'."
  (zerop (map-length map)))

(cl-defmethod map-empty-p ((map list))
  (null map))

(cl-defgeneric map-contains-key (map key &optional testfn)
  ;; FIXME: The test function to use generally depends on the map object,
  ;; so specifying `testfn' here is problematic: e.g. for hash-tables
  ;; we shouldn't use `gethash' unless `testfn' is the same as the map's own
  ;; test function!
  "Return non-nil if and only if MAP contains KEY.
TESTFN is deprecated.  Its default depends on MAP.
The default implementation delegates to `map-do'."
  (unless testfn (setq testfn #'equal))
  (catch 'map--catch
    (map-do (lambda (k _v)
              (if (funcall testfn key k) (throw 'map--catch t)))
            map)
    nil))

(cl-defmethod map-contains-key ((map list) key &optional testfn)
  (let ((v '(nil)))
    (not (eq v (alist-get key map v nil (or testfn #'equal))))))

(cl-defmethod map-contains-key ((map array) key &optional _testfn)
  (and (integerp key)
       (>= key 0)
       (< key (length map))))

(cl-defmethod map-contains-key ((map hash-table) key &optional _testfn)
  (let ((v '(nil)))
    (not (eq v (gethash key map v)))))

(cl-defgeneric map-some (pred map)
  "Return the first non-nil (PRED key val) in MAP.
The default implementation delegates to `map-apply'."
  ;; FIXME: Not sure if there's much benefit to defining it as defgeneric,
  ;; since as defined, I can't think of a map-type where we could provide an
  ;; algorithmically more efficient algorithm than the default.
  (catch 'map--break
    (map-apply (lambda (key value)
                 (let ((result (funcall pred key value)))
                   (when result
                     (throw 'map--break result))))
               map)
    nil))

(cl-defgeneric map-every-p (pred map)
  "Return non-nil if (PRED key val) is non-nil for all elements of MAP.
The default implementation delegates to `map-apply'."
  ;; FIXME: Not sure if there's much benefit to defining it as defgeneric,
  ;; since as defined, I can't think of a map-type where we could provide an
  ;; algorithmically more efficient algorithm than the default.
  (catch 'map--break
    (map-apply (lambda (key value)
              (or (funcall pred key value)
                  (throw 'map--break nil)))
            map)
    t))

(defun map-merge (type &rest maps)
  "Merge into a map of type TYPE all the key/value pairs in MAPS.
See `map-into' for all supported values of TYPE."
  (let ((result (map-into (pop maps) type)))
    (while maps
      ;; FIXME: When `type' is `list', we get an O(N^2) behavior.
      ;; For small tables, this is fine, but for large tables, we
      ;; should probably use a hash-table internally which we convert
      ;; to an alist in the end.
      (map-apply (lambda (key value)
                   (setf (map-elt result key) value))
                 (pop maps)))
    result))

(defun map-merge-with (type function &rest maps)
  "Merge into a map of type TYPE all the key/value pairs in MAPS.
When two maps contain the same key (`eql'), call FUNCTION on the two
values and use the value returned by it.
MAP can be a list, hash-table or array.
See `map-into' for all supported values of TYPE."
  (let ((result (map-into (pop maps) type))
        (not-found (cons nil nil)))
    (while maps
      (map-apply (lambda (key value)
                   (cl-callf (lambda (old)
                               (if (eql old not-found)
                                   value
                                 (funcall function old value)))
                       (map-elt result key not-found)))
                 (pop maps)))
    result))

(cl-defgeneric map-into (map type)
  "Convert the map MAP into a map of type TYPE.")
;; FIXME: I wish there was a way to avoid this η-redex!
(cl-defmethod map-into (map (_type (eql list))) (map-pairs map))
(cl-defmethod map-into (map (_type (eql alist))) (map-pairs map))
(cl-defmethod map-into (map (_type (eql plist)))
  (let ((plist '()))
    (map-do (lambda (k v) (setq plist `(,k ,v ,@plist))) map)
    plist))

(cl-defgeneric map-put! (map key value &optional testfn)
  "Associate KEY with VALUE in MAP.
If KEY is already present in MAP, replace the associated value
with VALUE.
This operates by modifying MAP in place.
If it cannot do that, it signals the `map-not-inplace' error.
If you want to insert an element without modifying MAP, use `map-insert'."
  ;; `testfn' only exists for backward compatibility with `map-put'!
  (declare (advertised-calling-convention (map key value) "27.1"))
  (map--dispatch map
    :list
    (if (map--plist-p map)
        (plist-put map key value)
      (let ((oldmap map))
        (setf (alist-get key map key nil (or testfn #'equal)) value)
        (unless (eq oldmap map)
          (signal 'map-not-inplace (list oldmap)))))
    :hash-table (puthash key value map)
    ;; FIXME: If `key' is too large, should we signal `map-not-inplace'
    ;; and let `map-insert' grow the array?
    :array (aset map key value)))

(define-error 'map-inplace "Can only modify map in place")

(cl-defgeneric map-insert (map key value)
  "Return a new map like MAP except that it associates KEY with VALUE.
This does not modify MAP.
If you want to insert an element in place, use `map-put!'."
  (if (listp map)
      (if (map--plist-p map)
          `(,key ,value ,@map)
        (cons (cons key value) map))
    ;; FIXME: Should we signal an error or use copy+put! ?
    (signal 'map-inplace (list map))))

;; There shouldn't be old source code referring to `map--put', yet we do
;; need to keep it for backward compatibility with .elc files where the
;; expansion of `setf' may call this function.
(define-obsolete-function-alias 'map--put #'map-put! "27.1")

(cl-defmethod map-apply (function (map list))
  (if (map--plist-p map)
      (cl-call-next-method)
    (seq-map (lambda (pair)
               (funcall function
                        (car pair)
                        (cdr pair)))
             map)))

(cl-defmethod map-apply (function (map hash-table))
  (let (result)
    (maphash (lambda (key value)
               (push (funcall function key value) result))
             map)
    (nreverse result)))

(cl-defmethod map-apply (function (map array))
  (let ((index 0))
    (seq-map (lambda (elt)
               (prog1
                   (funcall function index elt)
                 (setq index (1+ index))))
             map)))

(cl-defmethod map-do (function (map list))
  "Private function used to iterate over ALIST using FUNCTION."
  (if (map--plist-p map)
      (while map
        (funcall function (pop map) (pop map)))
    (seq-do (lambda (pair)
              (funcall function
                       (car pair)
                       (cdr pair)))
            map)))

(cl-defmethod map-do (function (array array))
  "Private function used to iterate over ARRAY using FUNCTION."
  (seq-do-indexed (lambda (elt index)
                     (funcall function index elt))
                   array))

(defun map--into-hash (map keyword-args)
  "Convert MAP into a hash-table.
KEYWORD-ARGS are forwarded to `make-hash-table'."
  (let ((ht (apply #'make-hash-table keyword-args)))
    (map-apply (lambda (key value)
                 (setf (gethash key ht) value))
               map)
    ht))

(cl-defmethod map-into (map (_type (eql hash-table)))
  "Convert MAP into a hash-table."
  (map--into-hash map (list :size (map-length map) :test 'equal)))

(cl-defmethod map-into (map (type (head hash-table)))
  "Convert MAP into a hash-table.
TYPE is a list where the car is `hash-table' and the cdr are the
keyword-args forwarded to `make-hash-table'.

Example:
    (map-into '((1 . 3)) '(hash-table :test eql))"
  (map--into-hash map (cdr type)))

(defun map--make-pcase-bindings (args)
  "Return a list of pcase bindings from ARGS to the elements of a map."
  (seq-map (lambda (elt)
             (if (consp elt)
                 `(app (pcase--flip map-elt ,(car elt)) ,(cadr elt))
               `(app (pcase--flip map-elt ',elt) ,elt)))
           args))

(defun map--make-pcase-patterns (args)
  "Return a list of `(map ...)' pcase patterns built from ARGS."
  (cons 'map
        (seq-map (lambda (elt)
                   (if (and (consp elt) (eq 'map (car elt)))
                       (map--make-pcase-patterns elt)
                     elt))
                 args)))

(provide 'map)
;;; map.el ends here
