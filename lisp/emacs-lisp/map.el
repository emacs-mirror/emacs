;;; map.el --- Map manipulation functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: convenience, map, hash-table, alist, array
;; Version: 1.2
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
           (t (error "Unsupported map: %s" ,map-var)))))

(defun map-elt (map key &optional default testfn)
  "Lookup KEY in MAP and return its associated value.
If KEY is not found, return DEFAULT which defaults to nil.

If MAP is a list, `eql' is used to lookup KEY.  Optional argument
TESTFN, if non-nil, means use its function definition instead of
`eql'.

MAP can be a list, hash-table or array."
  (declare
   (gv-expander
    (lambda (do)
      (gv-letplace (mgetter msetter) `(gv-delay-error ,map)
        (macroexp-let2* nil
            ;; Eval them once and for all in the right order.
            ((key key) (default default) (testfn testfn))
          `(if (listp ,mgetter)
               ;; Special case the alist case, since it can't be handled by the
               ;; map--put function.
               ,(gv-get `(alist-get ,key (gv-synthetic-place
                                          ,mgetter ,msetter)
                                    ,default nil ,testfn)
                        do)
             ,(funcall do `(map-elt ,mgetter ,key ,default)
                       (lambda (v) `(map--put ,mgetter ,key ,v)))))))))
  (map--dispatch map
    :list (alist-get key map default nil testfn)
    :hash-table (gethash key map default)
    :array (if (and (>= key 0) (< key (seq-length map)))
               (seq-elt map key)
             default)))

(defmacro map-put (map key value &optional testfn)
  "Associate KEY with VALUE in MAP and return VALUE.
If KEY is already present in MAP, replace the associated value
with VALUE.
When MAP is a list, test equality with TESTFN if non-nil, otherwise use `eql'.

MAP can be a list, hash-table or array."
  `(setf (map-elt ,map ,key nil ,testfn) ,value))

(defun map-delete (map key)
  "Delete KEY from MAP and return MAP.
No error is signaled if KEY is not a key of MAP.  If MAP is an
array, store nil at the index KEY.

MAP can be a list, hash-table or array."
  (map--dispatch map
    :list (setf (alist-get key map nil t) nil)
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

(defun map-keys (map)
  "Return the list of keys in MAP.

MAP can be a list, hash-table or array."
  (map-apply (lambda (key _) key) map))

(defun map-values (map)
  "Return the list of values in MAP.

MAP can be a list, hash-table or array."
  (map-apply (lambda (_ value) value) map))

(defun map-pairs (map)
  "Return the elements of MAP as key/value association lists.

MAP can be a list, hash-table or array."
  (map-apply #'cons map))

(defun map-length (map)
  "Return the length of MAP.

MAP can be a list, hash-table or array."
  (length (map-keys map)))

(defun map-copy (map)
  "Return a copy of MAP.

MAP can be a list, hash-table or array."
  (map--dispatch map
    :list (seq-copy map)
    :hash-table (copy-hash-table map)
    :array (seq-copy map)))

(defun map-apply (function map)
  "Apply FUNCTION to each element of MAP and return the result as a list.
FUNCTION is called with two arguments, the key and the value.

MAP can be a list, hash-table or array."
  (funcall (map--dispatch map
             :list #'map--apply-alist
             :hash-table #'map--apply-hash-table
             :array #'map--apply-array)
           function
           map))

(defun map-do (function map)
  "Apply FUNCTION to each element of MAP and return nil.
FUNCTION.is called with two arguments, the key and the value."
  (funcall (map--dispatch map
             :list #'map--do-alist
             :hash-table #'maphash
             :array #'map--do-array)
           function
           map))

(defun map-keys-apply (function map)
  "Return the result of applying FUNCTION to each key of MAP.

MAP can be a list, hash-table or array."
  (map-apply (lambda (key _)
               (funcall function key))
             map))

(defun map-values-apply (function map)
  "Return the result of applying FUNCTION to each value of MAP.

MAP can be a list, hash-table or array."
  (map-apply (lambda (_ val)
               (funcall function val))
             map))

(defun map-filter (pred map)
  "Return an alist of key/val pairs for which (PRED key val) is non-nil in MAP.

MAP can be a list, hash-table or array."
  (delq nil (map-apply (lambda (key val)
                         (if (funcall pred key val)
                             (cons key val)
                           nil))
                       map)))

(defun map-remove (pred map)
  "Return an alist of the key/val pairs for which (PRED key val) is nil in MAP.

MAP can be a list, hash-table or array."
  (map-filter (lambda (key val) (not (funcall pred key val)))
              map))

(defun mapp (map)
  "Return non-nil if MAP is a map (list, hash-table or array)."
  (or (listp map)
      (hash-table-p map)
      (arrayp map)))

(defun map-empty-p (map)
  "Return non-nil if MAP is empty.

MAP can be a list, hash-table or array."
  (map--dispatch map
    :list (null map)
    :array (seq-empty-p map)
    :hash-table (zerop (hash-table-count map))))

(defun map-contains-key (map key &optional testfn)
  "If MAP contain KEY return KEY, nil otherwise.
Equality is defined by TESTFN if non-nil or by `equal' if nil.

MAP can be a list, hash-table or array."
  (seq-contains (map-keys map) key testfn))

(defun map-some (pred map)
  "Return a non-nil if (PRED key val) is non-nil for any key/value pair in MAP.

MAP can be a list, hash-table or array."
  (catch 'map--break
    (map-apply (lambda (key value)
                 (let ((result (funcall pred key value)))
                   (when result
                     (throw 'map--break result))))
               map)
    nil))

(defun map-every-p (pred map)
  "Return non-nil if (PRED key val) is non-nil for all elements of the map MAP.

MAP can be a list, hash-table or array."
  (catch 'map--break
    (map-apply (lambda (key value)
              (or (funcall pred key value)
                  (throw 'map--break nil)))
            map)
    t))

(defun map-merge (type &rest maps)
  "Merge into a map of type TYPE all the key/value pairs in MAPS.

MAP can be a list, hash-table or array."
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
When two maps contain the same key, call FUNCTION on the two
values and use the value returned by it.
MAP can be a list, hash-table or array."
  (let ((result (map-into (pop maps) type))
        (not-found (cons nil nil)))
    (while maps
      (map-apply (lambda (key value)
                   (cl-callf (lambda (old)
                               (if (eq old not-found)
                                   value
                                 (funcall function old value)))
                       (map-elt result key not-found)))
                 (pop maps)))
    result))

(defun map-into (map type)
  "Convert the map MAP into a map of type TYPE.

TYPE can be one of the following symbols: list or hash-table.
MAP can be a list, hash-table or array."
  (pcase type
    (`list (map-pairs map))
    (`hash-table (map--into-hash-table map))
    (_ (error "Not a map type name: %S" type))))

(defun map--put (map key v)
  (map--dispatch map
    :list (let ((p (assoc key map)))
            (if p (setcdr p v)
              (error "No place to change the mapping for %S" key)))
    :hash-table (puthash key v map)
    :array (aset map key v)))

(defun map--apply-alist (function map)
  "Private function used to apply FUNCTION over MAP, MAP being an alist."
  (seq-map (lambda (pair)
             (funcall function
                      (car pair)
                      (cdr pair)))
           map))

(defun map--apply-hash-table (function map)
  "Private function used to apply FUNCTION over MAP, MAP being a hash-table."
  (let (result)
    (maphash (lambda (key value)
               (push (funcall function key value) result))
             map)
    (nreverse result)))

(defun map--apply-array (function map)
  "Private function used to apply FUNCTION over MAP, MAP being an array."
  (let ((index 0))
    (seq-map (lambda (elt)
               (prog1
                   (funcall function index elt)
                 (setq index (1+ index))))
             map)))

(defun map--do-alist (function alist)
  "Private function used to iterate over ALIST using FUNCTION."
  (seq-do (lambda (pair)
            (funcall function
                     (car pair)
                     (cdr pair)))
          alist))

(defun map--do-array (function array)
  "Private function used to iterate over ARRAY using FUNCTION."
  (seq-do-indexed (lambda (elt index)
                     (funcall function index elt))
                   array))

(defun map--into-hash-table (map)
  "Convert MAP into a hash-table."
  (let ((ht (make-hash-table :size (map-length map)
                             :test 'equal)))
    (map-apply (lambda (key value)
                 (setf (map-elt ht key) value))
               map)
    ht))

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
