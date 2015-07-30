;;; map.el --- Map manipulation functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: convenience, map, hash-table, alist, array
;; Version: 1.0
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

(pcase-defmacro map (&rest args)
  "pcase pattern matching map elements.
Matches if the object is a map (list, hash-table or array), and
binds values from ARGS to their corresponding elements of the map.

ARGS can be a list elements of the form (KEY PAT), in which case
KEY in an unquoted form.

ARGS can also be a list of symbols, which stands for ('SYMBOL
SYMBOL)."
  `(and (pred map-p)
        ,@(map--make-pcase-bindings args)))

(defmacro map-let (keys map &rest body)
  "Bind the variables in KEYS to the elements of MAP then evaluate BODY.

KEYS can be a list of symbols, in which case each element will be
bound to the looked up value in MAP.

KEYS can also be a list of (KEY VARNAME) pairs, in which case
KEY is an unquoted form.

MAP can be a list, hash-table or array."
  (declare (indent 2) (debug t))
  `(pcase-let ((,(map--make-pcase-patterns keys) ,map))
     ,@body))

(eval-when-compile
  (defmacro map--dispatch (map-var &rest args)
    "Evaluate one of the forms specified by ARGS based on the type of MAP.

The following keyword types are meaningful: `:list',
`:hash-table' and `:array'.

An error is thrown if MAP is neither a list, hash-table nor array.

Return RESULT if non-nil or the result of evaluation of the form."
    (declare (debug t) (indent 1))
    `(cond ((listp ,map-var) ,(plist-get args :list))
           ((hash-table-p ,map-var) ,(plist-get args :hash-table))
           ((arrayp ,map-var) ,(plist-get args :array))
           (t (error "Unsupported map: %s" ,map-var)))))

(defun map-elt (map key &optional default)
  "Perform a lookup in MAP of KEY and return its associated value.
If KEY is not found, return DEFAULT which defaults to nil.

If MAP is a list, `eql' is used to lookup KEY.

MAP can be a list, hash-table or array."
  (declare
   (gv-expander
    (lambda (do)
      (gv-letplace (mgetter msetter) `(gv-delay-error ,map)
        (macroexp-let2* nil
            ;; Eval them once and for all in the right order.
            ((key key) (default default))
          `(if (listp ,mgetter)
               ;; Special case the alist case, since it can't be handled by the
               ;; map--put function.
               ,(gv-get `(alist-get ,key (gv-synthetic-place
                                          ,mgetter ,msetter)
                                    ,default)
                        do)
             ,(funcall do `(map-elt ,mgetter ,key ,default)
                       (lambda (v) `(map--put ,mgetter ,key ,v)))))))))
  (map--dispatch map
    :list (alist-get key map default)
    :hash-table (gethash key map default)
    :array (if (and (>= key 0) (< key (seq-length map)))
               (seq-elt map key)
             default)))

(defmacro map-put (map key value)
  "In MAP, associate KEY with VALUE and return MAP.
If KEY is already present in MAP, replace the associated value
with VALUE.

MAP can be a list, hash-table or array."
  (macroexp-let2 nil map map
    `(progn
       (setf (map-elt ,map ,key) ,value)
       ,map)))

(defmacro map-delete (map key)
  "In MAP, delete the key KEY if present and return MAP.
If MAP is an array, store nil at the index KEY.

MAP can be a list, hash-table or array."
  (declare (debug t))
  (gv-letplace (mgetter msetter) `(gv-delay-error ,map)
    (macroexp-let2 nil key key
      `(if (not (listp ,mgetter))
           (map--delete ,mgetter ,key)
         ;; The alist case is special, since it can't be handled by the
         ;; map--delete function.
         (setf (alist-get ,key (gv-synthetic-place ,mgetter ,msetter)
                          nil t)
               nil)
         ,mgetter))))

(defun map-nested-elt (map keys &optional default)
  "Traverse MAP using KEYS and return the looked up value or DEFAULT if nil.

Map can be a nested map composed of alists, hash-tables and arrays."
  (or (seq-reduce (lambda (acc key)
                    (when (map-p acc)
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

(defun map-p (map)
  "Return non-nil if MAP is a map (list, hash-table or array)."
  (or (listp map)
      (hash-table-p map)
      (arrayp map)))

(defun map-empty-p (map)
  "Return non-nil is MAP is empty.

MAP can be a list, hash-table or array."
  (map--dispatch map
    :list (null map)
    :array (seq-empty-p map)
    :hash-table (zerop (hash-table-count map))))

(defun map-contains-key-p (map key &optional testfn)
  "Return non-nil if MAP contain the key KEY, nil otherwise.
Equality is defined by TESTFN if non-nil or by `equal' if nil.

MAP can be a list, hash-table or array."
  (seq-contains-p (map-keys map) key testfn))

(defun map-some-p (pred map)
  "Return a key/value pair for which (PRED key val) is non-nil in MAP.

MAP can be a list, hash-table or array."
  (catch 'map--break
    (map-apply (lambda (key value)
                 (when (funcall pred key value)
                   (throw 'map--break (cons key value))))
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
  "Merge into a map of type TYPE all the key/value pairs in the maps MAPS.

MAP can be a list, hash-table or array."
  (let (result)
    (while maps
      (map-apply (lambda (key value)
                   (setf (map-elt result key) value))
                 (pop maps)))
    (map-into result type)))

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

(defun map--delete (map key)
  (map--dispatch map
    :list (error "No place to remove the mapping for %S" key)
    :hash-table (remhash key map)
    :array (and (>= key 0)
                (<= key (seq-length map))
                (aset map key nil)))
  map)

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
