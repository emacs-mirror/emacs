;;; map.el --- Map manipulation functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2026 Free Software Foundation, Inc.

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Maintainer: emacs-devel@gnu.org
;; Keywords: extensions, lisp
;; Version: 3.3.1
;; Package-Requires: ((emacs "26"))

;; This is a GNU ELPA :core package.  Avoid functionality that is not
;; compatible with the version of Emacs recorded above.

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

;; map.el provides generic map-manipulation functions that work on
;; alists, plists, hash-tables, and arrays.  All functions are
;; prefixed with "map-".
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

Each element of ARGS can be of the form (KEY PAT [DEFAULT]),
which looks up KEY in the map and matches the associated value
against `pcase' pattern PAT.  DEFAULT specifies the fallback
value to use when KEY is not present in the map.  If omitted, it
defaults to nil.  Both KEY and DEFAULT are evaluated.

Each element can also be a SYMBOL, which is an abbreviation of
a (KEY PAT) tuple of the form (\\='SYMBOL SYMBOL).  When SYMBOL
is a keyword, it is an abbreviation of the form (:SYMBOL SYMBOL),
useful for binding plist values.

An element of ARGS fails to match if PAT does not match the
associated value or the default value.  The overall pattern fails
to match if any element of ARGS fails to match."
  `(and (pred mapp)
        ,@(map--make-pcase-bindings args)))

(defmacro map-let (keys map &rest body)
  "Bind the variables in KEYS to the elements of MAP, then evaluate BODY.

KEYS can be a list of symbols, in which case each element will be
bound to the looked up value in MAP.

KEYS can also be a list of (KEY VARNAME [DEFAULT]) sublists, in
which case KEY and DEFAULT are unquoted forms.

MAP can be an alist, plist, hash-table, or array."
  (declare (indent 2)
           (debug ((&rest &or symbolp ([form symbolp &optional form]))
                   form body)))
  `(pcase-let ((,(map--make-pcase-patterns keys) ,map))
     ,@body))

(define-error 'map-not-inplace "Cannot modify map in-place")

(defsubst map--plist-p (list)
  "Return non-nil if LIST is the start of a nonempty plist map."
  (and (consp list) (atom (car list))))

(defconst map--plist-has-predicate
  (condition-case nil
      (with-no-warnings (plist-get () nil #'eq) t)
    (wrong-number-of-arguments))
  "Non-nil means `plist-get' & co. accept a predicate in Emacs 29+.
Note that support for this predicate in map.el is patchy and
deprecated.")

(defun map--plist-member-1 (plist prop &optional predicate)
  "Compatibility shim for the PREDICATE argument of `plist-member'.
Assumes non-nil PLIST satisfies `map--plist-p'."
  (if (or (memq predicate '(nil eq)) (null plist))
      (plist-member plist prop)
    (let ((tail plist) found)
      (while (and (not (setq found (funcall predicate (car tail) prop)))
                  (consp (setq tail (cdr tail)))
                  (consp (setq tail (cdr tail)))))
      (and tail (not found)
           (signal 'wrong-type-argument `(plistp ,plist)))
      tail)))

(defalias 'map--plist-member
  (if map--plist-has-predicate #'plist-member #'map--plist-member-1)
  "Compatibility shim for `plist-member' in Emacs 29+.
\n(fn PLIST PROP &optional PREDICATE)")

(defun map--plist-put-1 (plist prop val &optional predicate)
  "Compatibility shim for the PREDICATE argument of `plist-put'.
Assumes non-nil PLIST satisfies `map--plist-p'."
  (if (or (memq predicate '(nil eq)) (null plist))
      (plist-put plist prop val)
    (let ((tail plist) prev found)
      (while (and (consp (cdr tail))
                  (not (setq found (funcall predicate (car tail) prop)))
                  (consp (setq prev tail tail (cddr tail)))))
      (cond (found (setcar (cdr tail) val))
            (tail (signal 'wrong-type-argument `(plistp ,plist)))
            (prev (setcdr (cdr prev) (cons prop (cons val (cddr prev)))))
            ((setq plist (cons prop (cons val plist)))))
      plist)))

(defalias 'map--plist-put
  (if map--plist-has-predicate #'plist-put #'map--plist-put-1)
  "Compatibility shim for `plist-put' in Emacs 29+.
\n(fn PLIST PROP VAL &optional PREDICATE)")

(cl-defgeneric map-elt (map key &optional default testfn)
  "Look up KEY in MAP and return its associated value.
If KEY is not found, return DEFAULT which defaults to nil.

TESTFN is the function to use for comparing keys.  It is
deprecated because its default and valid values depend on the MAP
argument, and it was never consistently supported by the map.el
API.  Generally, alist keys are compared with `equal', plist keys
with `eq', and hash-table keys with the hash-table's test
function.

In the base definition, MAP can be an alist, plist, hash-table,
or array."
  (declare
   ;; `testfn' is deprecated.
   (advertised-calling-convention (map key &optional default) "27.1")
   (gv-expander
    (lambda (do)
      (gv-letplace (mgetter msetter) `(gv-delay-error ,map)
        (macroexp-let2* nil
            ;; Eval them once and for all in the right order.
            ((key key) (default default) (testfn testfn))
          (funcall do
                   `(map-elt ,mgetter ,key ,default ,@(and testfn `(,testfn)))
                   (lambda (v)
                     (macroexp-let2 nil v v
                       `(condition-case nil
                            ;; Silence warnings about the hidden 4th arg.
                            (with-no-warnings
                              (map-put! ,mgetter ,key ,v ,testfn))
                          (map-not-inplace
                           ,(funcall msetter
                                     `(map-insert ,mgetter ,key ,v))
                           ;; Always return the value.
                           ,v)))))))))))

(cl-defmethod map-elt ((map list) key &optional default testfn)
  (if (map--plist-p map)
      (let ((res (map--plist-member map key testfn)))
        (if res (cadr res) default))
    (alist-get key map default nil (or testfn #'equal))))

(cl-defmethod map-elt ((map hash-table) key &optional default _testfn)
  (gethash key map default))

(cl-defmethod map-elt ((map array) key &optional default _testfn)
  (if (map-contains-key map key)
      (aref map key)
    default))

(defmacro map-put (map key value &optional testfn)
  "Associate KEY with VALUE in MAP and return VALUE.
If KEY is already present in MAP, replace the associated value
with VALUE.
If MAP is an alist, test equality with TESTFN if non-nil,
otherwise use `equal'.

MAP can be an alist, plist, hash-table, or array."
  (declare
   (obsolete "use `map-put!' or `(setf (map-elt ...) ...)' instead." "27.1"))
  (if testfn
      `(with-no-warnings
         (setf (map-elt ,map ,key nil ,testfn) ,value))
    `(setf (map-elt ,map ,key) ,value)))

(defun map--plist-delete (map key)
  (let ((tail map) last)
    (while (consp tail)
      (cond
       ((not (eq key (car tail)))
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
Keys not present in MAP are ignored.

Note that if MAP is a list (either alist or plist), and you're
deleting the final element in the list, the list isn't actually
destructively modified (but the return value will reflect the
deletion).  So if you're using this method on a list, you have to
say

  (setq map (map-delete map key))

for this to work reliably.")

(cl-defmethod map-delete ((map list) key)
  ;; FIXME: Signal map-not-inplace i.s.o returning a different list?
  (if (map--plist-p map)
      (map--plist-delete map key)
    (setf (alist-get key map nil t #'equal) nil)
    map))

(cl-defmethod map-delete ((map hash-table) key)
  (remhash key map)
  map)

(cl-defmethod map-delete ((map array) key)
  "Store nil at index KEY."
  (when (map-contains-key map key)
    (aset map key nil))
  map)

(defun map-nested-elt (map keys &optional default)
  "Traverse MAP using KEYS and return the looked up value or DEFAULT if nil.

MAP can be a nested map composed of alists, plists, hash-tables,
and arrays."
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

(cl-defmethod map-values ((map array))
  "Convert MAP into a list."
  (append map ()))

(cl-defgeneric map-pairs (map)
  "Return the key/value pairs in MAP as an alist.
The default implementation delegates to `map-apply'."
  (map-apply #'cons map))

(cl-defgeneric map-length (map)
  ;; FIXME: Should we rename this to `map-size'?
  "Return the number of key/value pairs in MAP.
Note that this does not always reflect the number of unique keys.
The default implementation delegates to `map-do'."
  (let ((size 0))
    (map-do (lambda (_k _v) (setq size (1+ size))) map)
    size))

(cl-defmethod map-length ((map hash-table))
  (hash-table-count map))

(cl-defmethod map-length ((map list))
  (if (map--plist-p map)
      (/ (length map) 2)
    (length map)))

(cl-defmethod map-length ((map array))
  (length map))

(cl-defgeneric map-copy (map)
  "Return a copy of MAP.")

(cl-defmethod map-copy ((map list))
  "Use `copy-alist' on alists and `copy-sequence' on plists."
  (if (map--plist-p map)
      (copy-sequence map)
    (copy-alist map)))

(cl-defmethod map-copy ((map hash-table))
  (copy-hash-table map))

(cl-defmethod map-copy ((map array))
  (copy-sequence map))

(cl-defgeneric map-apply (function map)
  "Apply FUNCTION to each element of MAP and return the result as a list.
FUNCTION is called with two arguments, the key of an element and its value.
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
  "Return the result of applying FUNCTION to each key in MAP.
The default implementation delegates to `map-apply'."
  (map-apply (lambda (key _)
               (funcall function key))
             map))

(cl-defgeneric map-values-apply (function map)
  "Return the result of applying FUNCTION to the value of each key in MAP.
The default implementation delegates to `map-apply'."
  (map-apply (lambda (_ val)
               (funcall function val))
             map))

(cl-defmethod map-values-apply (function (map array))
  (mapcar function map))

(cl-defgeneric map-filter (pred map)
  "Return an alist of key/val pairs for which (PRED key val) is non-nil in MAP.
The default implementation delegates to `map-apply'.
This does not modify MAP."
  (delq nil (map-apply (lambda (key val)
                         (and (funcall pred key val)
                              (cons key val)))
                       map)))

(cl-defgeneric map-remove (pred map)
  "Return an alist of the key/val pairs for which (PRED key val) is nil in MAP.
The default implementation delegates to `map-filter'.
This does not modify MAP."
  (map-filter (lambda (key val) (not (funcall pred key val)))
              map))

(cl-defgeneric mapp (map)
  "Return non-nil if MAP is a map (alist/plist, hash-table, array, ...)."
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
The default implementation delegates to `map-some'."
  (declare (advertised-calling-convention (map key) "27.1"))
  (unless testfn (setq testfn #'equal))
  (map-some (lambda (k _v) (funcall testfn key k)) map))

(cl-defmethod map-contains-key ((map list) key &optional testfn)
  "Return non-nil if MAP contains KEY.
If MAP is an alist, TESTFN defaults to `equal'.
If MAP is a plist, TESTFN defaults to `eq'."
  (if (map--plist-p map)
      (map--plist-member map key testfn)
    (let ((v '(nil)))
      (not (eq v (alist-get key map v nil (or testfn #'equal)))))))

(cl-defmethod map-contains-key ((map array) key &optional _testfn)
  "Return non-nil if KEY is an index of MAP, ignoring TESTFN."
  (and (natnump key) (< key (length map))))

(cl-defmethod map-contains-key ((map hash-table) key &optional _testfn)
  "Return non-nil if MAP contains KEY, ignoring TESTFN."
  ;; FIXME: use `hash-table-contains-p' from Compat when available.
  (if (fboundp 'hash-table-contains-p)
      (hash-table-contains-p key map)
    (let ((v '(nil)))
      (not (eq v (gethash key map v))))))

(cl-defgeneric map-some (pred map)
  "Return the first non-nil value from applying PRED to elements of MAP.
Return nil if no such element is found.
PRED is called with two arguments: the key of an element and its value.
The default implementation delegates to `map-do'."
  ;; FIXME: Not sure if there's much benefit to defining it as defgeneric,
  ;; since as defined, I can't think of a map-type where we could provide an
  ;; algorithmically more efficient algorithm than the default.
  (catch 'map--break
    (map-do (lambda (key value)
              (let ((result (funcall pred key value)))
                (when result
                  (throw 'map--break result))))
            map)
    nil))

(cl-defgeneric map-every-p (pred map)
  "Return non-nil if calling PRED on all elements of MAP returns non-nil.
PRED is called with two arguments: the key of an element and its value.
The default implementation delegates to `map-do'."
  ;; FIXME: Not sure if there's much benefit to defining it as defgeneric,
  ;; since as defined, I can't think of a map-type where we could provide an
  ;; algorithmically more efficient algorithm than the default.
  (catch 'map--break
    (map-do (lambda (key value)
              (or (funcall pred key value)
                  (throw 'map--break nil)))
            map)
    t))

(defun map--merge (merge type &rest maps)
  "Merge into a map of TYPE all the key/value pairs in MAPS.
MERGE is a function that takes the target MAP, a KEY and its
VALUE, merges KEY and VALUE into MAP, and returns the result.
MAP may be of a type other than TYPE."
  ;; Use a hash table internally if `type' is a list.  This avoids
  ;; both quadratic lookup behavior and the type ambiguity of nil.
  (let* ((tolist (memq type '(list alist plist)))
         (result (map-into (pop maps)
                            ;; Use same testfn as `map-elt' gv setter.
                           (cond ((eq type 'plist) '(hash-table :test eq))
                                 (tolist '(hash-table :test equal))
                                 (type)))))
    (dolist (map maps)
      (map-do (lambda (key value)
                (setq result (funcall merge result key value)))
              map))
    ;; Convert internal representation to desired type.
    (if tolist (map-into result type) result)))

(defun map-merge (type &rest maps)
  "Merge into a map of TYPE all the key/value pairs in MAPS.
See `map-into' for all supported values of TYPE.
This does not modify any of the MAPS."
  (apply #'map--merge
         (lambda (result key value)
           (setf (map-elt result key) value)
           result)
         type maps))

(defun map-merge-with (type function &rest maps)
  "Merge into a map of TYPE all the key/value pairs in MAPS.
When two maps contain the same key, call FUNCTION on the two
values and use the value FUNCTION returns.
Each of MAPS can be an alist, plist, hash-table, or array.
See `map-into' for all supported values of TYPE.
This does not modify any of the MAPS."
  (let ((not-found (list nil)))
    (apply #'map--merge
           (lambda (result key value)
             (cl-callf (lambda (old)
                         (if (eql old not-found)
                             value
                           (funcall function old value)))
                 (map-elt result key not-found))
             result)
           type maps)))

(cl-defgeneric map-into (map type)
  "Convert MAP into a map of TYPE.")

;; FIXME: I wish there was a way to avoid this η-redex!
(cl-defmethod map-into (map (_type (eql list)))
  "Convert MAP into an alist."
  (map-pairs map))

(cl-defmethod map-into (map (_type (eql alist)))
  "Convert MAP into an alist."
  (map-pairs map))

(cl-defmethod map-into (map (_type (eql plist)))
  "Convert MAP into a plist."
  (let (plist)
    (map-do (lambda (k v) (setq plist `(,v ,k ,@plist))) map)
    (nreverse plist)))

(cl-defgeneric map-put! (map key value &optional testfn)
  "Associate KEY with VALUE in MAP.
If KEY is already present in MAP, replace the associated value
with VALUE.
This operates by modifying MAP in place.  If it cannot do that,
it signals the `map-not-inplace' error.
To insert an element without modifying MAP, use `map-insert'."
  ;; `testfn' only exists for backward compatibility with `map-put'!
  (declare (advertised-calling-convention (map key value) "27.1")))

(cl-defmethod map-put! ((map list) key value &optional testfn)
  (if (map--plist-p map)
      (map--plist-put map key value testfn)
    (let ((oldmap map))
      (setf (alist-get key map key nil (or testfn #'equal)) value)
      (unless (eq oldmap map)
        (signal 'map-not-inplace (list oldmap)))))
  ;; Always return the value.
  value)

(cl-defmethod map-put! ((map hash-table) key value &optional _testfn)
  (puthash key value map))

(cl-defmethod map-put! ((map array) key value &optional _testfn)
  ;; FIXME: If `key' is too large, should we signal `map-not-inplace'
  ;; and let `map-insert' grow the array?
  (aset map key value))

;; There shouldn't be old source code referring to `map--put', yet we do
;; need to keep it for backward compatibility with .elc files where the
;; expansion of `setf' may call this function.
(define-obsolete-function-alias 'map--put #'map-put! "27.1")

(cl-defgeneric map-insert (map key value)
  "Return a new map like MAP except that it associates KEY with VALUE.
This does not modify MAP.
If you want to insert an element in place, use `map-put!'.
The default implementation defaults to `map-copy' and `map-put!'."
  (let ((copy (map-copy map)))
    (map-put! copy key value)
    copy))

(cl-defmethod map-insert ((map list) key value)
  "Cons KEY and VALUE to the front of MAP."
  (if (map--plist-p map)
      (cons key (cons value map))
    (cons (cons key value) map)))

(cl-defmethod map-apply (function (map list))
  (if (map--plist-p map)
      (cl-call-next-method)
    (mapcar (lambda (pair)
              (funcall function (car pair) (cdr pair)))
            map)))

(cl-defmethod map-apply (function (map hash-table))
  (let (result)
    (maphash (lambda (key value)
               (push (funcall function key value) result))
             map)
    (nreverse result)))

(cl-defmethod map-apply (function (map array))
  (seq-map-indexed (lambda (elt index)
                     (funcall function index elt))
                   map))

(cl-defmethod map-do (function (map list))
  (if (map--plist-p map)
      (while map
        (funcall function (pop map) (pop map)))
    (mapc (lambda (pair)
            (funcall function (car pair) (cdr pair)))
          map)
    nil))

(cl-defmethod map-do (function (map array))
  (seq-do-indexed (lambda (elt index)
                    (funcall function index elt))
                  map))

(defun map--into-hash (map keyword-args)
  "Convert MAP into a hash-table.
KEYWORD-ARGS are forwarded to `make-hash-table'."
  (let ((ht (apply #'make-hash-table keyword-args)))
    (map-do (lambda (key value)
              (puthash key value ht))
            map)
    ht))

(cl-defmethod map-into (map (_type (eql hash-table)))
  "Convert MAP into a hash-table with keys compared with `equal'."
  (map--into-hash map (list :size (map-length map) :test #'equal)))

(cl-defmethod map-into (map (type (head hash-table)))
  "Convert MAP into a hash-table.
TYPE is a list whose car is `hash-table' and cdr a list of
keyword-args forwarded to `make-hash-table'.

Example:
    (map-into \\='((1 . 3)) \\='(hash-table :test eql))"
  (map--into-hash map (cdr type)))

(defmacro map--pcase-map-elt (key default map)
  "A macro to make MAP the last argument to `map-elt'.

This allows using default values for `map-elt', which can't be
done using `pcase--flip'.

KEY is the key sought in the map.  DEFAULT is the default value."
  ;; It's obsolete in Emacs>29, but `map.el' is distributed via GNU ELPA
  ;; for earlier Emacsen.
  (declare (obsolete _ "30.1"))
  `(map-elt ,map ,key ,default))

(defun map--make-pcase-bindings (args)
  "Return a list of pcase bindings from ARGS to the elements of a map."
  (mapcar (if (< emacs-major-version 30)
              (lambda (elt)
                (cond ((consp elt)
                       `(app (map--pcase-map-elt ,(car elt) ,(caddr elt))
                             ,(cadr elt)))
                      ((keywordp elt)
                       (let ((var (intern (substring (symbol-name elt) 1))))
                         `(app (pcase--flip map-elt ,elt) ,var)))
                      (t `(app (pcase--flip map-elt ',elt) ,elt))))
            (lambda (elt)
              (cond ((consp elt)
                     `(app (map-elt _ ,(car elt) ,(caddr elt))
                           ,(cadr elt)))
                    ((keywordp elt)
                     (let ((var (intern (substring (symbol-name elt) 1))))
                       `(app (map-elt _ ,elt) ,var)))
                    (t `(app (map-elt _ ',elt) ,elt)))))
          args))

(defun map--make-pcase-patterns (args)
  "Return a list of `(map ...)' pcase patterns built from ARGS."
  (cons 'map
        (mapcar (lambda (elt)
                  (if (eq (car-safe elt) 'map)
                      (map--make-pcase-patterns elt)
                    elt))
                args)))

(provide 'map)
;;; map.el ends here
