;;; registry.el --- Track and remember data items by various fields

;; Copyright (C) 2011-2018 Free Software Foundation, Inc.

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: data

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

;; This library provides a general-purpose EIEIO-based registry
;; database with persistence, initialized with these fields:

;; version: a float

;; max-size: an integer, default most-positive-fixnum

;; prune-factor: a float between 0 and 1, default 0.1

;; precious: a list of symbols

;; tracked: a list of symbols

;; tracker: a hash table tuned for 100 symbols to track (you should
;; only access this with the :lookup2-function and the
;; :lookup2+-function)

;; data: a hash table with default size 10K and resize threshold 2.0
;; (this reflects the expected usage so override it if you know better)

;; ...plus methods to do all the work: `registry-search',
;; `registry-lookup', `registry-lookup-secondary',
;; `registry-lookup-secondary-value', `registry-insert',
;; `registry-delete', `registry-prune', `registry-size' which see

;; and with the following properties:

;; Every piece of data has a unique ID and some general-purpose fields
;; (F1=D1, F2=D2, F3=(a b c)...) expressed as an alist, e.g.

;; ((F1 D1) (F2 D2) (F3 a b c))

;; Note that whether a field has one or many pieces of data, the data
;; is always a list of values.

;; The user decides which fields are "precious", F2 for example.  When
;; the registry is pruned, any entries without the F2 field will be
;; removed until the size is :max-size * :prune-factor _less_ than the
;; maximum database size. No entries with the F2 field will be removed
;; at PRUNE TIME, which means it may not be possible to prune back all
;; the way to the target size.

;; When an entry is inserted, the registry will reject new entries if
;; they bring it over the :max-size limit, even if they have the F2
;; field.

;; The user decides which fields are "tracked", F1 for example.  Any
;; new entry is then indexed by all the tracked fields so it can be
;; quickly looked up that way.  The data is always a list (see example
;; above) and each list element is indexed.

;; Precious and tracked field names must be symbols.  All other
;; fields can be any other Emacs Lisp types.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'eieio-base)

;; The version number needs to be kept outside of the class definition
;; itself.  The persistent-save process does *not* write to file any
;; slot values that are equal to the default :initform value.  If a
;; database object is at the most recent version, therefore, its
;; version number will not be written to file.  That makes it
;; difficult to know when a database needs to be upgraded.
(defvar registry-db-version 0.2
  "The current version of the registry format.")

(defclass registry-db (eieio-persistent)
  ((version :initarg :version
            :initform nil
            :type (or null float)
            :documentation "The registry version.")
   (max-size :initarg :max-size
	     ;; EIEIO's :initform is not 100% compatible with CLOS in
	     ;; that if the form is an atom, it assumes it's constant
	     ;; value rather than an expression, so in order to get the value
	     ;; of `most-positive-fixnum', we need to use an
	     ;; expression that's not just a symbol.
             :initform (symbol-value 'most-positive-fixnum)
             :type integer
             :custom integer
             :documentation "The maximum number of registry entries.")
   (prune-factor
    :initarg :prune-factor
    :initform 0.1
    :type float
    :custom float
    :documentation "Prune to (:max-size * :prune-factor) less
    than the :max-size limit.  Should be a float between 0 and 1.")
   (tracked :initarg :tracked
            :initform nil
            :type t
            :documentation "The tracked (indexed) fields, a list of symbols.")
   (precious :initarg :precious
             :initform nil
             :type t
             :documentation "The precious fields, a list of symbols.")
   (tracker :initarg :tracker
            :type hash-table
            :documentation "The field tracking hash table.")
   (data :initarg :data
         :type hash-table
         :documentation "The data hash table.")))

(cl-defmethod initialize-instance :before ((this registry-db) slots)
  "Check whether a registry object needs to be upgraded."
  ;; Hardcoded upgrade routines.  Version 0.1 to 0.2 requires the
  ;; :max-soft slot to disappear, and the :max-hard slot to be renamed
  ;; :max-size.
  (let ((current-version
	 (and (plist-member slots :version)
	      (plist-get slots :version))))
    (when (or (null current-version)
	      (eql current-version 0.1))
      (setq slots
	    (plist-put slots :max-size (plist-get slots :max-hard)))
      (setq slots
	    (plist-put slots :version registry-db-version))
      (cl-remf slots :max-hard)
      (cl-remf slots :max-soft))))

(cl-defmethod initialize-instance :after ((this registry-db) slots)
  "Set value of data slot of THIS after initialization."
  (with-slots (data tracker) this
    (unless (member :data slots)
      (setq data
	    (make-hash-table :size 10000 :rehash-size 2.0 :test 'equal)))
    (unless (member :tracker slots)
      (setq tracker (make-hash-table :size 100 :rehash-size 2.0)))))

(cl-defmethod registry-lookup ((db registry-db) keys)
  "Search for KEYS in the registry-db THIS.
Returns an alist of the key followed by the entry in a list, not a cons cell."
  (let ((data (oref db data)))
    (delq nil
	  (mapcar
	   (lambda (k)
	     (when (gethash k data)
	       (list k (gethash k data))))
	   keys))))

(cl-defmethod registry-lookup-breaks-before-lexbind ((db registry-db) keys)
  "Search for KEYS in the registry-db THIS.
Returns an alist of the key followed by the entry in a list, not a cons cell."
  (let ((data (oref db data)))
    (delq nil
	  (cl-loop for key in keys
                   when (gethash key data)
                   collect (list key (gethash key data))))))

(cl-defmethod registry-lookup-secondary ((db registry-db) tracksym
					 &optional create)
  "Search for TRACKSYM in the registry-db THIS.
When CREATE is not nil, create the secondary index hash table if needed."
  (let ((h (gethash tracksym (oref db tracker))))
    (if h
	h
      (when create
	(puthash tracksym
		 (make-hash-table :size 800 :rehash-size 2.0 :test 'equal)
		 (oref db tracker))
	(gethash tracksym (oref db tracker))))))

(cl-defmethod registry-lookup-secondary-value ((db registry-db) tracksym val
					       &optional set)
  "Search for TRACKSYM with value VAL in the registry-db THIS.
When SET is not nil, set it for VAL (use t for an empty list)."
  ;; either we're asked for creation or there should be an existing index
  (when (or set (registry-lookup-secondary db tracksym))
    ;; set the entry if requested,
    (when set
      (puthash val (if (eq t set) '() set)
	       (registry-lookup-secondary db tracksym t)))
    (gethash val (registry-lookup-secondary db tracksym))))

(defun registry--match (mode entry check-list)
  ;; for all members
  (when check-list
    (let ((key (nth 0 (nth 0 check-list)))
          (vals (cdr-safe (nth 0 check-list)))
          found)
      (while (and key vals (not found))
        (setq found (cl-case mode
                      (:member
                       (member (car-safe vals) (cdr-safe (assoc key entry))))
                      (:regex
                       (string-match (car vals)
                                     (mapconcat
                                      'prin1-to-string
                                      (cdr-safe (assoc key entry))
                                      "\0"))))
              vals (cdr-safe vals)))
      (or found
          (registry--match mode entry (cdr-safe check-list))))))

(cl-defmethod registry-search ((db registry-db) &rest spec)
  "Search for SPEC across the registry-db THIS.
For example calling with `:member \\='(a 1 2)' will match entry \((a 3 1)).
Calling with `:all t' (any non-nil value) will match all.
Calling with `:regex \\='(a \"h.llo\")' will match entry \(a \"hullo\" \"bye\").
The test order is to check :all first, then :member, then :regex."
  (when db
    (let ((all (plist-get spec :all))
	  (member (plist-get spec :member))
	  (regex (plist-get spec :regex)))
      (cl-loop for k being the hash-keys of (oref db data)
               using (hash-values v)
               when (or
                     ;; :all non-nil returns all
                     all
                     ;; member matching
                     (and member (registry--match :member v member))
                     ;; regex matching
                     (and regex (registry--match :regex v regex)))
               collect k))))

(cl-defmethod registry-delete ((db registry-db) keys assert &rest spec)
  "Delete KEYS from the registry-db THIS.
If KEYS is nil, use SPEC to do a search.
Updates the secondary ('tracked') indices as well.
With assert non-nil, errors out if the key does not exist already."
  (let* ((data (oref db data))
	 (keys (or keys
		   (apply 'registry-search db spec)))
	 (tracked (oref db tracked)))

    (dolist (key keys)
      (let ((entry (gethash key data)))
	(when assert
	  (cl-assert entry nil "Key %s does not exist in database" key))
	;; clean entry from the secondary indices
	(dolist (tr tracked)
	  ;; is this tracked symbol indexed?
	  (when (registry-lookup-secondary db tr)
	    ;; for every value in the entry under that key...
	    (dolist (val (cdr-safe (assq tr entry)))
	      (let* ((value-keys (registry-lookup-secondary-value
				  db tr val)))
		(when (member key value-keys)
		  ;; override the previous value
		  (registry-lookup-secondary-value
		   db tr val
		   ;; with the indexed keys MINUS the current key
		   ;; (we pass t when the list is empty)
		   (or (delete key value-keys) t)))))))
	(remhash key data)))
    keys))

(cl-defmethod registry-size ((db registry-db))
  "Returns the size of the registry-db object THIS.
This is the key count of the `data' slot."
  (hash-table-count (oref db data)))

(cl-defmethod registry-full ((db registry-db))
  "Checks if registry-db THIS is full."
  (>= (registry-size db)
      (oref db max-size)))

(cl-defmethod registry-insert ((db registry-db) key entry)
  "Insert ENTRY under KEY into the registry-db THIS.
Updates the secondary ('tracked') indices as well.
Errors out if the key exists already."
  (cl-assert (not (gethash key (oref db data))) nil
             "Key already exists in database")
  (cl-assert (not (registry-full db)) nil
             "registry max-size limit reached")

  ;; store the entry
  (puthash key entry (oref db data))

  ;; store the secondary indices
  (dolist (tr (oref db tracked))
    ;; for every value in the entry under that key...
    (dolist (val (cdr-safe (assq tr entry)))
      (let* ((value-keys (registry-lookup-secondary-value db tr val)))
	(cl-pushnew key value-keys :test 'equal)
	(registry-lookup-secondary-value db tr val value-keys))))
  entry)

(cl-defmethod registry-reindex ((db registry-db))
  "Rebuild the secondary indices of registry-db THIS."
  (let ((count 0)
	(expected (* (length (oref db tracked)) (registry-size db))))
    (dolist (tr (oref db tracked))
      (let (values)
	(maphash
	 (lambda (key v)
	   (cl-incf count)
	   (when (and (< 0 expected)
		      (= 0 (mod count 1000)))
	     (message "reindexing: %d of %d (%.2f%%)"
		      count expected (/ (* 100.0 count) expected)))
	   (dolist (val (cdr-safe (assq tr v)))
	     (let* ((value-keys (registry-lookup-secondary-value db tr val)))
	       (push key value-keys)
	       (registry-lookup-secondary-value db tr val value-keys))))
	 (oref db data))))))

(cl-defmethod registry-prune ((db registry-db) &optional sortfunc)
  "Prunes the registry-db object DB.

Attempts to prune the number of entries down to \(*
:max-size :prune-factor) less than the max-size limit, so
pruning doesn't need to happen on every save. Removes only
entries without the :precious keys, so it may not be possible to
reach the target limit.

Entries to be pruned are first sorted using SORTFUNC.  Entries
from the front of the list are deleted first.

Returns the number of deleted entries."
  (let ((size (registry-size db))
	(target-size
	 (floor (- (oref db max-size)
		   (* (oref db max-size)
		      (oref db prune-factor)))))
	candidates)
    (if (registry-full db)
	(progn
	  (setq candidates
		(registry-collect-prune-candidates
		 db (- size target-size) sortfunc))
	  (length (registry-delete db candidates nil)))
      0)))

(cl-defmethod registry-collect-prune-candidates ((db registry-db)
						 limit sortfunc)
  "Collects pruning candidates from the registry-db object DB.

Proposes only entries without the :precious keys, and attempts to
return LIMIT such candidates.  If SORTFUNC is provided, sort
entries first and return candidates from beginning of list."
  (let* ((precious (oref db precious))
	 (precious-p (lambda (entry-key)
		       (cdr (memq (car-safe entry-key) precious))))
	 (data (oref db data))
	 (candidates (cl-loop for k being the hash-keys of data
			      using (hash-values v)
			      when (and (listp v)
                                        (cl-notany precious-p v))
			      collect (cons k v))))
    ;; We want the full entries for sorting, but should only return a
    ;; list of entry keys.
    (when sortfunc
      (setq candidates (sort candidates sortfunc)))
    (cl-subseq (mapcar #'car candidates) 0 (min limit (length candidates)))))

(provide 'registry)
;;; registry.el ends here
