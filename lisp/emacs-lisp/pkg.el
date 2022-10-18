;;; pkg.el --- Lisp packages -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: Gerd Möllmann <gerd@gnu.org>
;; Keywords: lisp, tools, maint
;; Version: 1.0

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

;; This file is part of the implementation of Lisp packages for Emacs.
;; Code is partly adapted from CMUCL, which is in the public domain.

;; The goal of this is, among others, to do as much as possible in
;; Lisp, not C.

;;; Code:

(require 'cl-lib)

(defvar *default-package-use-list* nil
  "tbd")

(defun pkg-check-disjoint (&rest args)
  "Check whether all given arguments specify disjoint sets of symbols.
Each argument is of the form (:key . set)."
  (cl-loop for (current-arg . rest-args) on args
           do
	   (cl-loop with (key1 . set1) = current-arg
	            for (key2 . set2) in rest-args
	            for common = (cl-delete-duplicates
	                          (cl-intersection set1 set2 :test #'string=))
	            unless (null common)
	            do
	            (error "Parameters %s and %s must be disjoint \
but have common elements %s" key1 key2 common))))

(defun pkg-stringify-name (name kind)
  (cl-typecase name
    (string name)
    (symbol (symbol-name name))
    (base-char (char-to-string name))
    (t (error "Bogus %s name: %s" kind name))))

(defun pkg-stringify-names (names kind)
  (mapcar (lambda (name) (pkg-stringify-name name kind)) names))

(defun pkg-package-namify (n)
  (pkg-stringify-name n "package"))

(defun pkg-name-to-package (name)
  (gethash name *package-registry* nil))

(defun pkg-enter-new-nicknames (package nicknames)
  (cl-check-type nicknames list)
  (dolist (n nicknames)
    (let* ((n (pkg-package-namify n))
	   (found (pkg-name-to-package n)))
      (cond ((not found)
	     (setf (gethash n *package-registry*) package)
	     (push n (package-%nicknames package)))
	    ((eq found package))
	    ((string= (package-name found) n)
	     (error "%s is a package name, so it cannot be a nickname for %s."
	            n (package-name package)))
	    (t
	     (error "%s is already a nickname for %s"
                    n (package-name found)))))))

;;; package-or-lose  --  Internal
;;;
;;;    Take a package-or-string-or-symbol and return a package.
;;;
(defun package-or-lose (thing)
  (cond ((packagep thing)
	 (unless (package-%name thing)
	   (error "Can't do anything to a deleted package: %s" thing))
	 thing)
	(t
	 (let ((thing (pkg-package-namify thing)))
	   (cond ((pkg-name-to-package thing))
		 (t (make-package thing)))))))

(defun find-or-make-symbol (name package)
  (cl-multiple-value-bind (symbol how)
      (find-symbol name package)
    (if how
	symbol
      (intern name package))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            defpackage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %defpackage (name nicknames size shadows shadowing-imports
			 use imports interns exports doc-string)
  (let ((package (or (find-package name)
		     (progn
		       (when (eq use :default)
			 (setf use *default-package-use-list*))
		       (make-package name
				     :use nil
				     :size (or size 10))))))
    (unless (string= (package-name package) name)
      (error "%s is a nick-name for the package %s" name (package-name name)))
    (pkg-enter-new-nicknames package nicknames)

    ;; Shadows and Shadowing-imports.
    (let ((old-shadows (package-%shadowing-symbols package)))
      (shadow shadows package)
      (dolist (sym-name shadows)
	(setf old-shadows (remove (find-symbol sym-name package) old-shadows)))
      (dolist (simports-from shadowing-imports)
	(let ((other-package (package-or-lose (car simports-from))))
	  (dolist (sym-name (cdr simports-from))
	    (let ((sym (find-or-make-symbol sym-name other-package)))
	      (shadowing-import sym package)
	      (setf old-shadows (remove sym old-shadows))))))
      (when old-shadows
	(warn "%s also shadows the following symbols: %s"
	      name old-shadows)))

    ;; Use
    (unless (eq use :default)
      (let ((old-use-list (package-use-list package))
	    (new-use-list (mapcar #'package-or-lose use)))
	(use-package (cl-set-difference new-use-list old-use-list) package)
	(let ((laterize (cl-set-difference old-use-list new-use-list)))
	  (when laterize
	    (unuse-package laterize package)
	    (warn "%s previously used the following packages: %s"
		  name laterize)))))

    ;; Import and Intern.
    (dolist (sym-name interns)
      (intern sym-name package))
    (dolist (imports-from imports)
      (let ((other-package (package-or-lose (car imports-from))))
	(dolist (sym-name (cdr imports-from))
	  (import (list (find-or-make-symbol sym-name other-package))
		  package))))

    ;; Exports.
    (let ((old-exports nil)
	  (exports (mapcar (lambda (sym-name) (intern sym-name package)) exports)))
      (do-external-symbols (sym package)
	 (push sym old-exports))
      (export exports package)
      (let ((diff (cl-set-difference old-exports exports)))
	(when diff
	  (warn "%s also exports the following symbols: %s" name diff))))

    ;; Documentation
    (setf (package-doc-string package) doc-string)
    package))



(defmacro defpackage (package &rest options)
  "Defines a new package called PACKAGE.  Each of OPTIONS should be one of the
   following:
     (:NICKNAMES {package-name}*)
     (:SIZE <integer>)
     (:SHADOW {symbol-name}*)
     (:SHADOWING-IMPORT-FROM <package-name> {symbol-name}*)
     (:USE {package-name}*)
     (:IMPORT-FROM <package-name> {symbol-name}*)
     (:INTERN {symbol-name}*)
     (:EXPORT {symbol-name}*)
     (:DOCUMENTATION doc-string)
   All options except :SIZE and :DOCUMENTATION can be used multiple times."
  (let ((nicknames nil)
	(size nil)
	(shadows nil)
	(shadowing-imports nil)
	(use nil)
	(use-p nil)
	(imports nil)
	(interns nil)
	(exports nil)
	(doc nil))
    (dolist (option options)
      (unless (consp option)
	(error "Bogus DEFPACKAGE option: %s" option))
      (cl-case (car option)
	(:nicknames
	 (setf nicknames (pkg-stringify-names (cdr option) "package")))
	(:size
	 (cond (size
		(error "Can't specify :SIZE twice."))
	       ((and (consp (cdr option))
		     (cl-typep (cl-second option) 'natnum))
		(setf size (cl-second option)))
	       (t
		(error "Bogus :SIZE, must be a positive integer: %s"
                       (cl-second option)))))
	(:shadow
	 (let ((new (pkg-stringify-names (cdr option) "symbol")))
	   (setf shadows (append shadows new))))
	(:shadowing-import-from
	 (let ((package-name (pkg-stringify-name (cl-second option) "package"))
	       (names (pkg-stringify-names (cddr option) "symbol")))
	   (let ((assoc (cl-assoc package-name shadowing-imports
			          :test #'string=)))
	     (if assoc
		 (setf (cdr assoc) (append (cdr assoc) names))
	       (setf shadowing-imports
		     (cl-acons package-name names shadowing-imports))))))
	(:use
	 (let ((new (pkg-stringify-names (cdr option) "package")))
	   (setf use (cl-delete-duplicates (nconc use new) :test #'string=))
	   (setf use-p t)))
	(:import-from
	 (let ((package-name (pkg-stringify-name (cl-second option) "package"))
	       (names (pkg-stringify-names (cddr option) "symbol")))
	   (let ((assoc (cl-assoc package-name imports
			          :test #'string=)))
	     (if assoc
		 (setf (cdr assoc) (append (cdr assoc) names))
	       (setf imports (cl-acons package-name names imports))))))
	(:intern
	 (let ((new (pkg-stringify-names (cdr option) "symbol")))
	   (setf interns (append interns new))))
	(:export
	 (let ((new (pkg-stringify-names (cdr option) "symbol")))
	   (setf exports (append exports new))))
	(:documentation
	 (when doc
	   (error "Can't specify :DOCUMENTATION twice."))
	 (setf doc (cl-coerce (cl-second option) 'string)))
	(t
	 (error "Bogus DEFPACKAGE option: %s" option))))
    (pkg-check-disjoint `(:intern ,@interns) `(:export  ,@exports))
    (pkg-check-disjoint `(:intern ,@interns)
		        `(:import-from ,@(apply 'append (mapcar 'cl-rest imports)))
		        `(:shadow ,@shadows)
		        `(:shadowing-import-from
                          ,@(apply 'append (mapcar 'cl-rest shadowing-imports))))
    `(cl-eval-when (compile load eval)
       (%defpackage ,(pkg-stringify-name package "package") ',nicknames ',size
		    ',shadows ',shadowing-imports ',(if use-p use :default)
		    ',imports ',interns ',exports ',doc))))

;;; pkg.el ends here
