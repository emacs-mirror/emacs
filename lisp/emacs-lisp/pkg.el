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

;; The implementation strives to do as much as possible in Lisp, not
;; C.  C functions with names like 'package-%...' are defined which
;; allow low-level access to the guts of Lisp_Package objects.
;; Several variables are exposed from C that allow manipulating
;; internal state.

;; All that is dangerous :-).

;;; Code:

(require 'cl-lib)
(require 'cl-macs)
(require 'gv)

;;; Define setters for internal package details.
(gv-define-simple-setter package-%name package-%set-name)
(gv-define-simple-setter package-%nicknames package-%set-nicknames)
(gv-define-simple-setter package-%use-list package-%set-use-list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (symbol (cl-symbol-name name))
    (base-char (char-to-string name))
    (t (error "Bogus %s name: %s" kind name))))

(defun pkg-stringify-names (names kind)
  (mapcar (lambda (name) (pkg-stringify-name name kind)) names))

(defun pkg-package-namify (n)
  (pkg-stringify-name n "package"))

(defun pkg-find-package (name)
  (gethash name *package-registry* nil))

(defun pkg--symbol-listify (thing)
  (cond ((listp thing)
	 (dolist (s thing)
	   (unless (symbolp s)
             (error "%s is not a symbol") s))
	 thing)
	((symbolp thing)
         (list thing))
	(t
	 (error "%s is neither a symbol nor a list of symbols" thing))))

(defun pkg-find-or-make-package (name)
  (if (packagep name)
      (progn
	(unless (package-%name name)
	  (error "Can't do anything with deleted package: %s" name))
	name)
    (let* ((name (pkg-stringify-name name "package name")))
      (or (pkg-find-package name)
	  (make-package name)))))

(defun pkg-packages-from-names (names)
  (mapcar (lambda (name) (pkg-find-or-make-package name))
          names))

(defun pkg-package-or-lose (name)
  (if (packagep name)
      name
    (let ((pkg-name (pkg-stringify-name name "package")))
      (or (find-package pkg-name)
          (error "No package %s found" name)))))

(defun pkg--check-name-conflicts (package)
  (cl-flet ((check (name)
              (when (gethash name *package-registry*)
                (error "%s conflicts with existing package" name))))
    (check (package-%name package))
    (dolist (n (package-%nicknames package)) (check n))))

(defun pkg--add-to-registry (package)
  (pkg--check-name-conflicts package)
  (puthash (package-%name package) package *package-registry*)
  (mapc (lambda (name) (puthash name package *package-registry*))
        (package-%nicknames package)))

(defun pkg--remove-from-registry (package)
  "Remove PACKAGE from the package registry."
  (remhash (package-%name package) *package-registry*)
  (mapc (lambda (name) (remhash name *package-registry*))
        (package-%nicknames package)))

(defun pkg--package-or-default (package)
  (cond ((packagep package) package)
        ((null package) *package*)
        (t (pkg-package-or-lose package))))

(defun pkg--symbol-listify (thing)
  (cond ((listp thing)
	 (dolist (s thing)
	   (unless (symbolp s)
             (error "%s is not a symbol" s)))
	 thing)
	((symbolp thing)
         (list thing))
	(t
	 (error "%s is neither a symbol nor a list of symbols" thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        Basic stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(cl-defun make-package (name &key nicknames use (size 10))
  "tbd"
  (cl-check-type size natnum)
  (let* ((name (pkg-stringify-name name "package name"))
         (nicknames (pkg-stringify-names nicknames "package nickname"))
         (use (pkg-packages-from-names use))
         (package (make-%package name size)))
    (setf (package-%nicknames package) nicknames
          (package-%use-list package) use)
    package))

;;;###autoload
(defun package-name (package)
  (package-%name (pkg-package-or-lose package)))

;;;###autoload
(defun package-nicknames (package)
  (package-%nicknames (pkg-package-or-lose package)))

;;;###autoload
(defun package-shadowing-symbols (package)
  (package-%shadowing-symbols (pkg-package-or-lose package)))

;;;###autoload
(defun package-use-list (package)
  (package-%use-list (pkg-package-or-lose package)))

;;;###autoload
(defun package-used-by-list (package)
  (let ((package (pkg-package-or-lose package))
        ((used-by ())))
    (dolist (p (list-all-packages))
      (when (memq package (package-%use-list p))
        (cl-pushnew p used-by)))
    used-by))

;;;###autoload
(defun list-all-packages ()
  (let ((all ()))
    (maphash (lambda (_name package)
               (cl-pushnew package all))
             *package-registry*)
    all))

;;;###autoload
(defun find-package (package)
  (if (packagep package)
      package
    (let ((name (pkg-stringify-name package "package name")))
      (gethash name *package-registry*))))

;;;###autoload
(defun delete-package (package)
  (if (and (packagep package)
           (null (package-name package)))
      nil
    (let ((package (pkg-package-or-lose package)))
    (when (or (eq package *emacs-package*)
              (eq package *keyword-package*))
      (error "Cannot delete standard package"))
    (pkg--remove-from-registry package)
    (setf (package-%name package) nil)
    t)))

;;;###autoload
(defun rename-package (package new-name &optional new-nicknames)
  (let ((package (pkg-package-or-lose package)))
    (unless (package-%name package)
      ;; That's what CLHS says, and SBCL does...
      (error "Cannot rename deleted package"))
    (pkg--remove-from-registry package)
    (setf (package-%nicknames package) new-nicknames)
    (setf (package-%name package) new-name)
    (pkg--add-to-registry package)))


;;; Here...

;;;###autoload
(defun export (symbols &optional package)
  "tbd"
  (let ((symbols (pkg--symbol-listify symbols))
        (package (pkg--package-or-default package))
        (syms ()))

    ;; Ignore any symbols that are already external.
    (dolist (sym symbols)
      (cl-multiple-value-bind (_s status)
	  (find-symbol (cl-symbol-name sym) package)
        (unless (or (eq :external status)
                    (memq (sym syms)))
          (push sym syms))))

    ;; Find symbols and packages with conflicts.
    (let ((used-by (package-used-by-list package))
	  (cpackages ())
	  (cset ()))
      (dolist (sym syms)
	(let ((name (cl-symbol-name sym)))
	  (dolist (p used-by)
	    (cl-multiple-value-bind (s w)
                (find-symbol name p)
	      (when (and w (not (eq s sym))
			 (not (member s (package-%shadowing-symbols p))))
		(cl-pushnew sym cset)
		(cl-pushnew p cpackages))))))

      (when cset
	(error "Exporting these symbols from the %s package: %s
 results in name conflicts with these packages: %s"
	       (package-name package)
               cset
	       (mapcar #'package-name cpackages))))

    ;; Check that all symbols are accessible.
    (let ((missing ())
	  (imports ()))
      (dolist (sym syms)
	(cl-multiple-value-bind (s w)
            (find-symbol (cl-symbol-name sym) package)
	  (cond ((not (and w (eq s sym)))
                 (push sym missing))
		((eq w :inherited)
                 (push sym imports)))))
      (when missing
	(error "These symbols are not accessible in the %s package: %s"
               (package-%name package)
               missing)))

    ;; Import
    (import imports package)

    ;; And now, three pages later, we export the suckers.
    (dolist (sym syms)
      (package-%set-status sym package :external))
    t))


;;;###autoload
(defun unexport (_symbols &optional package)
  (setq package (pkg--package-or-default package))
  (error "not yet implemented"))

;;;###autoload
(defun import (symbols &optional package)
  (let ((package (pkg--package-or-default package))
        (symbols (pkg--symbol-listify symbols)))
    (list package symbols)
    (error "not yet implemented")))

;;;###autoload
(defun shadow (_symbols &optional package)
  (setq package (pkg--package-or-default package))
  (error "not yet implemented"))

;;;###autoload
(defun shadowing-import (_symbols &optional package)
  (setq package (pkg--package-or-default package))
  (error "not yet implemented"))

;;;###autoload
(defun use-package (_use package)
  (setq package (pkg--package-or-default package))
  (cl-pushnew (package-%use-list package) package))

;;;###autoload
(defun unuse-package (_unuse package)
  (setq package (pkg--package-or-default package))
  (setf (package-%use-list package)
        (delq package (package-%use-list package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            defpackage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun pkg--enter-new-nicknames (package nicknames)
;;   (cl-check-type nicknames list)
;;   (dolist (n nicknames)
;;     (let* ((n (pkg-package-namify n))
;; 	   (found (pkg-name-to-package n)))
;;       (cond ((not found)
;; 	     (setf (gethash n *package-registry*) package)
;; 	     (push n (package-%nicknames package)))
;; 	    ((eq found package))
;; 	    ((string= (package-name found) n)
;; 	     (error "%s is a package name, so it cannot be a nickname for %s."
;; 	            n (package-name package)))
;; 	    (t
;; 	     (error "%s is already a nickname for %s"
;;                     n (package-name found)))))))

;; (defun pkg-defpackage (name nicknames size shadows shadowing-imports
;; 			    use imports interns exports doc-string)
;;   (let ((package (find-package name)))
;;     (unless package
;;       (setq package (make-package name :use nil :size (or size 10))))
;;     (unless (string= (package-name package) name)
;;       (error "%s is a nickname for the package %s"
;;              name (package-name package)))

;;     Nicknames
;;     (pkg--enter-new-nicknames package nicknames)

;;     Shadows and Shadowing-imports.
;;     (let ((old-shadows (package-%shadowing-symbols package)))
;;       (shadow shadows package)
;;       (dolist (sym-name shadows)
;; 	(setf old-shadows (remove (find-symbol sym-name package) old-shadows)))
;;       (dolist (simports-from shadowing-imports)
;; 	(let ((other-package (package-or-lose (car simports-from))))
;; 	  (dolist (sym-name (cdr simports-from))
;; 	    (let ((sym (find-or-make-symbol sym-name other-package)))
;; 	      (shadowing-import sym package)
;; 	      (setf old-shadows (remove sym old-shadows))))))
;;       (when old-shadows
;; 	(warn "%s also shadows the following symbols: %s"
;; 	      name old-shadows)))

;;     Use
;;     (let ((old-use-list (package-use-list package))
;; 	  (new-use-list (mapcar #'package-or-lose use)))
;;       (use-package (cl-set-difference new-use-list old-use-list) package)
;;       (let ((laterize (cl-set-difference old-use-list new-use-list)))
;; 	(when laterize
;; 	  (unuse-package laterize package)
;; 	    (warn "%s previously used the following packages: %s"
;; 		  name laterize))))

;;     Import and Intern.
;;     (dolist (sym-name interns)
;;       (intern sym-name package))
;;     (dolist (imports-from imports)
;;       (let ((other-package (package-or-lose (car imports-from))))
;; 	(dolist (sym-name (cdr imports-from))
;; 	  (import (list (find-or-make-symbol sym-name other-package))
;; 		  package))))

;;     Exports.
;;     (let ((old-exports nil)
;; 	  (exports (mapcar (lambda (sym-name) (intern sym-name package)) exports)))
;;       (do-external-symbols (sym package)
;; 	 (push sym old-exports))
;;       (export exports package)
;;       (let ((diff (cl-set-difference old-exports exports)))
;; 	(when diff
;; 	  (warn "%s also exports the following symbols: %s" name diff))))

;;     Documentation
;;     (setf (package-doc-string package) doc-string)
;;     package))



;; (defmacro defpackage (package &rest options)
;;   "Defines a new package called PACKAGE.  Each of OPTIONS should be one of the
;;    following:
;;      (:NICKNAMES {package-name}*)
;;      (:SIZE <integer>)
;;      (:SHADOW {symbol-name}*)
;;      (:SHADOWING-IMPORT-FROM <package-name> {symbol-name}*)
;;      (:USE {package-name}*)
;;      (:IMPORT-FROM <package-name> {symbol-name}*)
;;      (:INTERN {symbol-name}*)
;;      (:EXPORT {symbol-name}*)
;;      (:DOCUMENTATION doc-string)
;;    All options except :SIZE and :DOCUMENTATION can be used multiple times."
;;   (let ((nicknames nil)
;; 	(size nil)
;; 	(shadows nil)
;; 	(shadowing-imports nil)
;; 	(use nil)
;; 	(use-p nil)
;; 	(imports nil)
;; 	(interns nil)
;; 	(exports nil)
;; 	(doc nil))
;;     (dolist (option options)
;;       (unless (consp option)
;; 	(error "Bogus DEFPACKAGE option: %s" option))
;;       (cl-case (car option)
;; 	(:nicknames
;; 	 (setf nicknames (pkg-stringify-names (cdr option) "package")))
;; 	(:size
;; 	 (cond (size
;; 		(error "Can't specify :SIZE twice."))
;; 	       ((and (consp (cdr option))
;; 		     (cl-typep (cl-second option) 'natnum))
;; 		(setf size (cl-second option)))
;; 	       (t
;; 		(error "Bogus :SIZE, must be a positive integer: %s"
;;                        (cl-second option)))))
;; 	(:shadow
;; 	 (let ((new (pkg-stringify-names (cdr option) "symbol")))
;; 	   (setf shadows (append shadows new))))
;; 	(:shadowing-import-from
;; 	 (let ((package-name (pkg-stringify-name (cl-second option) "package"))
;; 	       (names (pkg-stringify-names (cddr option) "symbol")))
;; 	   (let ((assoc (cl-assoc package-name shadowing-imports
;; 			          :test #'string=)))
;; 	     (if assoc
;; 		 (setf (cdr assoc) (append (cdr assoc) names))
;; 	       (setf shadowing-imports
;; 		     (cl-acons package-name names shadowing-imports))))))
;; 	(:use
;; 	 (let ((new (pkg-stringify-names (cdr option) "package")))
;; 	   (setf use (cl-delete-duplicates (nconc use new) :test #'string=))
;; 	   (setf use-p t)))
;; 	(:import-from
;; 	 (let ((package-name (pkg-stringify-name (cl-second option) "package"))
;; 	       (names (pkg-stringify-names (cddr option) "symbol")))
;; 	   (let ((assoc (cl-assoc package-name imports
;; 			          :test #'string=)))
;; 	     (if assoc
;; 		 (setf (cdr assoc) (append (cdr assoc) names))
;; 	       (setf imports (cl-acons package-name names imports))))))
;; 	(:intern
;; 	 (let ((new (pkg-stringify-names (cdr option) "symbol")))
;; 	   (setf interns (append interns new))))
;; 	(:export
;; 	 (let ((new (pkg-stringify-names (cdr option) "symbol")))
;; 	   (setf exports (append exports new))))
;; 	(:documentation
;; 	 (when doc
;; 	   (error "Can't specify :DOCUMENTATION twice."))
;; 	 (setf doc (cl-coerce (cl-second option) 'string)))
;; 	(t
;; 	 (error "Bogus DEFPACKAGE option: %s" option))))
;;     (pkg-check-disjoint `(:intern ,@interns) `(:export  ,@exports))
;;     (pkg-check-disjoint `(:intern ,@interns)
;; 		        `(:import-from ,@(apply 'append (mapcar 'cl-rest imports)))
;; 		        `(:shadow ,@shadows)
;; 		        `(:shadowing-import-from
;;                           ,@(apply 'append (mapcar 'cl-rest shadowing-imports))))
;;     `(cl-eval-when (compile load eval)
;;        (pkg-defpackage ,(pkg-stringify-name package "package") ',nicknames ',size
;; 		       ',shadows ',shadowing-imports ',(if use-p use :default)
;; 		       ',imports ',interns ',exports ',doc))))

;;; pkg.el ends here
