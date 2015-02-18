;;; cl-preloaded.el --- Preloaded part of the CL library  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Free Software Foundation, Inc

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>

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

;; The expectation is that structs defined with cl-defstruct do not
;; need cl-lib at run-time, but we'd like to hide the details of the
;; cl-struct metadata behind the cl-struct-define function, so we put
;; it in this pre-loaded file.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defun cl-struct-define (name docstring parent type named slots children-sym
                              tag print-auto)
  (cl-assert (or type (equal '(cl-tag-slot) (car slots))))
  (cl-assert (or type (not named)))
  (if (boundp children-sym)
      (add-to-list children-sym tag)
    (set children-sym (list tag)))
  (let* ((parent-class parent))
    (while parent-class
      (add-to-list (intern (format "cl-struct-%s-tags" parent-class)) tag)
      (setq parent-class (get parent-class 'cl-struct-include))))
  ;; If the cl-generic support, we need to be able to check
  ;; if a vector is a cl-struct object, without knowing its particular type.
  ;; So we use the (otherwise) unused function slots of the tag symbol
  ;; to put a special witness value, to make the check easy and reliable.
  (unless named (fset tag :quick-object-witness-check))
  (put name 'cl-struct-slots slots)
  (put name 'cl-struct-type (list type named))
  (if parent (put name 'cl-struct-include parent))
  (if print-auto (put name 'cl-struct-print print-auto))
  (if docstring (put name 'structure-documentation docstring)))

;; The `assert' macro from the cl package signals
;; `cl-assertion-failed' at runtime so always define it.
(define-error 'cl-assertion-failed (purecopy "Assertion failed"))

(defun cl--assertion-failed (form &optional string sargs args)
  (if debug-on-error
      (debug `(cl-assertion-failed ,form ,string ,@sargs))
    (if string
        (apply #'error string (append sargs args))
      (signal 'cl-assertion-failed `(,form ,@sargs)))))

;; Make sure functions defined with cl-defsubst can be inlined even in
;; packages which do not require CL.  We don't put an autoload cookie
;; directly on that function, since those cookies only go to cl-loaddefs.
(autoload 'cl--defsubst-expand "cl-macs")
;; Autoload, so autoload.el and font-lock can use it even when CL
;; is not loaded.
(put 'cl-defun    'doc-string-elt 3)
(put 'cl-defmacro 'doc-string-elt 3)
(put 'cl-defsubst 'doc-string-elt 3)
(put 'cl-defstruct 'doc-string-elt 2)

(provide 'cl-preloaded)
;;; cl-preloaded.el ends here
