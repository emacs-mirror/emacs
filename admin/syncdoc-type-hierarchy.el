;;;  syncdoc-type-hierarchy.el--- -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Free Software Foundation, Inc.

;; Author: Andrea Corallo <acorallo@gnu.org>
;; Keywords: documentation

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

;; This file is used to keep the type hierarchy representation present
;; in the elisp manual in sync with the current type hierarchy.  This
;; is specified in `cl--direct-supertypes-of-type' in cl-preloaded.el, so each
;; time `cl--direct-supertypes-of-type' is modified
;; `syncdoc-update-type-hierarchy' must be run before the
;; documentation is regenerated.

;; We do not call this directly from make docs in order not to add a
;; dependency on the tool "dot".

;;; Code:

(require 'cl-lib)
(require 'org-table)

(defconst syncdoc-lispref-dir
  (expand-file-name "../doc/lispref/"
                    (file-name-directory
                     (or (macroexp-file-name)
                         buffer-file-name))))

(defconst syncdoc-hierarchy
  (let ((ht (copy-hash-table cl--direct-supertypes-of-type)))
    ;; Include info about "representative" other structure types,
    ;; to illustrate how they fit.
    (mapc #'require '(kmacro eieio-base elisp-mode frameset transient))
    (let ((extra-types '(advice kmacro cl-structure-object cl-structure-class
                         eieio-default-superclass eieio-named transient-infix
                         xref-elisp-location frameset-register))
          (seen ()))
      (while extra-types
        (let* ((type (pop extra-types))
               (class (get type 'cl--class))
               (parents (cl--class-parents class)))
          (unless (member type seen)
            (push type seen)
            (push (type-of class) extra-types)
            (puthash type (cond
                           (parents
                            (let ((ps (mapcar #'cl--class-name parents)))
                              (setq extra-types (append ps extra-types))
                              ps))
                           ;; EIEIO's parents don't mention the default.
                           ((and (eq (type-of class) 'eieio--class)
                                 (not (eq type 'eieio-default-superclass)))
                            '(eieio-default-superclass))
                           ;; OClosures can still be lists :-(
                           ((eq 'oclosure type) '(function))
                           (t '(atom)))
                     ht)))))
    ht))

(defun syncdoc-insert-dot-content (rankdir)
  (maphash (lambda (child parents)
              (cl-loop for parent in parents
                       do (insert " \"" (symbol-name child) "\" -> \""
                                  (symbol-name parent) "\";\n")))
           syncdoc-hierarchy)
  (sort-lines nil (point-min) (point-max))

  (goto-char (point-min))
  (insert "digraph {\n rankdir=\"" rankdir "\";\n")
  (goto-char (point-max))
  (insert "}\n"))

(defun syncdoc-make-type-table (file)
  (with-temp-file file
    (insert "|Type| Derived Types|\n|-\n")
    (let ((subtypes ()))
      ;; First collect info from the "builtin" types.
      (maphash (lambda (type parents)
                 (dolist (parent parents)
                   (push type (alist-get parent subtypes))))
               syncdoc-hierarchy)
      (cl-loop for (type . children) in (reverse subtypes)
               do (insert "|" (symbol-name type) " |")
               do (cl-loop with x = 0
                           for child in (reverse children)
                           for child-len = (length (symbol-name child))
                           when (> (+ x child-len 2) 60)
                           do (progn
                                (insert "|\n||")
                                (setq x 0))
                           do (insert (symbol-name child) " ")
                           do (cl-incf x (1+ child-len)) )
               do (insert "\n")))
    (org-table-align)))

(defun syncdoc-update-type-hierarchy ()
  "Update the type hierarchy representation used by the elisp manual."
  (interactive)
  (with-temp-buffer
    (syncdoc-insert-dot-content "LR")
    (with-demoted-errors "%S"           ;In case "dot" is not found!
      (call-process-region nil nil "dot" t (current-buffer) nil "-Tjpg" "-o"
                           (expand-file-name "type_hierarchy.jpg"
                                             syncdoc-lispref-dir))))
  (syncdoc-make-type-table (expand-file-name "type_hierarchy.txt"
                                             syncdoc-lispref-dir)))

;;; syncdoc-type-hierarchy.el ends here
