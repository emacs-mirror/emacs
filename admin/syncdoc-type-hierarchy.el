;;;  syncdoc-type-hierarchy.el--- -*- lexical-binding: t -*-

;; Copyright (C) 2023-2026 Free Software Foundation, Inc.

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
(require 'org)

(defconst syncdoc-file (or (macroexp-file-name) buffer-file-name))

(defconst syncdoc-emacs-repo-dir
  (expand-file-name "../" (file-name-directory syncdoc-file)))

(defconst syncdoc-lispref-dir
  (expand-file-name "doc/lispref/" syncdoc-emacs-repo-dir))

(defconst syncdoc-all-types
  (let (res)
    (mapatoms (lambda (type)
                (when (cl-find-class type)
                  (push type res)))
              obarray)
    (nreverse
     (merge-ordered-lists
      (sort
       (mapcar (lambda (type) (cl--class-allparents (cl-find-class type)))
               res)
       (lambda (ts1 ts2) (> (length ts1) (length ts2)))))))
  "List of all types.")

(defconst syncdoc-hierarchy
  (progn
    ;; Require it here so we don't load it before `syncdoc-all-types' is
    ;; computed.
    (cl-loop
     with h = (make-hash-table :test #'eq)
     for type in syncdoc-all-types
     do (puthash type (mapcar #'cl--class-name
                       (cl--class-parents (cl-find-class type)))
         h)
     finally return h)))

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
      (sort subtypes
            (lambda (x1 x2)
              (< (length (memq (car x2) syncdoc-all-types))
                 (length (memq (car x1) syncdoc-all-types)))))
      (cl-loop for (type . children) in (reverse subtypes)
               do (insert "|" (symbol-name type) " |")
               do (cl-loop with x = 0
                           for child in children
                           for child-len = (length (symbol-name child))
                           when (> (+ x child-len 2) 60)
                           do (progn
                                (insert "|\n||")
                                (setq x 0))
                           do (insert (symbol-name child) " ")
                           do (incf x (1+ child-len)) )
               do (insert "\n")))
    (require 'org-table)
    (declare-function org-table-align "org")
    (org-table-align)))

(defun syncdoc-update-type-hierarchy0 ()
  "Update the type hierarchy representation used by the elisp manual."
  (with-temp-buffer
    (syncdoc-insert-dot-content "LR")
    (with-demoted-errors "%S"           ;In case "dot" is not found!
      (call-process-region nil nil "dot" t (current-buffer) nil "-Tjpg" "-o"
                           (expand-file-name "elisp_type_hierarchy.jpg"
                                             syncdoc-lispref-dir))))
  (syncdoc-make-type-table (expand-file-name "elisp_type_hierarchy.txt"
                                             syncdoc-lispref-dir)))

(defun syncdoc-update-type-hierarchy ()
  "Update the type hierarchy representation used by the elisp manual."
  (interactive)
  (call-process (expand-file-name "src/emacs" syncdoc-emacs-repo-dir)
		nil t t "-Q" "--batch" "-l" syncdoc-file
                "-f" "syncdoc-update-type-hierarchy0"))

;;; syncdoc-type-hierarchy.el ends here
