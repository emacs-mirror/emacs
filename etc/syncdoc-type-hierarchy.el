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
;; is specified in `cl--type-hierarchy' in cl-preloaded.el, so each
;; time `cl--type-hierarchy' is modified
;; `syncdoc-update-type-hierarchy' must be run before the
;; documentation is regenerated.

;; We do not call this directly from make docs in order not to add a
;; dependency on the tool "dot".

;;; Code:

(require 'cl-lib)
(require 'org-table)

(defconst syncdoc-lispref-dir (concat (file-name-directory
                                       (or load-file-name
                                           buffer-file-name))
                                      "../doc/lispref/"))

(defun syncdoc-insert-dot-content (rankdir)
  (maphash (lambda (child parents)
              (cl-loop for parent in parents
                       do (insert " \"" (symbol-name child) "\" -> \""
                                  (symbol-name parent) "\";\n")))
           cl--direct-supertypes-of-type)
  (sort-lines nil (point-min) (point-max))

  (goto-char (point-min))
  (insert "digraph {\n rankdir=\"" rankdir "\";\n")
  (goto-char (point-max))
  (insert "}\n"))

(defun syncdoc-make-type-table (file)
  (with-temp-file file
    (insert "|Type| Derived Types|\n|-\n")
    (cl-loop for (type . children) in cl--type-hierarchy
             do (insert "|" (symbol-name type) " |")
             do (cl-loop with x = 0
                         for child in children
                         for child-len = (length (symbol-name child))
                         when (> (+ x child-len 2) 60)
                         do (progn
                              (insert "|\n||")
                              (setq x 0))
                         do (insert (symbol-name child) " ")
                         do (cl-incf x (1+ child-len)) )
             do (insert "\n"))
    (org-table-align)))

(defun syncdoc-update-type-hierarchy ()
  "Update the type hierarchy representation used by the elisp manual."
  (interactive)
  (with-temp-buffer
    (syncdoc-insert-dot-content "LR")
    (call-process-region nil nil "dot" t (current-buffer) nil "-Tjpg" "-o"
                         (expand-file-name "type_hierarchy.jpg"
                                           syncdoc-lispref-dir)))
  (syncdoc-make-type-table (expand-file-name "type_hierarchy.txt"
                                             syncdoc-lispref-dir)))

;;; syncdoc-type-hierarchy.el ends here
