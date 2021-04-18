;;; project-tests.el --- tests for project.el -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Keywords:

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

;; Unit tests for progmodes/project.el.

;;; Code:

(require 'project)

(require 'ert)
(require 'grep)
(require 'xref)

(ert-deftest project/quoted-directory ()
  "Check that `project-files' and `project-find-regexp' deal with
quoted directory names (Bug#47799)."
  (skip-unless (executable-find find-program))
  (skip-unless (executable-find "xargs"))
  (skip-unless (executable-find "grep"))
  (let ((directory (make-temp-file "project-tests-" :directory)))
    (unwind-protect
        (let ((default-directory directory)
              (project-current-inhibit-prompt t)
              (project-find-functions nil)
              (project-list-file
               (expand-file-name "projects" directory))
              (project (cons 'transient (file-name-quote directory)))
              (file (expand-file-name "file" directory)))
          (add-hook 'project-find-functions (lambda (_dir) project))
          (should (eq (project-current) project))
          (write-region "contents" nil file nil nil nil 'excl)
          (should (equal (project-files project)
                         (list (file-name-quote file))))
          (let* ((references nil)
                 (xref-search-program 'grep)
                 (xref-show-xrefs-function
                  (lambda (fetcher _display)
                    (push (funcall fetcher) references))))
            (project-find-regexp "tent")
            (pcase references
              (`((,item))
               (should
                ;; FIXME: Shouldn't `xref-match-item' be a subclass of
                ;; `xref-item'?
                (cl-typep item '(or xref-item xref-match-item)))
               (should
                (file-equal-p
                 (xref-location-group (xref-item-location item))
                 file)))
              (otherwise
               (ert-fail (format-message "Unexpected references: %S"
                                         otherwise))))))
      (delete-directory directory :recursive))))

;;; project-tests.el ends here
