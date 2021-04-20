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

(ert-deftest project/quoted-directory ()
  "Check that `project-files' deals with quoted directory
names (Bug#47799)."
  (let ((directory (make-temp-file "project-tests-" :directory)))
    (unwind-protect
        (let ((project (cons 'transient (file-name-quote directory)))
              (file (expand-file-name "file" directory)))
          (make-empty-file file)
          (should (equal (project-files project)
                         (list (file-name-quote file)))))
      (delete-directory directory :recursive))))

;;; project-tests.el ends here
