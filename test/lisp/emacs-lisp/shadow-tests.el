;;; shadow-tests.el --- Test suite for shadow.  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

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

;;; Code:

(require 'ert)
(require 'shadow)
(eval-when-compile (require 'cl-lib))

(defconst shadow-tests-data-directory
  (expand-file-name "lisp/emacs-lisp/shadow-resources"
                    (or (getenv "EMACS_TEST_DIRECTORY")
                        (expand-file-name "../../.."
                                          (or load-file-name
                                              buffer-file-name))))
  "Directory for shadow test files.")

(ert-deftest shadow-case-insensitive ()
  "Test shadowing for case insensitive filenames."
  ;; Override `file-name-case-insensitive-p' so we test the same thing
  ;; regardless of what file system we're running on.
  (cl-letf (((symbol-function 'file-name-case-insensitive-p) (lambda (_f) t)))
    (should (equal (list (expand-file-name "p1/foo" shadow-tests-data-directory)
                         (expand-file-name "p2/FOO" shadow-tests-data-directory))
                   (load-path-shadows-find
                    (list (expand-file-name "p1/" shadow-tests-data-directory)
                          (expand-file-name "p2/" shadow-tests-data-directory))))))
  (cl-letf (((symbol-function 'file-name-case-insensitive-p) (lambda (_f) nil)))
    (should-not (load-path-shadows-find
                 (list (expand-file-name "p1/" shadow-tests-data-directory)
                       (expand-file-name "p2/" shadow-tests-data-directory))))))

;;; shadow-tests.el ends here.
